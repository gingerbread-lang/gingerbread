use super::{model, proto};
use lsp_types::notification::{Exit, Initialized, Notification};
use lsp_types::request::{Initialize, Request};
use lsp_types::{InitializeParams, InitializeResult};
use std::io;
use thiserror::Error;

pub(crate) struct Connection<'s> {
    reader: io::StdinLock<'s>,
    writer: io::StdoutLock<'s>,
    buf: proto::ScratchReadBuf,
    next_id: model::ReqId,
}

pub(crate) struct ConnectionStorage {
    stdin: io::Stdin,
    stdout: io::Stdout,
}

#[must_use]
pub(crate) enum ReqHandler<'c, 's> {
    Unhandled { req: model::Req, connection: &'c mut Connection<'s> },
    Handled,
}

pub(crate) enum NotHandler {
    Unhandled { not: model::Not },
    Handled,
}

#[derive(Debug, Error)]
pub(crate) enum InitializeError {
    #[error("an error occurred while reading a message during initialization")]
    Read(#[from] proto::ReadMsgError),

    #[error("an error occurred while writing a message during initialization")]
    Write(#[from] proto::WriteMsgError),

    #[error("malformed JSON was read during initialization")]
    MalformedJson(#[from] serde_json::Error),

    #[error("an `initialized` notification was not sent after initialization")]
    NoInitializedNot,
}

impl<'s> Connection<'s> {
    pub(crate) fn new(
        storage: &'s ConnectionStorage,
        f: impl FnOnce(InitializeParams) -> InitializeResult,
    ) -> Result<Option<Self>, InitializeError> {
        let mut connection = Self {
            reader: storage.stdin.lock(),
            writer: storage.stdout.lock(),
            buf: proto::ScratchReadBuf::default(),
            next_id: model::ReqId::Integer(0),
        };

        connection.initialize(f)?;

        Ok(Some(connection))
    }

    pub(crate) fn make_request<R>(&mut self, params: R::Params) -> Result<(), proto::WriteMsgError>
    where
        R: Request,
    {
        // types from lsp_types can always be serialized as JSON
        let params = serde_json::to_value(params).unwrap();

        self.write_msg(&model::Msg::Req(model::Req {
            id: self.next_id.clone(),
            method: R::METHOD.to_string(),
            params,
        }))
    }

    pub(crate) fn read_msg(&mut self) -> Result<model::Msg, proto::ReadMsgError> {
        let msg = proto::read_msg(&mut self.reader, &mut self.buf)?;
        self.bump_to_next_id(&msg);

        Ok(msg)
    }

    pub(crate) fn req_handler<'a>(&'a mut self, req: model::Req) -> ReqHandler<'a, 's> {
        ReqHandler::Unhandled { req, connection: self }
    }

    pub(crate) fn not_handler(&mut self, not: model::Not) -> NotHandler {
        NotHandler::Unhandled { not }
    }

    fn initialize(
        &mut self,
        f: impl FnOnce(InitializeParams) -> InitializeResult,
    ) -> Result<(), InitializeError> {
        let (params, id) = match self.wait_for_initialize_req()? {
            Some((params, id)) => (params, id),
            None => return Ok(()),
        };

        // types from lsp_types can always be serialized as JSON
        let result = serde_json::to_value(f(params)).unwrap();

        self.write_msg(&model::Msg::Res(model::Res { id, result, error: None }))?;

        match self.read_msg()? {
            model::Msg::Not(not) if not.method == Initialized::METHOD => Ok(()),
            _ => Err(InitializeError::NoInitializedNot),
        }
    }

    fn wait_for_initialize_req(
        &mut self,
    ) -> Result<Option<(InitializeParams, model::ReqId)>, InitializeError> {
        loop {
            let request = match self.read_msg()? {
                model::Msg::Req(req) => req,
                model::Msg::Res(_) => continue,
                model::Msg::Not(not) if not.method == Exit::METHOD => return Ok(None),
                model::Msg::Not(_) => continue,
            };

            if request.method == Initialize::METHOD {
                let params = serde_json::from_value(request.params)?;
                return Ok(Some((params, request.id)));
            }

            self.write_msg(&model::Msg::Res(model::Res {
                id: request.id,
                result: serde_json::Value::Null,
                error: Some(model::ResError {
                    code: model::error_codes::SERVER_NOT_INITIALIZED,
                    message: "Initialize request has not been sent yet".to_string(),
                    data: None,
                }),
            }))?
        }
    }

    fn write_msg(&mut self, msg: &model::Msg) -> Result<(), proto::WriteMsgError> {
        self.bump_to_next_id(msg);
        proto::write_msg(&mut self.writer, msg)
    }

    fn bump_to_next_id(&mut self, msg: &model::Msg) {
        match msg {
            model::Msg::Req(req) => match req.id {
                model::ReqId::Integer(n) => self.next_id = model::ReqId::Integer(n + 1),
                model::ReqId::String(_) => {}
            },
            model::Msg::Res(_) | model::Msg::Not(_) => {}
        }
    }
}

#[derive(Debug, Error)]
pub(crate) enum HandleMsgError {
    #[error("an error occurred while writing the response to a message")]
    Write(#[from] proto::WriteMsgError),

    #[error("the JSON content of a message was malformed")]
    MalformedJson(#[from] serde_json::Error),
}

impl ReqHandler<'_, '_> {
    pub(crate) fn on<R, F>(self, f: F) -> Result<Self, HandleMsgError>
    where
        R: Request,
        F: FnOnce(R::Params) -> Result<R::Result, proto::WriteMsgError>,
    {
        match self {
            Self::Unhandled { req, connection } if req.method == R::METHOD => {
                let params = serde_json::from_value(req.params)?;
                let result = f(params)?;

                // types from lsp_types can always be serialized as JSON
                let result = serde_json::to_value(result).unwrap();

                connection.write_msg(&model::Msg::Res(model::Res {
                    id: req.id,
                    result,
                    error: None,
                }))?;

                Ok(Self::Handled)
            }

            Self::Unhandled { .. } | Self::Handled { .. } => Ok(self),
        }
    }

    pub(crate) fn finish(self) -> Result<(), proto::WriteMsgError> {
        match self {
            Self::Unhandled { req, connection } => {
                connection.write_msg(&model::Msg::Res(model::Res {
                    id: req.id,
                    result: serde_json::Value::Null,
                    error: Some(model::ResError {
                        code: model::error_codes::METHOD_NOT_FOUND,
                        message: "unknown request".to_string(),
                        data: None,
                    }),
                }))
            }

            Self::Handled => Ok(()),
        }
    }
}

impl NotHandler {
    pub(crate) fn on<N, F>(self, f: F) -> Result<Self, HandleMsgError>
    where
        N: Notification,
        F: FnOnce(N::Params) -> Result<(), proto::WriteMsgError>,
    {
        match self {
            Self::Unhandled { not } if not.method == N::METHOD => {
                f(serde_json::from_value(not.params)?)?;
                Ok(Self::Handled)
            }
            Self::Unhandled { .. } | Self::Handled { .. } => Ok(self),
        }
    }
}

impl ConnectionStorage {
    pub(crate) fn new() -> Self {
        Self { stdin: io::stdin(), stdout: io::stdout() }
    }
}
