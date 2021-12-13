use super::{model, proto};
use lsp_types::notification::{Exit, Initialized, Notification};
use lsp_types::request::{Initialize, Request};
use lsp_types::{InitializeParams, InitializeResult};
use std::{io, mem};
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

pub(crate) struct ReqHandler {
    req: Option<model::Req>,
}

pub(crate) struct NotHandler {
    not: Option<model::Not>,
}

#[derive(Debug, Error)]
pub(crate) enum InitializeError {
    #[error("an error occurred while reading a message during initialization")]
    Read(#[from] proto::ReadMsgError),

    #[error("an error occurred while writing a message during initialization")]
    Write(#[from] proto::WriteMsgError),

    #[error("an error occurred while serializing or deserializing during initialization")]
    Serde(#[from] serde_json::Error),

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

    pub(crate) fn write_msg(&mut self, msg: &model::Msg) -> Result<(), proto::WriteMsgError> {
        self.bump_to_next_id(msg);
        proto::write_msg(&mut self.writer, msg)
    }

    pub(crate) fn read_msg(&mut self) -> Result<model::Msg, proto::ReadMsgError> {
        let msg = proto::read_msg(&mut self.reader, &mut self.buf)?;
        self.bump_to_next_id(&msg);

        Ok(msg)
    }

    pub(crate) fn req_handler(&mut self, req: model::Req) -> ReqHandler {
        ReqHandler { req: Some(req) }
    }

    pub(crate) fn not_handler(&mut self, not: model::Not) -> NotHandler {
        NotHandler { not: Some(not) }
    }

    pub(crate) fn next_id(&self) -> model::ReqId {
        self.next_id.clone()
    }

    fn initialize(
        &mut self,
        f: impl FnOnce(InitializeParams) -> InitializeResult,
    ) -> Result<(), InitializeError> {
        let (params, id) = match self.wait_for_initialize_req()? {
            Some((params, id)) => (params, id),
            None => return Ok(()),
        };

        let result = f(params);

        self.write_msg(&model::Msg::Res(model::Res {
            id,
            result: serde_json::to_value(result)?,
            error: None,
        }))?;

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

impl ReqHandler {
    pub(crate) fn on<R, F>(mut self, f: F) -> Result<Self, proto::WriteMsgError>
    where
        R: Request,
        F: FnOnce(R::Params) -> Result<R::Result, proto::WriteMsgError>,
    {
        let req = match mem::take(&mut self.req) {
            Some(req) => req,
            None => return Ok(self),
        };

        if req.method == R::METHOD {
            // types from lsp_types can always be deserialized as JSON
            f(serde_json::from_value(req.params).unwrap())?;
        }

        Ok(self)
    }
}

impl NotHandler {
    pub(crate) fn on<N, F>(mut self, f: F) -> Result<Self, proto::WriteMsgError>
    where
        N: Notification,
        F: FnOnce(N::Params) -> Result<(), proto::WriteMsgError>,
    {
        let not = match mem::take(&mut self.not) {
            Some(req) => req,
            None => return Ok(self),
        };

        if not.method == N::METHOD {
            // types from lsp_types can always be deserialized as JSON
            f(serde_json::from_value(not.params).unwrap())?;
        }

        Ok(self)
    }
}

impl ConnectionStorage {
    pub(crate) fn new() -> Self {
        Self { stdin: io::stdin(), stdout: io::stdout() }
    }
}
