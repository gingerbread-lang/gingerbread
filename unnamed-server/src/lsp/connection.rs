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
}

pub(crate) struct ConnectionStorage {
    stdin: io::Stdin,
    stdout: io::Stdout,
}

impl ConnectionStorage {
    pub(crate) fn new() -> Self {
        Self { stdin: io::stdin(), stdout: io::stdout() }
    }
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
        };

        let (params, id) = match connection.wait_for_initialize_req()? {
            Some((params, id)) => (params, id),
            None => return Ok(None),
        };

        let result = f(params);

        connection.write_msg(&model::Msg::Res(model::Res {
            id,
            result: serde_json::to_value(result)?,
            error: None,
        }))?;

        match connection.read_msg()? {
            model::Msg::Not(not) if not.method == Initialized::METHOD => {}
            _ => return Err(InitializeError::NoInitializedNot),
        }

        Ok(Some(connection))
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
}

impl Connection<'_> {
    pub(crate) fn write_msg(&mut self, msg: &model::Msg) -> Result<(), proto::WriteMsgError> {
        proto::write_msg(&mut self.writer, msg)
    }

    pub(crate) fn read_msg(&mut self) -> Result<model::Msg, proto::ReadMsgError> {
        proto::read_msg(&mut self.reader, &mut self.buf)
    }
}
