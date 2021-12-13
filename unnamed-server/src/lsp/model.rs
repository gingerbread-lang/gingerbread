use serde::ser::SerializeStruct;
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub(crate) enum Msg {
    Req(Req),
    Res(Res),
    Not(Not),
}

impl Serialize for Msg {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        const JSONRPC_VERSION: &str = "2.0";

        match self {
            Msg::Req(req) => {
                let mut s = serializer.serialize_struct("Req", 4)?;
                s.serialize_field("jsonrpc", JSONRPC_VERSION)?;
                s.serialize_field("id", &req.id)?;
                s.serialize_field("method", &req.method)?;
                s.serialize_field("params", &req.params)?;
                s.end()
            }

            Msg::Res(res) => {
                let mut s = serializer.serialize_struct("Res", 4)?;
                s.serialize_field("jsonrpc", JSONRPC_VERSION)?;
                s.serialize_field("id", &res.id)?;
                s.serialize_field("result", &res.result)?;

                if let Some(error) = &res.error {
                    s.serialize_field("error", error)?;
                }

                s.end()
            }

            Msg::Not(not) => {
                let mut s = serializer.serialize_struct("Not", 4)?;
                s.serialize_field("jsonrpc", JSONRPC_VERSION)?;
                s.serialize_field("method", &not.method)?;
                s.serialize_field("params", &not.params)?;
                s.end()
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct Req {
    pub(crate) id: ReqId,
    pub(crate) method: String,
    pub(crate) params: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct Res {
    pub(crate) id: ReqId,
    pub(crate) result: serde_json::Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) error: Option<ResError>,
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct Not {
    pub(crate) method: String,
    pub(crate) params: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct ResError {
    pub(crate) code: i32,
    pub(crate) message: String,
    pub(crate) data: Option<serde_json::Value>,
}

#[allow(dead_code)]
pub(crate) mod error_codes {
    pub(crate) const PARSE_ERROR: i32 = -32700;
    pub(crate) const INVALID_REQUEST: i32 = -32600;
    pub(crate) const METHOD_NOT_FOUND: i32 = -32601;
    pub(crate) const INVALID_PARAMS: i32 = -32602;
    pub(crate) const INTERNAL_ERROR: i32 = -32603;
    pub(crate) const SERVER_NOT_INITIALIZED: i32 = -32002;
    pub(crate) const UNKNOWN: i32 = -32001;
    pub(crate) const CONTENT_MODIFIED: i32 = -32801;
    pub(crate) const REQUEST_CANCELLED: i32 = -32800;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum ReqId {
    Integer(u32),
    String(String),
}
