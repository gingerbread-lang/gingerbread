use super::model::Msg;
use std::io::{self, BufRead, Write};
use thiserror::Error;

#[derive(Default)]
pub struct ScratchReadBuf {
    string: String,
    bytes: Vec<u8>,
}

pub fn read_msg(reader: &mut impl BufRead, buf: &mut ScratchReadBuf) -> Result<Msg, ReadMsgError> {
    let header = read_header(reader, &mut buf.string)?;
    read_content(reader, &mut buf.bytes, header.content_length)?;
    let msg = serde_json::from_slice(&buf.bytes)?;

    Ok(msg)
}

pub fn write_msg(writer: &mut impl Write, msg: &Msg) -> Result<(), WriteMsgError> {
    // we know Msg can always be serialized
    let serialized = serde_json::to_string(msg).unwrap();

    writer.write_all(
        format!("Content-Length: {}\r\n\r\n{}", serialized.len(), serialized).as_bytes(),
    )?;

    writer.flush()?;

    Ok(())
}

fn read_content(
    reader: &mut impl BufRead,
    buf: &mut Vec<u8>,
    content_length: usize,
) -> Result<(), ReadMsgError> {
    buf.resize(content_length, 0);
    reader.read_exact(buf)?;

    Ok(())
}

fn read_header(reader: &mut impl BufRead, buf: &mut String) -> Result<Header, ReadMsgError> {
    let mut content_length = None;
    let mut seen_content_type = false;

    while let Some(field) = read_header_field(reader, buf)? {
        match field.name {
            "Content-Length" if content_length.is_some() => {
                return Err(ReadMsgError::DuplicatedHeaderFieldName)
            }

            "Content-Length" => match field.value.parse() {
                Ok(len) => content_length = Some(len),
                Err(_) => return Err(ReadMsgError::NonIntContentLength),
            },

            "Content-Type" => {
                if seen_content_type {
                    return Err(ReadMsgError::DuplicatedHeaderFieldName);
                }

                // the spec recommends the incorrect `utf8` is supported
                // in addition to the correct `utf-8`
                // for backwards compatibility
                if let "application/vscode-jsonrpc; charset=utf-8"
                | "application/vscode-jsonrpc; charset=utf8" = field.value
                {
                    return Err(ReadMsgError::NonDefaultContentType);
                }

                seen_content_type = true;
            }

            _ => return Err(ReadMsgError::UnrecognizedHeaderFieldName),
        }
    }

    match content_length {
        Some(content_length) => Ok(Header { content_length }),
        None => Err(ReadMsgError::MissingContentLength),
    }
}

fn read_header_field<'a>(
    reader: &mut impl BufRead,
    buf: &'a mut String,
) -> Result<Option<HeaderField<'a>>, ReadMsgError> {
    buf.clear();
    reader.read_line(buf)?;

    let field_text = match buf.strip_suffix("\r\n") {
        // in this case we saw \r\n when we should have seen another field,
        // so we return None to indicate we’ve reached the end of the header
        Some("") => return Ok(None),

        Some(field_text) => field_text,
        None => return Err(ReadMsgError::MissingCarriageReturnAtEndOfHeaderField),
    };

    match field_text.split_once(": ") {
        Some((name, value)) => Ok(Some(HeaderField { name, value })),
        None => Err(ReadMsgError::MissingHeaderFieldNameValueSeparator),
    }
}

struct Header {
    content_length: usize,
}

struct HeaderField<'a> {
    name: &'a str,
    value: &'a str,
}

#[derive(Debug, Error)]
pub enum WriteMsgError {
    #[error("failed writing data to connection")]
    Io(#[from] io::Error),
}

#[derive(Debug, Error)]
pub enum ReadMsgError {
    #[error("failed reading data from connection")]
    Io(#[from] io::Error),

    #[error("failed deserializing message content from JSON")]
    Deserialize(#[from] serde_json::Error),

    #[error("a header field other than Content-Length or Content-Type was supplied")]
    UnrecognizedHeaderFieldName,

    #[error("a Content-Type other than the default of application/vscode-jsonrpc and UTF-8 was specified")]
    NonDefaultContentType,

    #[error("a header field with the same name was provided multiple times")]
    DuplicatedHeaderFieldName,

    #[error("the Content-Length header field was not supplied")]
    MissingContentLength,

    #[error("the Content-Length header field did not have an integer value")]
    NonIntContentLength,

    #[error("the header was missing a carriage return (\\r) at the end of a field")]
    MissingCarriageReturnAtEndOfHeaderField,

    #[error("the `: ` separator between a header field’s name and value was missing")]
    MissingHeaderFieldNameValueSeparator,
}
