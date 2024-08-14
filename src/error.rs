use crate::src::Src;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("Syntax error: {0} at {1:?}")]
    SyntaxError(String, Src),
    #[error("Runtime error: {0}")]
    RuntimeError(String),
    #[allow(dead_code)]
    #[error("Other error: {0}")]
    Other(String),
}

impl Error {
    pub fn syntax(message: impl Into<String>, src: Src) -> Error {
        Error::SyntaxError(message.into(), src)
    }
}

pub type Result<T> = std::result::Result<T, Error>;
