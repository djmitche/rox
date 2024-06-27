use thiserror::Error;
use crate::src::Src;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Syntax error: {0} at {1:?}")]
    SyntaxError(String, Src),
}

pub type Result<T> = std::result::Result<T, Error>;

impl Error {
    pub fn syntax<T>(message: impl Into<String>, src: Src) -> Result<T> {
        Err(Error::SyntaxError(message.into(), src))
    }
}
