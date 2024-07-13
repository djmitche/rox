use crate::src::Src;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Syntax error: {0} at {1:?}")]
    SyntaxError(String, Src),
}

impl Error {
    pub fn syntax(message: impl Into<String>, src: Src) -> Error {
        Error::SyntaxError(message.into(), src)
    }
}

/// A collection of one or more errors that occurred while compiling the program.
#[derive(Debug, Default)]
pub struct Errors {
    pub phase: &'static str,
    pub errors: Vec<Error>,
}

impl Errors {
    pub fn new(phase: &'static str) -> Self {
        Errors {
            phase,
            errors: Vec::new(),
        }
    }

    pub fn from_error(phase: &'static str, error: Error) -> Self {
        Errors {
            phase,
            errors: vec![error],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn add(&mut self, error: Error) {
        self.errors.push(error);
    }
}

impl std::fmt::Display for Errors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.errors.len() == 1 {
            return self.errors[0].fmt(f);
        }
        writeln!(f, "From {}:", self.phase)?;
        for e in &self.errors {
            e.fmt(f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl std::error::Error for Errors {}

pub type Result<T> = std::result::Result<T, Error>;
pub type MultiResult<T> = std::result::Result<T, Errors>;
