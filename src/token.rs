use crate::src::Src;
use thiserror::Error;

#[derive(Error, Copy, Clone, Debug, PartialEq, Eq)]
pub enum ScannerError {
    #[error("Invalid number")]
    InvalidNumber,
    #[error("Unterminated string")]
    UnterminatedString,
    #[error("Unrecognized input")]
    UnrecognizedInput,
}

/// The type of a token.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    /// A special variant that indicates an error occurred while scanning.
    Error(ScannerError),
}

/// A token in the input stream.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    /// Type of the token
    pub ty: TokenType,
    /// Source for this token, from the input program. For tokens not completely identified by
    /// `ty`, this is further interpreted during parsing.
    pub src: Src,
}

impl Token {
    /// Fetch the source string for this token, given the program.
    pub fn src_str<'p>(&self, program: &'p str) -> &'p str {
        &program[self.src.offset..self.src.offset + self.src.len]
    }

    /// Convert an Error variant into a [`crate::error::Error`]. Any other variant will panic.
    pub fn to_error(&self) -> crate::error::Error {
        let TokenType::Error(e) = self.ty else {
            panic!("Not an Error variant");
        };
        crate::error::Error::SyntaxError(e.to_string(), self.src)
    }
}

pub type Tokens = Vec<Token>;
