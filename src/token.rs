use crate::src::Src;

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
}

pub type Tokens = Vec<Token>;
