#![allow(dead_code)]
use anyhow::Result;
use unicode_width::UnicodeWidthStr;

/// The type of a token.
#[derive(Debug, PartialEq, Eq)]
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
    Var,
    While,
    Eof,
}

/// A token in the input stream.
#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    /// Type of the token
    ty: TokenType,
    /// Byte offset of the first character of the token in the input string.
    offset: usize,
    /// Length, in bytes, of the lexeme.
    len: usize,
}

impl Token {
    pub fn new(ty: TokenType, offset: usize, len: usize) -> Self {
        Token { ty, offset, len }
    }

    /// Return a two-line string highlighting this token in the line that contains it, preceded by
    /// a line number.
    // TODO: doesn't handle multi-line tokens
    pub fn highlight_in_line(&self, program: &str) -> String {
        let before_token = &program[..self.offset];
        // Calculate the line number of this token.
        let line_num = before_token.chars().filter(|c| *c == '\n').count() + 1;
        // Calculate the offsets of the beginning and end of the line containing this token.
        let beginning_of_line = before_token.rfind('\n').map(|i| i + 1).unwrap_or(0);
        let line_len = program[beginning_of_line..]
            .find('\n')
            .unwrap_or_else(|| program[beginning_of_line..].len());
        let line = &program[beginning_of_line..beginning_of_line + line_len];
        // Calculate the width (in terminal characters) of the text before the token.
        let terminal_offset = UnicodeWidthStr::width(&program[beginning_of_line..self.offset]);
        // Calculate the width of the token itself.
        let terminal_token_width =
            UnicodeWidthStr::width(&program[self.offset..self.offset + self.len]);
        let line_num_str = format!("{}", line_num);
        let line_num_width = line_num_str.len();
        format!("{}: {}\n{empty:line_num_width$}  {empty:terminal_offset$}{empty:^<terminal_token_width$} {:?}",
                line_num_str, line, self.ty, empty="")
    }
}

/// Scan the given input into a sequence of tokens.
pub fn scan_tokens(program: &str) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let len = program.len();
    let mut char_indices = program.char_indices().peekable();

    loop {
        let Some((i, c)) = char_indices.next() else {
            break;
        };

        let next_c = char_indices.peek().map(|(_, c)| *c);
        let next_is = |p| next_c == Some(p);

        // First, ignore whitespace and comments.
        match c {
            '/' if next_is('/') => {
                // Skip second slash.
                char_indices.next();
                // Skip to newline or EOF.
                while let Some((_, c)) = char_indices.next() {
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }
            ' ' | '\r' | '\t' | '\n' => continue,
            _ => {},
        }

        // Match 1- and 2-character tokens.
        if let Some((ty, mut len)) = match c {
            // Single-character tokens.
            '(' => Some((TokenType::LeftParen, 1)),
            ')' => Some((TokenType::RightParen, 1)),
            '{' => Some((TokenType::LeftBrace, 1)),
            '}' => Some((TokenType::RightBrace, 1)),
            ',' => Some((TokenType::Comma, 1)),
            '.' => Some((TokenType::Dot, 1)),
            '-' => Some((TokenType::Minus, 1)),
            '+' => Some((TokenType::Plus, 1)),
            ';' => Some((TokenType::Semicolon, 1)),
            '*' => Some((TokenType::Star, 1)),
            '/' => Some((TokenType::Slash, 1)),
            // Characters that may be single or the first of a 2-character token.
            '!' if next_is('=') => Some((TokenType::BangEqual, 2)),
            '!' => Some((TokenType::Bang, 1)),
            '=' if next_is('=') => Some((TokenType::EqualEqual, 2)),
            '=' => Some((TokenType::Equal, 1)),
            '>' if next_is('=') => Some((TokenType::GreaterEqual, 2)),
            '>' => Some((TokenType::Greater, 1)),
            '<' if next_is('=') => Some((TokenType::LessEqual, 2)),
            '<' => Some((TokenType::Less, 1)),
            _ => None,
        } {
            tokens.push(Token::new(ty, i, len));
            while len > 1 {
                // Skip any peek'd values.
                char_indices.next();
                len -= 1;
            }
            continue;
        }

        // Match numbers.
        if c.is_numeric() {
        }
    }

    tokens.push(Token::new(TokenType::Eof, len, 0));
    Ok(dbg!(tokens))
}
