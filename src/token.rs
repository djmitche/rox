use unicode_width::UnicodeWidthStr;

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
pub struct Token<'p> {
    /// Type of the token
    pub ty: TokenType,
    /// Source for this token, from the input program. For tokens not completely identified by
    /// `ty`, this is further interpreted during parsing.
    pub src: &'p str,
}

impl<'p> Token<'p> {
    /// Return a two-line string highlighting this token in the line that contains it, preceded by
    /// a line number.
    // TODO: doesn't handle multi-line tokens
    pub fn highlight_in_line(&self, program: &str) -> String {
        let offset =
            crate::util::str_offset_in(self.src, program).expect("token is not in program");
        let before_token = &program[..offset];
        // Calculate the line number of this token.
        let line_num = before_token.chars().filter(|c| *c == '\n').count() + 1;
        // Calculate the offsets of the beginning and end of the line containing this token.
        let beginning_of_line = before_token.rfind('\n').map(|i| i + 1).unwrap_or(0);
        let line_len = program[beginning_of_line..]
            .find('\n')
            .unwrap_or_else(|| program[beginning_of_line..].len());
        let line = &program[beginning_of_line..beginning_of_line + line_len];
        // Calculate the width (in terminal characters) of the text before the token.
        let terminal_offset = UnicodeWidthStr::width(&program[beginning_of_line..offset]);
        // Calculate the width of the token itself.
        let terminal_token_width =
            UnicodeWidthStr::width(&program[offset..offset + self.src.len()]);
        let line_num_str = format!("{}", line_num);
        let line_num_width = line_num_str.len();
        format!("{}: {}\n{empty:line_num_width$}  {empty:terminal_offset$}{empty:^<terminal_token_width$} {:?}",
                line_num_str, line, self.ty, empty="")
    }
}

pub type Tokens<'p> = Vec<Token<'p>>;
