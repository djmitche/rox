#![allow(dead_code, unused_imports)]
use anyhow::Result;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0},
    combinator::{all_consuming, map_res, not, recognize, value},
    multi::{many0, separated_list0},
    sequence::{pair, preceded, terminated},
    Finish, IResult,
};
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
    Identifier, // TODO
    String,     // TODO
    Number,     // TODO
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
    Eof,
}

/// A token in the input stream.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token<'p> {
    /// Type of the token
    ty: TokenType,
    /// String representing this token. This should be a slice of the original program.
    val: &'p str,
}

impl<'p> Token<'p> {
    pub fn new(ty: TokenType, val: &'p str) -> Self {
        Token { ty, val }
    }

    /// Return a two-line string highlighting this token in the line that contains it, preceded by
    /// a line number.
    // TODO: doesn't handle multi-line tokens
    pub fn highlight_in_line(&self, program: &str) -> String {
        let offset =
            crate::util::str_offset_in(self.val, program).expect("token is not in program");
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
            UnicodeWidthStr::width(&program[offset..offset + self.val.len()]);
        let line_num_str = format!("{}", line_num);
        let line_num_width = line_num_str.len();
        format!("{}: {}\n{empty:line_num_width$}  {empty:terminal_offset$}{empty:^<terminal_token_width$} {:?}",
                line_num_str, line, self.ty, empty="")
    }
}

/// A piece of punctuation, which can be squished right up against other tokens.
fn punctuation(literal: &'static str, ty: TokenType) -> impl FnMut(&str) -> IResult<&str, Token> {
    move |i| {
        let (i, val) = recognize(tag(literal))(i)?;
        Ok((i, Token { ty, val }))
    }
}

/// A keyword, which must be followed by a non-alpha character.
fn keyword(literal: &'static str, ty: TokenType) -> impl FnMut(&str) -> IResult<&str, Token> {
    move |i| {
        let (i, val) = recognize(pair(tag(literal), not(alphanumeric1)))(i)?;
        Ok((i, Token { ty, val }))
    }
}

fn token(i: &str) -> IResult<&str, Token> {
    use TokenType::*;
    alt((
        alt((
            punctuation("(", LeftParen),
            punctuation(")", RightParen),
            punctuation("{", LeftBrace),
            punctuation("}", RightBrace),
            punctuation(",", Comma),
            punctuation(".", Dot),
            punctuation("-", Minus),
            punctuation("+", Plus),
            punctuation(";", Semicolon),
            punctuation("/", Slash),
            punctuation("*", Star),
            // For the remainder, the longer version must come first.
            punctuation("!=", BangEqual),
            punctuation("!", Bang),
            punctuation("==", EqualEqual),
            punctuation("=", Equal),
            punctuation(">=", GreaterEqual),
            punctuation(">", Greater),
            punctuation("<=", LessEqual),
            punctuation("<", Less),
        )),
        alt((
            keyword("and", And),
            keyword("class", Class),
            keyword("else", Else),
            keyword("false", False),
            keyword("fun", Fun),
            keyword("for", For),
            keyword("if", If),
            keyword("nil", Nil),
            keyword("or", Or),
            keyword("print", Print),
            keyword("return", Return),
            keyword("super", Super),
            keyword("this", This),
            keyword("true", True),
            keyword("var", Var),
            keyword("while", While),
        )),
    ))(i)
}

/// Scan the given input into a sequence of tokens.
pub fn scan(program: &str) -> Result<Vec<Token>> {
    // Get the sequence of tokens, with whitespace allowed at the beginning and end and between
    // each token.
    match all_consuming(terminated(many0(preceded(multispace0, token)), multispace0))(program)
        .finish()
    {
        Ok((_, tokens)) => Ok(tokens),
        // Anyhow errors don't wrap nom errors very well..
        Err(e) => anyhow::bail!("scanner error: {e:?}"),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use TokenType::*;

    fn to_strings<'p>(tokens: &[Token<'p>]) -> Vec<(TokenType, &'p str)> {
        tokens.into_iter().map(|t| (t.ty, t.val)).collect()
    }

    #[test]
    fn parens_and_whitespace() {
        let exp = vec![(LeftParen, "("), (RightParen, ")")];
        assert_eq!(to_strings(&scan("()").unwrap()), exp);
        assert_eq!(to_strings(&scan("( )").unwrap()), exp);
        assert_eq!(to_strings(&scan(" ()").unwrap()), exp);
        assert_eq!(to_strings(&scan("() ").unwrap()), exp);
        assert_eq!(to_strings(&scan(" ( )").unwrap()), exp);
        assert_eq!(to_strings(&scan(" () ").unwrap()), exp);
        assert_eq!(to_strings(&scan(" ( ) ").unwrap()), exp);
    }

    #[test]
    fn punctuation() {
        let exp = vec![
            (LeftParen, "("),
            (RightParen, ")"),
            (LeftBrace, "{"),
            (RightBrace, "}"),
            (Comma, ","),
            (Dot, "."),
            (Minus, "-"),
            (Plus, "+"),
            (Semicolon, ";"),
            (Slash, "/"),
            (Star, "*"),
            (Bang, "!"),
            (BangEqual, "!="),
            (EqualEqual, "=="),
            (Equal, "="),
            (Greater, ">"),
            (GreaterEqual, ">="),
            (Less, "<"),
            (LessEqual, "<="),
        ];
        assert_eq!(to_strings(&scan("(){},.-+;/*!!====>>=<<=").unwrap()), exp);
        assert_eq!(
            to_strings(&scan("( ) { } , . - + ; / * ! != == = > >= < <=").unwrap()),
            exp
        );
    }

    #[test]
    fn multi_equal() {
        // This is a choice, but we choose to parse multiple `=` characters preferentially as `==`
        // with an odd number ending with `=`.
        assert_eq!(
            to_strings(&scan("===").unwrap()),
            vec![(EqualEqual, "=="), (Equal, "=")]
        );
        assert_eq!(
            to_strings(&scan("====").unwrap()),
            vec![(EqualEqual, "=="), (EqualEqual, "==")]
        );
        assert_eq!(
            to_strings(&scan("=====").unwrap()),
            vec![(EqualEqual, "=="), (EqualEqual, "=="), (Equal, "=")]
        );
    }

    #[test]
    fn keywords_and_whitespace() {
        let exp = vec![(And, "and")];
        assert_eq!(to_strings(&scan("and").unwrap()), exp);
        assert_eq!(to_strings(&scan(" and").unwrap()), exp);
        assert_eq!(to_strings(&scan("and ").unwrap()), exp);
        assert_eq!(to_strings(&scan(" and ").unwrap()), exp);
    }

    #[test]
    fn keywords() {
        let exp = vec![
            (And, "and"),
            (Class, "class"),
            (Else, "else"),
            (False, "false"),
            (Fun, "fun"),
            (For, "for"),
            (If, "if"),
            (Nil, "nil"),
            (Or, "or"),
            (Print, "print"),
            (Return, "return"),
            (Super, "super"),
            (This, "this"),
            (True, "true"),
            (Var, "var"),
            (While, "while"),
        ];
        assert_eq!(
            to_strings(
                &scan(
                    "and class else false fun for if nil or print return super this true var while"
                )
                .unwrap()
            ),
            exp
        );
    }

    #[test]
    fn keywords_mushed_together() {
        assert!(&scan("andand").is_err());

        let exp = vec![(And, "and"), (And, "and")];
        assert_eq!(to_strings(&scan("and and").unwrap()), exp);
    }

    #[test]
    fn keywords_and_punctuation() {
        let exp = vec![(LeftParen, "("), (And, "and"), (RightBrace, "}")];
        assert_eq!(to_strings(&scan("(and}").unwrap()), exp);
        assert_eq!(to_strings(&scan("( and }").unwrap()), exp);
    }
}
