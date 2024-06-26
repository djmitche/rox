use crate::src::Src;
use crate::token::{Token, TokenType, Tokens};
use crate::util::str_offset_in;
use anyhow::Result;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, digit1, multispace0},
    combinator::{all_consuming, not, recognize},
    multi::many0,
    sequence::{pair, preceded, terminated, tuple},
    Finish, IResult,
};

/// A token in the input stream, using a string as the src. This is compatible with nom's
/// parsing of `&str`.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Tok<'p> {
    /// Type of the token
    pub ty: TokenType,
    /// Source for this token, from the input program. For tokens not completely identified by
    /// `ty`, this is further interpreted during parsing.
    pub src: &'p str,
}

/// Either whitespace or an EOL comment.
fn space_or_comment(i: &str) -> IResult<&str, &str> {
    recognize(tuple((
        // Zero or more space + comment..
        many0(preceded(
            multispace0,
            tuple((multispace0, tag("//"), many0(is_not("\n")))),
        )),
        // Followed by zero or more spaces.
        multispace0,
    )))(i)
}

/// A piece of punctuation, which can be squished right up against other tokens.
fn punctuation(literal: &'static str, ty: TokenType) -> impl FnMut(&str) -> IResult<&str, Tok> {
    move |i| {
        let (i, val) = recognize(tag(literal))(i)?;
        Ok((i, Tok { ty, src: val }))
    }
}

/// A keyword, which must be followed by a non-alpha character.
fn keyword(literal: &'static str, ty: TokenType) -> impl FnMut(&str) -> IResult<&str, Tok> {
    move |i| {
        let (i, val) = recognize(pair(tag(literal), not(alphanumeric1)))(i)?;
        Ok((i, Tok { ty, src: val }))
    }
}

fn identifier(i: &str) -> IResult<&str, Tok> {
    let ty = TokenType::Identifier;
    let (i, val) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(i)?;
    Ok((i, Tok { ty, src: val }))
}

fn string(i: &str) -> IResult<&str, Tok> {
    let ty = TokenType::String;
    let (i, val) = recognize(tuple((tag("\""), many0(is_not("\"")), tag("\""))))(i)?;
    Ok((i, Tok { ty, src: val }))
}

fn number(i: &str) -> IResult<&str, Tok> {
    let ty = TokenType::Number;
    let (i, val) = recognize(alt((recognize(tuple((digit1, tag("."), digit1))), digit1)))(i)?;
    Ok((i, Tok { ty, src: val }))
}

fn token(i: &str) -> IResult<&str, Tok> {
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
        number,
        identifier,
        string,
    ))(i)
}

/// Scan the given input into a sequence of tokens.
pub fn scan(program: &str) -> Result<Tokens> {
    // Get the sequence of tokens, with whitespace allowed at the beginning and end and between
    // each token.
    match all_consuming(terminated(
        many0(preceded(space_or_comment, token)),
        space_or_comment,
    ))(program)
    .finish()
    {
        Ok((_, tokens)) => Ok(tokens
            .into_iter()
            .map(|tok| Token {
                ty: tok.ty,
                src: Src {
                    offset: str_offset_in(tok.src, program).expect("Token was not in program"),
                    len: tok.src.len(),
                },
            })
            .collect()),
        // Anyhow errors don't wrap nom errors very well..
        Err(e) => anyhow::bail!("scanner error: {e:?}"),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use TokenType::*;

    /// Convert Tokens into tuples of type and string slice. This allows easier assertions in
    /// tests, instead of comparing against `Token` values, which include offsets in the program.
    ///
    /// The cost is that assertions don't completely cover those offsets. For example, in `a + a`,
    /// the first and last tokens are represented as `(Identifier, "a")`, and the assertion would
    /// pass even if both are slices of the first occurrence of `a` in the program.
    fn scan_result<'p>(program: &'p str) -> Vec<(TokenType, &'p str)> {
        let tokens = scan(program).unwrap();
        tokens
            .into_iter()
            .map(|token| (token.ty, token.src_str(program)))
            .collect()
    }

    #[test]
    fn parens_and_whitespace() {
        let exp = vec![(LeftParen, "("), (RightParen, ")")];
        assert_eq!(scan_result("()"), exp);
        assert_eq!(scan_result("( )"), exp);
        assert_eq!(scan_result(" ()"), exp);
        assert_eq!(scan_result("() "), exp);
        assert_eq!(scan_result(" ( )"), exp);
        assert_eq!(scan_result(" () "), exp);
        assert_eq!(scan_result(" ( ) "), exp);
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
        assert_eq!(scan_result("(){},.-+;/*!!====>>=<<="), exp);
        assert_eq!(
            scan_result("( ) { } , . - + ; / * ! != == = > >= < <="),
            exp
        );
    }

    #[test]
    fn multi_equal() {
        // This is a choice, but we choose to parse multiple `=` characters preferentially as `==`
        // with an odd number ending with `=`.
        assert_eq!(scan_result("==="), vec![(EqualEqual, "=="), (Equal, "=")]);
        assert_eq!(
            scan_result("===="),
            vec![(EqualEqual, "=="), (EqualEqual, "==")]
        );
        assert_eq!(
            scan_result("====="),
            vec![(EqualEqual, "=="), (EqualEqual, "=="), (Equal, "=")]
        );
    }

    #[test]
    fn keywords_and_whitespace() {
        let exp = vec![(And, "and")];
        assert_eq!(scan_result("and"), exp);
        assert_eq!(scan_result(" and"), exp);
        assert_eq!(scan_result("and "), exp);
        assert_eq!(scan_result(" and "), exp);
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
            scan_result(
                "and class else false fun for if nil or print return super this true var while"
            ),
            exp
        );
    }

    #[test]
    fn keywords_mushed_together() {
        assert_eq!(scan_result("andand"), vec![(Identifier, "andand")]);
        assert_eq!(scan_result("varif"), vec![(Identifier, "varif")]);
        assert_eq!(scan_result("and and"), vec![(And, "and"), (And, "and")]);
    }

    #[test]
    fn keywords_and_punctuation() {
        let exp = vec![(LeftParen, "("), (And, "and"), (RightBrace, "}")];
        assert_eq!(scan_result("(and}"), exp);
        assert_eq!(scan_result("( and }"), exp);
    }

    #[test]
    fn numbers() {
        assert_eq!(scan_result("1.2"), vec![(Number, "1.2")]);
        assert_eq!(scan_result("12.34"), vec![(Number, "12.34")]);
        assert_eq!(scan_result("1"), vec![(Number, "1")]);
    }

    #[test]
    fn comments() {
        assert_eq!(scan_result("if // 123"), vec![(If, "if")]);
    }

    #[test]
    fn invalid_numbers() {
        // Maybe these should be an error, but let's just tokenize them as we find them.
        assert_eq!(scan_result(".1"), vec![(Dot, "."), (Number, "1")]);
        assert_eq!(scan_result(".12"), vec![(Dot, "."), (Number, "12")]);
        assert_eq!(scan_result("1."), vec![(Number, "1"), (Dot, ".")]);
        assert_eq!(scan_result("12."), vec![(Number, "12"), (Dot, ".")]);
    }

    #[test]
    fn identifiers() {
        for ident in ["foo", "_foo", "foo_", "foo_bar", "foo123", "_123", "__"] {
            assert_eq!(scan_result(ident), vec![(Identifier, ident)]);
        }
    }

    #[test]
    fn strings() {
        assert_eq!(scan_result(r#""foo""#), vec![(String, r#""foo""#)]);
        assert_eq!(scan_result(r#""""#), vec![(String, r#""""#)]);
        assert_eq!(scan_result(r#""123""#), vec![(String, r#""123""#)]);
        assert_eq!(scan_result(r#""if""#), vec![(String, r#""if""#)]);
        assert_eq!(scan_result(r#""foo bar""#), vec![(String, r#""foo bar""#)]);
        assert_eq!(scan_result("\"foo\nbar\""), vec![(String, "\"foo\nbar\"")]);
    }
}
