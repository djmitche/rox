#![allow(unused_imports, dead_code)]
use crate::ast::parsed::{Declaration, Expr, Program, Stmt};
use crate::ast::{BinaryOp, LogicalOp, Node, NodeRef, UnaryOp};
use crate::error::{Error, Errors, MultiResult, Result};
use crate::scanner::scan;
use crate::src::Src;
use crate::token::{Token, TokenType, Tokens};

// ---

struct Parser<'p> {
    program: &'p str,
    tokens: &'p [Token],
    errors: Errors,
}

#[derive(Debug)]
enum ParseResult<T> {
    Success(usize, T),
    Failure,
    Error(String),
}

use ParseResult::*;

impl<T: std::cmp::PartialEq> PartialEq for ParseResult<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Success(lt, lv), Success(rt, rv)) => (lt, lv) == (rt, rv),
            // Errors are never equal
            (Error(_), Error(_)) => false,
            (Failure, Failure) => true,
            _ => false,
        }
    }
}

// ---

trait Recognizer: Sized {
    type Output;
    fn recognize(&self, parser: &mut Parser, t: usize) -> ParseResult<Self::Output>;

    fn then<R, T>(self, r: R) -> impl Recognizer<Output = (Self::Output, T)>
    where
        R: Recognizer<Output = T>,
    {
        RecognizeTuple { r1: self, r2: r }
    }

    fn or<R>(self, r: R) -> impl Recognizer<Output = Self::Output>
    where
        R: Recognizer<Output = Self::Output>,
    {
        RecognizeAlternate { r1: self, r2: r }
    }
}

struct RecognizeToken(TokenType);
impl Recognizer for RecognizeToken {
    type Output = Token;

    fn recognize(&self, parser: &mut Parser, t: usize) -> ParseResult<Self::Output> {
        let Some(token) = parser.tokens.get(t) else {
            return Failure;
        };
        if token.ty == self.0 {
            Success(t + 1, token.clone())
        } else {
            Failure
        }
    }
}

fn recognize_token(ty: TokenType) -> impl Recognizer<Output = Token> {
    RecognizeToken(ty)
}

struct RecognizeTuple<R1, T1, R2, T2>
where
    R1: Recognizer<Output = T1>,
    R2: Recognizer<Output = T2>,
{
    r1: R1,
    r2: R2,
}

impl<R1, T1, R2, T2> Recognizer for RecognizeTuple<R1, T1, R2, T2>
where
    R1: Recognizer<Output = T1>,
    R2: Recognizer<Output = T2>,
{
    type Output = (T1, T2);

    fn recognize(&self, parser: &mut Parser, t: usize) -> ParseResult<Self::Output> {
        // TODO: return errors correctly
        let Success(t, v1) = self.r1.recognize(parser, t) else {
            return Failure;
        };
        let Success(t, v2) = self.r2.recognize(parser, t) else {
            return Failure;
        };
        Success(t, (v1, v2))
    }
}

struct RecognizeAlternate<R1, R2, T>
where
    R1: Recognizer<Output = T>,
    R2: Recognizer<Output = T>,
{
    r1: R1,
    r2: R2,
}

impl<R1, R2, T> Recognizer for RecognizeAlternate<R1, R2, T>
where
    R1: Recognizer<Output = T>,
    R2: Recognizer<Output = T>,
{
    type Output = T;

    fn recognize(&self, parser: &mut Parser, t: usize) -> ParseResult<Self::Output> {
        match self.r1.recognize(parser, t) {
            rv @ Success(_, _) => return rv,
            Failure => (),
            Error(e) => return Error(e),
        };
        match self.r2.recognize(parser, t) {
            rv @ Success(_, _) => return rv,
            Failure => (),
            Error(e) => return Error(e),
        };
        Failure
    }
}

// TODO: map as method
// TODO: alt(&[..])
// TODO: catching and re-syncing after error?

/*
fn map<R, T, U, F>(mut r: R, f: F) -> impl FnMut(&mut Parser, usize) -> ParseResult<U>
where
    R: FnMut(&mut Parser, usize) -> ParseResult<T>,
    F: Fn(T) -> U,
{
    move |parser: &mut Parser, t: usize| -> ParseResult<U> {
        match r(parser, t) {
            Success(t, v) => Success(t, f(v)),
            Failure => Failure,
            Error(e) => Error(e),
        }
    }
}

impl<'p> Parser<'p> {
    fn parse_primary(&mut self, t: usize) -> ParseResult<Node<Expr>> {
        alt2(
            map(recognize_token(TokenType::Identifier), |tok| {
                Expr::variable(tok.src, tok.src_str(self.program))
            }),
            map(recognize_token(TokenType::String), |tok| {
                Expr::string(tok.src, tok.src_str(self.program))
            }),
        )(self, t)
    }
}
*/

#[cfg(test)]
mod test {
    use super::*;

    use crate::src::Src;

    /// Shortcut to create a Src.
    fn s(offset: usize, len: usize) -> Src {
        Src { offset, len }
    }

    /// Shortcut to create a Token
    fn tok(src: Src, ty: TokenType) -> Token {
        Token { src, ty }
    }

    #[test]
    fn recognize_token_match() {
        let program = "()";
        let mut parser = Parser {
            program,
            tokens: &scan(program).unwrap(),
            errors: Errors::new("test"),
        };
        assert_eq!(
            recognize_token(TokenType::LeftParen).recognize(&mut parser, 0),
            Success(1, tok(s(0, 1), TokenType::LeftParen))
        );
        assert_eq!(
            recognize_token(TokenType::RightParen).recognize(&mut parser, 1),
            Success(2, tok(s(1, 1), TokenType::RightParen))
        );
    }

    #[test]
    fn recognize_token_eof() {
        let program = "()";
        let mut parser = Parser {
            program,
            tokens: &scan(program).unwrap(),
            errors: Errors::new("test"),
        };
        assert_eq!(
            recognize_token(TokenType::RightParen).recognize(&mut parser, 2),
            Failure
        );
    }

    #[test]
    fn recognize_and_match() {
        let program = "1 () 2";
        let mut parser = Parser {
            program,
            tokens: &scan(program).unwrap(),
            errors: Errors::new("test"),
        };
        assert_eq!(
            recognize_token(TokenType::LeftParen)
                .then(recognize_token(TokenType::RightParen))
                .recognize(&mut parser, 1),
            Success(
                3,
                (
                    tok(s(2, 1), TokenType::LeftParen),
                    tok(s(3, 1), TokenType::RightParen)
                )
            )
        );
    }

    #[test]
    fn alt2_match() {
        let program = "false";
        let mut parser = Parser {
            program,
            tokens: &scan(program).unwrap(),
            errors: Errors::new("test"),
        };
        assert_eq!(
            recognize_token(TokenType::True)
                .or(recognize_token(TokenType::False))
                .recognize(&mut parser, 0),
            Success(1, tok(s(0, 5), TokenType::False))
        );
    }

    /*
    fn test_parse_primary(program: &str, t: usize, exp: ParseResult<Node<Expr>>) {
        let mut parser = Parser {
            program,
            tokens: &scan(program).unwrap(),
            errors: Errors::new("test"),
        };
        assert_eq!(parser.parse_primary(t), exp);
    }

    #[test]
    fn parse_primary_ident() {
        test_parse_primary("x", 0, Success(1, Expr::variable(s(0, 1), "x")));
    }

    #[test]
    fn parse_primary_string() {
        test_parse_primary("\"abc\"", 0, Success(1, Expr::string(s(0, 5), "\"abc\"")));
    }
    */
}
