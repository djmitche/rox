//! Components out of which the parser is built
//!
//! This is loosely modeled on nom, as combinatorial "recognizers" which recognize
//! some prefix of the token stream starting at the given token.
use super::{ParseResult, Parser};
use crate::token::{Token, TokenType};

use ParseResult::*;

pub trait Recognizer: Sized {
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

    fn map<U>(self, f: impl Fn(Self::Output) -> U) -> impl Recognizer<Output = U> {
        RecognizeMap { r: self, f }
    }
}

/// Buil da recognizer that will match the given toekn type and return the token.
pub(super) fn recognize_token(ty: TokenType) -> impl Recognizer<Output = Token> {
    RecognizeToken(ty)
}

/// Build a recognizer that executes the given function.
pub(super) fn recognizer<F, T>(f: F) -> impl Recognizer<Output = T>
where
    F: Fn(&mut Parser, usize) -> ParseResult<T>,
{
    RecognizeFn { f }
}

/// Recognize nothing. This is useful in alternatives.
pub(super) fn recognize_nothing() -> impl Recognizer<Output = ()> {
    recognizer(|_, t| Success(t, ()))
}

pub(super) fn left_assoc<SUB, SEP, SEPT, FOLD, T>(
    sub: SUB,
    sep: SEP,
    fold: FOLD,
) -> impl Recognizer<Output = T>
where
    SUB: Recognizer<Output = T>,
    SEP: Recognizer<Output = SEPT>,
    FOLD: Fn(T, SEPT, T) -> T,
{
    recognizer(move |parser, t| {
        let (lhs_t, lhs) = sub.recognize(parser, t)?;
        let mut t = lhs_t;
        let mut n = lhs;
        loop {
            let Success(op_t, op) = sep.recognize(parser, t) else {
                break;
            };
            let Success(rhs_t, rhs) = sub.recognize(parser, op_t) else {
                break;
            };
            t = rhs_t;
            n = fold(n, op, rhs)
        }
        Success(t, n)
    })
}

/// A recognizer that always fails.
struct RecognizeFail<T>(std::marker::PhantomData<T>);
impl<T> Recognizer for RecognizeFail<T> {
    type Output = T;

    fn recognize(&self, _parser: &mut Parser, _t: usize) -> ParseResult<Self::Output> {
        Failure
    }
}

/// A recognizer for a single token.
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

/// A recognizer for a tuple of tokens, returning that tuple.
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

/// A recognizer for a set of alternatives, matched in order.
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

/// A recognizer that maps the given function onto the success value of the given recognizer.
struct RecognizeMap<R, F, T, U>
where
    R: Recognizer<Output = T>,
    F: Fn(T) -> U,
{
    r: R,
    f: F,
}
impl<R, F, T, U> Recognizer for RecognizeMap<R, F, T, U>
where
    R: Recognizer<Output = T>,
    F: Fn(T) -> U,
{
    type Output = U;

    fn recognize(&self, parser: &mut Parser, t: usize) -> ParseResult<Self::Output> {
        match self.r.recognize(parser, t) {
            Success(t, v) => Success(t, (self.f)(v)),
            Failure => Failure,
            Error(e) => Error(e),
        }
    }
}

/// A recognizer that delegates to a function.
struct RecognizeFn<F, T>
where
    F: Fn(&mut Parser, usize) -> ParseResult<T>,
{
    f: F,
}
impl<F, T> Recognizer for RecognizeFn<F, T>
where
    F: Fn(&mut Parser, usize) -> ParseResult<T>,
{
    type Output = T;

    fn recognize(&self, parser: &mut Parser, t: usize) -> ParseResult<Self::Output> {
        (self.f)(parser, t)
    }
}

/// Create a recognizer that will match the first successful item.
macro_rules! first_success {
    ($branch1:expr, $($branchn:expr),*, ) => {
        or_first_success!($branch1, $($branchn),*)
    };
    ($branch1:expr, $($branchn:expr),*) => {
        or_first_success!($branch1, $($branchn),*)
    };
}

macro_rules! or_first_success {
    ($prefix:expr,) => {
        $prefix
    };
    ($prefix:expr, $branch1:expr) => {
        or_first_success!($prefix, $branch1,)
    };
    ($prefix:expr, $branch1:expr, $($branchn:expr),*) => {
        or_first_success!($prefix.or($branch1), $($branchn),*)
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{error::Errors, scanner::scan, src::Src};

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

    #[test]
    fn test_map() {
        let program = "abc";
        let mut parser = Parser {
            program,
            tokens: &scan(program).unwrap(),
            errors: Errors::new("test"),
        };
        assert_eq!(
            recognize_token(TokenType::Identifier)
                .map(|t| program[t.src.offset..t.src.offset + t.src.len].to_string())
                .recognize(&mut parser, 0),
            Success(1, String::from("abc"))
        );
    }
}
