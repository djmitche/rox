#![allow(unused_imports, dead_code)]
use crate::scanner::{Token, TokenType, Tokens};
use anyhow::Result;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take},
    character::complete::{alpha1, alphanumeric1, digit1, multispace0},
    combinator::{all_consuming, map_res, not, recognize, verify},
    multi::many0,
    sequence::{pair, preceded, terminated, tuple},
    Finish, IResult,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'p> {
    /// Variable, carrying the name of the variable.
    Variable(&'p str),
    /// String, containing the string content (without `"` characters).
    String(&'p str),
    /// Number literal, containing its textual representation.
    Number(&'p str),
    /// Boolean literal.
    Boolean(bool),
    /// Nil literal.
    Nil,
    /// Boolean negation.
    Not(ExprRef<'p>),
    /// Numeric negation.
    Neg(ExprRef<'p>),
    /// Multiplication.
    Mul(ExprRef<'p>, ExprRef<'p>),
    /// Division.
    Div(ExprRef<'p>, ExprRef<'p>),
    /// Addition.
    Add(ExprRef<'p>, ExprRef<'p>),
    /// Subtraction.
    Sub(ExprRef<'p>, ExprRef<'p>),
}

type ExprRef<'p> = Box<Expr<'p>>;

#[derive(Clone, Debug)]
pub struct Parser<'p> {
    tokens: Tokens<'p>,
    position: usize,
}

// Each parsing function returns (remainder, value) where `remainder` is the remaining token
// stream.

fn parse_primary<'p>(tokens: &'p [Token<'p>]) -> Result<(&'p [Token<'p>], Expr<'p>)> {
    let Some(tok) = tokens.get(0) else {
        anyhow::bail!("Expected primary, got EOF");
    };
    Ok((
        &tokens[1..],
        match tok.ty {
            TokenType::Identifier => Expr::Variable(tok.val),
            TokenType::String => Expr::String(&tok.val[1..tok.val.len() - 1]),
            TokenType::Number => Expr::Number(tok.val),
            TokenType::True => Expr::Boolean(true),
            TokenType::False => Expr::Boolean(false),
            TokenType::Nil => Expr::Nil,
            _ => anyhow::bail!("Expected primary, got {0:?}", tok.ty),
        },
    ))
}

fn parse_unary<'p>(tokens: &'p [Token<'p>]) -> Result<(&'p [Token<'p>], Expr<'p>)> {
    let expr_type = match tokens.get(0) {
        Some(Token { ty, .. }) if *ty == TokenType::Bang => Expr::Not,
        Some(Token { ty, .. }) if *ty == TokenType::Minus => Expr::Neg,
        // TODO ( .. )
        _ => return parse_primary(tokens),
    };
    let (remainder, inner) = parse_unary(&tokens[1..])?;
    Ok((remainder, expr_type(Box::new(inner))))
}

fn parse_factor<'p>(mut tokens: &'p [Token<'p>]) -> Result<(&'p [Token<'p>], Expr<'p>)> {
    let mut value;
    (tokens, value) = parse_unary(tokens)?;
    loop {
        let expr_type = match tokens.get(0) {
            Some(Token { ty, .. }) if *ty == TokenType::Slash => Expr::Div,
            Some(Token { ty, .. }) if *ty == TokenType::Star => Expr::Mul,
            _ => return Ok((tokens, value)),
        };
        tokens = &tokens[1..];
        let rhs;
        (tokens, rhs) = parse_unary(dbg!(tokens))?;
        value = expr_type(Box::new(value), Box::new(rhs));
    }
}

fn parse_term<'p>(mut tokens: &'p [Token<'p>]) -> Result<(&'p [Token<'p>], Expr<'p>)> {
    // TODO: test
    let mut value;
    (tokens, value) = parse_factor(tokens)?;
    loop {
        let expr_type = match tokens.get(0) {
            Some(Token { ty, .. }) if *ty == TokenType::Plus => Expr::Add,
            Some(Token { ty, .. }) if *ty == TokenType::Minus => Expr::Sub,
            _ => return Ok((tokens, value)),
        };
        tokens = &tokens[1..];
        let rhs;
        (tokens, rhs) = parse_factor(dbg!(tokens))?;
        value = expr_type(Box::new(value), Box::new(rhs));
    }
}

fn parse<'p>(tokens: &'p Tokens<'p>) -> Result<Expr<'p>> {
    let (remainder, result) = parse_term(&tokens[..])?;
    if remainder.len() > 0 {
        anyhow::bail!("Extra tokens at end");
    }
    Ok(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::scan;

    use Expr::*;

    fn r(expr: Expr) -> ExprRef {
        Box::new(expr)
    }

    #[test]
    fn ident() -> Result<()> {
        assert_eq!(parse(&scan("abc")?)?, Variable("abc"));
        Ok(())
    }

    #[test]
    fn unconsumed_tokens() -> Result<()> {
        assert!(parse(&scan("abc def")?).is_err());
        Ok(())
    }

    #[test]
    fn string() -> Result<()> {
        assert_eq!(parse(&scan("\"abc\" ")?)?, String("abc"));
        Ok(())
    }

    #[test]
    fn number() -> Result<()> {
        assert_eq!(parse(&scan(" 1.3")?)?, Number("1.3"));
        Ok(())
    }

    #[test]
    fn bool() -> Result<()> {
        assert_eq!(parse(&scan(" true ")?)?, Boolean(true));
        assert_eq!(parse(&scan(" false")?)?, Boolean(false));
        Ok(())
    }

    #[test]
    fn not() -> Result<()> {
        assert_eq!(parse(&scan("!false")?)?, Not(r(Boolean(false))));
        Ok(())
    }

    #[test]
    fn neg() -> Result<()> {
        assert_eq!(parse(&scan("-5")?)?, Neg(r(Number("5"))));
        Ok(())
    }

    #[test]
    fn double_not() -> Result<()> {
        assert_eq!(parse(&scan("!!false")?)?, Not(r(Not(r(Boolean(false))))));
        Ok(())
    }

    #[test]
    fn nil() -> Result<()> {
        assert_eq!(parse(&scan("nil")?)?, Nil);
        Ok(())
    }

    #[test]
    fn mul() -> Result<()> {
        assert_eq!(parse(&scan("1 *2")?)?, Mul(r(Number("1")), r(Number("2"))));
        Ok(())
    }
}
