#![allow(unused_imports, dead_code)]
use crate::ast::{Expr, Node, NodeRef};
use crate::token::{Token, TokenType, Tokens};
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

// Each parsing function returns (remainder, value) where `remainder` is the remaining token
// stream.

/// parse a primary (one-token) expression, or a parethesized expression.
fn parse_primary<'p>(tokens: &'p [Token<'p>]) -> Result<(&'p [Token<'p>], Node<Expr<'p>>)> {
    let Some(tok) = tokens.get(0) else {
        anyhow::bail!("Expected primary, got EOF");
    };
    Ok((
        &tokens[1..],
        match tok.ty {
            TokenType::Identifier => Expr::variable(tok.src, tok.src),
            TokenType::String => Expr::string(tok.src, &tok.src[1..tok.src.len() - 1]),
            TokenType::Number => Expr::number(tok.src, tok.src),
            TokenType::True => Expr::boolean(tok.src, true),
            TokenType::False => Expr::boolean(tok.src, false),
            TokenType::Nil => Expr::nil(tok.src),
            TokenType::LeftParen => return parse_parenthesized(&tokens[1..]),
            _ => anyhow::bail!("Expected primary, got {0:?}", tok.ty),
        },
    ))
}

/// Parse a parenthesized expression, where `tokens` begins after the opening parenthesis.
fn parse_parenthesized<'p>(tokens: &'p [Token<'p>]) -> Result<(&'p [Token<'p>], Node<Expr<'p>>)> {
    let (tokens, value) = parse_term(tokens)?;
    let Some(Token { ty, .. }) = tokens.get(0) else {
        anyhow::bail!("Unexpected EOF");
    };
    if *ty != TokenType::RightParen {
        anyhow::bail!("Expected RParen, got {0:?}", ty);
    }
    Ok((&tokens[1..], value))
}

fn parse_unary<'p>(tokens: &'p [Token<'p>]) -> Result<(&'p [Token<'p>], Node<Expr<'p>>)> {
    let (ty, src) = match tokens.get(0) {
        Some(Token { ty, src }) if [TokenType::Bang, TokenType::Minus].contains(ty) => (ty, src),
        _ => return parse_primary(tokens),
    };
    // Parse the unary expression following this operator.
    let (remainder, inner) = parse_unary(&tokens[1..])?;
    let result = match ty {
        TokenType::Bang => Expr::not(src, inner),
        TokenType::Minus => Expr::neg(src, inner),
        _ => unreachable!(),
    };
    Ok((remainder, result))
}

fn parse_factor<'p>(mut tokens: &'p [Token<'p>]) -> Result<(&'p [Token<'p>], Node<Expr<'p>>)> {
    let mut value;
    (tokens, value) = parse_unary(tokens)?;
    loop {
        let (ty, src) = match tokens.get(0) {
            Some(Token { ty, src }) if [TokenType::Star, TokenType::Slash].contains(ty) => {
                (ty, src)
            }
            _ => return Ok((tokens, value)),
        };
        tokens = &tokens[1..];
        let rhs;
        (tokens, rhs) = parse_unary(dbg!(tokens))?;
        value = match ty {
            TokenType::Star => Expr::mul(src, Box::new(value), Box::new(rhs)),
            TokenType::Slash => Expr::div(src, Box::new(value), Box::new(rhs)),
            _ => unreachable!(),
        };
    }
}

fn parse_term<'p>(mut tokens: &'p [Token<'p>]) -> Result<(&'p [Token<'p>], Node<Expr<'p>>)> {
    let mut value;
    (tokens, value) = parse_factor(tokens)?;
    loop {
        let (ty, src) = match tokens.get(0) {
            Some(Token { ty, src }) if [TokenType::Plus, TokenType::Minus].contains(ty) => {
                (ty, src)
            }
            _ => return Ok((tokens, value)),
        };
        tokens = &tokens[1..];
        let rhs;
        (tokens, rhs) = parse_factor(dbg!(tokens))?;
        value = match ty {
            TokenType::Plus => Expr::add(src, Box::new(value), Box::new(rhs)),
            TokenType::Minus => Expr::sub(src, Box::new(value), Box::new(rhs)),
            _ => unreachable!(),
        };
    }
}

fn parse<'p>(tokens: &'p Tokens<'p>) -> Result<Node<Expr<'p>>> {
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

    fn r<K>(expr: Node<K>) -> NodeRef<K> {
        Box::new(expr)
    }

    #[test]
    fn ident() -> Result<()> {
        assert_eq!(parse(&scan("abc")?)?, Expr::variable("abc", "abc"));
        Ok(())
    }

    #[test]
    fn unconsumed_tokens() -> Result<()> {
        assert!(parse(&scan("abc def")?).is_err());
        Ok(())
    }

    #[test]
    fn string() -> Result<()> {
        assert_eq!(parse(&scan("\"abc\" ")?)?, Expr::string("\"abc\"", "abc"));
        Ok(())
    }

    #[test]
    fn number() -> Result<()> {
        assert_eq!(parse(&scan(" 1.3")?)?, Expr::number("1.3", "1.3"));
        Ok(())
    }

    #[test]
    fn bool() -> Result<()> {
        assert_eq!(parse(&scan(" true ")?)?, Expr::boolean("true", true));
        assert_eq!(parse(&scan(" false")?)?, Expr::boolean("false", false));
        Ok(())
    }

    // TODO: This and following tests have incorrect spans
    #[test]
    fn not() -> Result<()> {
        assert_eq!(
            parse(&scan("!false")?)?,
            Expr::not("!", r(Expr::boolean("false", false)))
        );
        Ok(())
    }

    #[test]
    fn neg() -> Result<()> {
        assert_eq!(parse(&scan("-5")?)?, Expr::neg("-", r(Expr::number("5", "5"))));
        Ok(())
    }

    #[test]
    fn double_not() -> Result<()> {
        assert_eq!(parse(&scan("!!false")?)?, Expr::not("!", r(Expr::not("!", r(Expr::boolean("false", false))))));
        Ok(())
    }

    #[test]
    fn nil() -> Result<()> {
        assert_eq!(parse(&scan("nil")?)?, Expr::nil("nil"));
        Ok(())
    }

    #[test]
    fn mul() -> Result<()> {
        assert_eq!(parse(&scan("1 *2")?)?, Expr::mul("*", r(Expr::number("1", "1")), r(Expr::number("2", "2"))));
        Ok(())
    }

    #[test]
    fn sub() -> Result<()> {
        assert_eq!(parse(&scan("1 -2")?)?, Expr::sub("-", r(Expr::number("1", "1")), r(Expr::number("2", "2"))));
        Ok(())
    }

    #[test]
    fn unnecessary_parens() -> Result<()> {
        assert_eq!(
            parse(&scan("(1 *2)")?)?,
            Expr::mul("*", r(Expr::number("1", "1")), r(Expr::number("2", "2")))
        );
        Ok(())
    }

    #[test]
    fn parens() -> Result<()> {
        assert_eq!(
            parse(&scan("3*(1 +2)")?)?,
            Expr::mul("*", r(Expr::number("3", "3")), r(Expr::add("+", r(Expr::number("1", "1")), r(Expr::number("2", "2")))))
        );
        Ok(())
    }

    #[test]
    fn parens_nested() -> Result<()> {
        assert_eq!(
            parse(&scan("3*((1+2)+5)")?)?,
            Expr::mul("*",
                r(Expr::number("3", "3")),
                r(Expr::add("+", r(Expr::add("+", r(Expr::number("1", "1")), r(Expr::number("2", "2")))), r(Expr::number("5", "5"))))
            )
        );
        Ok(())
    }
}
