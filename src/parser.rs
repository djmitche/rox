#![allow(unused_imports, dead_code)]
use crate::ast::{Expr, Node, NodeRef};
use crate::scanner::scan;
use crate::token::{Token, TokenType, Tokens};
use anyhow::Result;

struct Parser<'p> {
    program: &'p str,
    tokens: &'p [Token],
}

impl<'p> Parser<'p> {
    // Each parsing function returns (remainder, value) where `remainder` is the remaining token
    // stream.

    /// parse a primary (one-token) expression, or a parethesized expression.
    fn parse_primary(&self, t: usize) -> Result<(usize, Node<Expr>)> {
        let Some(tok) = self.tokens.get(t) else {
            anyhow::bail!("Expected primary, got EOF");
        };
        fn strip_quotes(quoted: &str) -> &str {
            &quoted[1..quoted.len() - 1]
        }
        Ok((
            t + 1,
            match tok.ty {
                TokenType::Identifier => Expr::variable(tok.src, tok.src_str(self.program)),
                TokenType::String => Expr::string(tok.src, strip_quotes(tok.src_str(self.program))),
                TokenType::Number => Expr::number(tok.src, tok.src_str(self.program)),
                TokenType::True => Expr::boolean(tok.src, true),
                TokenType::False => Expr::boolean(tok.src, false),
                TokenType::Nil => Expr::nil(tok.src),
                TokenType::LeftParen => return self.parse_parenthesized(t + 1),
                _ => anyhow::bail!("Expected primary, got {0:?}", tok.ty),
            },
        ))
    }

    /// Parse a parenthesized expression, where `tokens` begins after the opening parenthesis.
    fn parse_parenthesized(&self, t: usize) -> Result<(usize, Node<Expr>)> {
        let (t, value) = self.parse_term(t)?;
        let Some(Token { ty, .. }) = self.tokens.get(t) else {
            anyhow::bail!("Unexpected EOF");
        };
        if *ty != TokenType::RightParen {
            anyhow::bail!("Expected RParen, got {0:?}", ty);
        }
        Ok((t + 1, value))
    }

    fn parse_unary(&self, t: usize) -> Result<(usize, Node<Expr>)> {
        let (ty, src) = match self.tokens.get(t) {
            Some(Token { ty, src }) if [TokenType::Bang, TokenType::Minus].contains(ty) => {
                (ty, *src)
            }
            _ => return self.parse_primary(t),
        };
        // Parse the unary expression following this operator.
        let (t, inner) = self.parse_unary(t + 1)?;
        let result = match ty {
            TokenType::Bang => Expr::not(src + inner.src, inner),
            TokenType::Minus => Expr::neg(src + inner.src, inner),
            _ => unreachable!(),
        };
        Ok((t, result))
    }

    fn parse_factor(&self, t: usize) -> Result<(usize, Node<Expr>)> {
        let (mut t, mut value) = self.parse_unary(t)?;
        loop {
            let ty = match self.tokens.get(t) {
                Some(Token { ty, .. }) if [TokenType::Star, TokenType::Slash].contains(ty) => ty,
                _ => return Ok((t, value)),
            };
            t += 1;
            let rhs;
            (t, rhs) = self.parse_unary(t)?;
            value = match ty {
                TokenType::Star => Expr::mul(value.src + rhs.src, Box::new(value), Box::new(rhs)),
                TokenType::Slash => Expr::div(value.src + rhs.src, Box::new(value), Box::new(rhs)),
                _ => unreachable!(),
            };
        }
    }

    fn parse_term(&self, t: usize) -> Result<(usize, Node<Expr>)> {
        let (mut t, mut value) = self.parse_factor(t)?;
        loop {
            let ty = match self.tokens.get(t) {
                Some(Token { ty, .. }) if [TokenType::Plus, TokenType::Minus].contains(ty) => ty,
                _ => return Ok((t, value)),
            };
            t += 1;
            let rhs;
            (t, rhs) = self.parse_factor(t)?;
            value = match ty {
                TokenType::Plus => Expr::add(value.src + rhs.src, Box::new(value), Box::new(rhs)),
                TokenType::Minus => Expr::sub(value.src + rhs.src, Box::new(value), Box::new(rhs)),
                _ => unreachable!(),
            };
        }
    }
}

fn parse(program: &str) -> Result<Node<Expr>> {
    let tokens = scan(program)?;
    let parser = Parser {
        tokens: &tokens[..],
        program,
    };
    let (remainder, result) = parser.parse_term(0)?;
    if remainder < tokens.len() {
        anyhow::bail!("Extra tokens at end");
    }
    Ok(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::src::Src;

    /// Shortcut to create a Src.
    fn s(offset: usize, len: usize) -> Src {
        Src { offset, len }
    }

    #[test]
    fn ident() -> Result<()> {
        assert_eq!(parse("abc")?, Expr::variable(s(0, 3), "abc"));
        Ok(())
    }

    #[test]
    fn unconsumed_tokens() -> Result<()> {
        assert!(parse("abc def").is_err());
        Ok(())
    }

    #[test]
    fn string() -> Result<()> {
        assert_eq!(parse("\"abc\" ")?, Expr::string(s(0, 5), "abc"));
        Ok(())
    }

    #[test]
    fn number() -> Result<()> {
        assert_eq!(parse(" 1.3")?, Expr::number(s(1, 3), "1.3"));
        Ok(())
    }

    #[test]
    fn bool() -> Result<()> {
        assert_eq!(parse(" true ")?, Expr::boolean(s(1, 4), true));
        assert_eq!(parse(" false")?, Expr::boolean(s(1, 5), false));
        Ok(())
    }

    #[test]
    fn not() -> Result<()> {
        assert_eq!(
            parse("!false")?,
            Expr::not(s(0, 6), Expr::boolean(s(1, 5), false))
        );
        Ok(())
    }

    #[test]
    fn neg() -> Result<()> {
        assert_eq!(parse("-5")?, Expr::neg(s(0, 2), Expr::number(s(1, 1), "5")));
        Ok(())
    }

    #[test]
    fn double_not() -> Result<()> {
        assert_eq!(
            parse("!!false")?,
            Expr::not(s(0, 7), Expr::not(s(1, 6), Expr::boolean(s(2, 5), false)))
        );
        Ok(())
    }

    #[test]
    fn nil() -> Result<()> {
        assert_eq!(parse("nil")?, Expr::nil(s(0, 3)));
        Ok(())
    }

    #[test]
    fn mul() -> Result<()> {
        assert_eq!(
            parse("1 *2")?,
            Expr::mul(
                s(0, 4),
                Expr::number(s(0, 1), "1"),
                Expr::number(s(3, 1), "2")
            )
        );
        Ok(())
    }

    #[test]
    fn sub() -> Result<()> {
        assert_eq!(
            parse("1 -2")?,
            Expr::sub(
                s(0, 4),
                Expr::number(s(0, 1), "1"),
                Expr::number(s(3, 1), "2")
            )
        );
        Ok(())
    }

    #[test]
    fn unnecessary_parens() -> Result<()> {
        assert_eq!(
            parse("(1 *2)")?,
            Expr::mul(
                s(1, 4),
                Expr::number(s(1, 1), "1"),
                Expr::number(s(4, 1), "2")
            )
        );
        Ok(())
    }

    #[test]
    fn parens() -> Result<()> {
        assert_eq!(
            parse("3*(1 +2)")?,
            Expr::mul(
                s(0, 7),
                Expr::number(s(0, 1), "3"),
                Expr::add(
                    s(3, 4),
                    Expr::number(s(3, 1), "1"),
                    Expr::number(s(6, 1), "2")
                )
            )
        );
        Ok(())
    }

    #[test]
    fn parens_nested() -> Result<()> {
        assert_eq!(
            parse("3*((1+2)+5)")?,
            Expr::mul(
                s(0, 10),
                Expr::number(s(0, 1), "3"),
                Expr::add(
                    s(4, 6),
                    Expr::add(
                        s(4, 3),
                        Expr::number(s(4, 1), "1"),
                        Expr::number(s(6, 1), "2")
                    ),
                    Expr::number(s(9, 1), "5")
                )
            )
        );
        Ok(())
    }
}
