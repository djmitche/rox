#![allow(unused_imports, dead_code)]
use crate::ast::{BinaryOp, Expr, Node, NodeRef, Program, Stmt, UnaryOp};
use crate::error::{Error, Errors, MultiResult, Result};
use crate::scanner::scan;
use crate::src::Src;
use crate::token::{Token, TokenType, Tokens};

struct Parser<'p> {
    program: &'p str,
    tokens: &'p [Token],
    errors: Errors,
}

impl<'p> Parser<'p> {
    // Each parsing function returns (remainder, value) where `remainder` is the index of the
    // remaining token stream.
    fn unexpected_eof<T>(&mut self) -> MultiResult<T> {
        self.errors.add(Error::syntax(
            "Unexpected EOF",
            Src {
                offset: self.program.len(),
                len: 0,
            },
        ));
        Err(std::mem::take(&mut self.errors))
    }

    /// parse a primary (one-token) expression, or a parethesized expression.
    fn parse_primary(&mut self, t: usize) -> MultiResult<(usize, Node<Expr>)> {
        let Some(tok) = self.tokens.get(t) else {
            return self.unexpected_eof();
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
                TokenType::LeftParen => return self.parse_parenthesized(t),
                _ => {
                    self.errors.add(Error::syntax(
                        format!("Unexpected token {:?}", tok.ty),
                        tok.src,
                    ));
                    // Synchronize by trying the next token
                    return self.parse_primary(t + 1);
                }
            },
        ))
    }

    /// Parse a parenthesized expression, where tokens[t] is LeftParen.
    fn parse_parenthesized(&mut self, mut t: usize) -> MultiResult<(usize, Node<Expr>)> {
        let mut src = self.tokens[t].src;
        t += 1;
        let (mut t, mut value) = self.parse_expression(t)?;
        src += value.src;
        let Some(tok) = self.tokens.get(t) else {
            return self.unexpected_eof();
        };
        if tok.ty == TokenType::RightParen {
            t += 1;
            src += tok.src;
        } else {
            self.errors.add(Error::syntax(
                format!("Expected `)`, got {:?}", tok.ty),
                tok.src,
            ));
            // Synchronize by assuming the `)` was present.
        }
        // Expand the span of src to include the parentheses.
        value.src = src;
        Ok((t, value))
    }

    fn parse_unary(&mut self, t: usize) -> MultiResult<(usize, Node<Expr>)> {
        let (ty, src) = match self.tokens.get(t) {
            Some(Token { ty, src }) if [TokenType::Bang, TokenType::Minus].contains(ty) => {
                (ty, *src)
            }
            _ => return self.parse_primary(t),
        };
        // Parse the unary expression following this operator.
        let (t, inner) = self.parse_unary(t + 1)?;
        let result = Expr::unary_op(UnaryOp::try_from(*ty).unwrap(), src + inner.src, inner);
        Ok((t, result))
    }

    fn parse_factor(&mut self, t: usize) -> MultiResult<(usize, Node<Expr>)> {
        let (mut t, mut value) = self.parse_unary(t)?;
        loop {
            let ty = match self.tokens.get(t) {
                Some(Token { ty, .. }) if [TokenType::Star, TokenType::Slash].contains(ty) => ty,
                _ => return Ok((t, value)),
            };
            t += 1;
            let rhs;
            (t, rhs) = self.parse_unary(t)?;
            value = Expr::binary_op(
                BinaryOp::try_from(*ty).unwrap(),
                value.src + rhs.src,
                Box::new(value),
                Box::new(rhs),
            );
        }
    }

    fn parse_term(&mut self, t: usize) -> MultiResult<(usize, Node<Expr>)> {
        let (mut t, mut value) = self.parse_factor(t)?;
        loop {
            let ty = match self.tokens.get(t) {
                Some(Token { ty, .. }) if [TokenType::Plus, TokenType::Minus].contains(ty) => ty,
                _ => return Ok((t, value)),
            };
            t += 1;
            let rhs;
            (t, rhs) = self.parse_factor(t)?;
            value = Expr::binary_op(
                BinaryOp::try_from(*ty).unwrap(),
                value.src + rhs.src,
                Box::new(value),
                Box::new(rhs),
            );
        }
    }

    fn parse_comparison(&mut self, t: usize) -> MultiResult<(usize, Node<Expr>)> {
        let (mut t, mut value) = self.parse_term(t)?;
        loop {
            let types = [
                TokenType::Less,
                TokenType::LessEqual,
                TokenType::GreaterEqual,
                TokenType::Greater,
            ];
            let ty = match self.tokens.get(t) {
                Some(Token { ty, .. }) if types.contains(ty) => ty,
                _ => return Ok((t, value)),
            };
            t += 1;
            let rhs;
            (t, rhs) = self.parse_term(t)?;
            value = Expr::binary_op(
                BinaryOp::try_from(*ty).unwrap(),
                value.src + rhs.src,
                Box::new(value),
                Box::new(rhs),
            );
        }
    }

    fn parse_equality(&mut self, t: usize) -> MultiResult<(usize, Node<Expr>)> {
        let (mut t, mut value) = self.parse_comparison(t)?;
        loop {
            let types = [TokenType::BangEqual, TokenType::EqualEqual];
            let ty = match self.tokens.get(t) {
                Some(Token { ty, .. }) if types.contains(ty) => ty,
                _ => return Ok((t, value)),
            };
            t += 1;
            let rhs;
            (t, rhs) = self.parse_comparison(t)?;
            value = Expr::binary_op(
                BinaryOp::try_from(*ty).unwrap(),
                value.src + rhs.src,
                Box::new(value),
                Box::new(rhs),
            );
        }
    }

    fn parse_expression(&mut self, t: usize) -> MultiResult<(usize, Node<Expr>)> {
        self.parse_equality(t)
    }

    fn parse_statement(&mut self, t: usize) -> MultiResult<(usize, Node<Stmt>)> {
        let (mut t, mut stmt) = if let Some(Token {
            ty: TokenType::Print,
            src,
        }) = self.tokens.get(t)
        {
            let (t, expr) = self.parse_expression(t + 1)?;
            (t, Stmt::print(*src + expr.src, expr))
        } else {
            let (t, expr) = self.parse_expression(t)?;
            (t, Stmt::expr(expr.src, expr))
        };
        let Some(tok) = self.tokens.get(t) else {
            return self.unexpected_eof();
        };
        if tok.ty == TokenType::Semicolon {
            t += 1;
            stmt.src += tok.src;
        } else {
            self.errors.add(Error::syntax(
                format!("Expected `;`, got {:?}", tok.ty),
                tok.src,
            ));
            // Synchronize by assuming the `;` was present.
        }
        Ok((t, stmt))
    }

    fn parse_program(&mut self, mut t: usize) -> MultiResult<(usize, Node<Program>)> {
        let mut statements = Vec::new();
        let offset = self.tokens.get(t).map(|tok| tok.src.offset).unwrap_or(0);
        let mut src = Src { offset, len: 0 };
        while t < self.tokens.len() {
            let stmt;
            (t, stmt) = self.parse_statement(t)?;
            src = src + stmt.src;
            statements.push(stmt);
        }
        Ok((t, Program::stmts(src, statements)))
    }
}

pub fn parse(program: &str) -> MultiResult<Node<Program>> {
    let tokens = scan(program)?;
    let mut parser = Parser {
        tokens: &tokens[..],
        program,
        errors: Errors::new("parser"),
    };
    let (remainder, result) = parser.parse_program(0)?;
    if remainder < tokens.len() {
        let tok = &tokens[remainder];
        parser.errors.add(Error::syntax(
            format!("Unexpected token after program: {:?}", tok.ty),
            tok.src,
        ));
    }
    if !parser.errors.is_empty() {
        return Err(parser.errors);
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

    /// Shortcut for a program containing a single expression statement.
    fn prog(expr: Node<Expr>) -> Node<Program> {
        let mut src = expr.src;
        src.len += 1; // Account for trailing semicolon.
        let stmt = Stmt::expr(src, expr);
        Program::stmts(src, vec![stmt])
    }

    #[test]
    fn ident() -> MultiResult<()> {
        assert_eq!(parse("abc;")?, prog(Expr::variable(s(0, 3), "abc")));
        Ok(())
    }

    #[test]
    fn unconsumed_tokens() -> MultiResult<()> {
        assert!(parse("abc def").is_err());
        Ok(())
    }

    #[test]
    fn string() -> MultiResult<()> {
        assert_eq!(parse("\"abc\"; ")?, prog(Expr::string(s(0, 5), "abc")));
        Ok(())
    }

    #[test]
    fn number() -> MultiResult<()> {
        assert_eq!(parse(" 1.3;")?, prog(Expr::number(s(1, 3), "1.3")));
        Ok(())
    }

    #[test]
    fn bool() -> MultiResult<()> {
        assert_eq!(parse(" true;")?, prog(Expr::boolean(s(1, 4), true)));
        assert_eq!(parse(" false;")?, prog(Expr::boolean(s(1, 5), false)));
        Ok(())
    }

    #[test]
    fn not() -> MultiResult<()> {
        assert_eq!(
            parse("!false;")?,
            prog(Expr::unary_op(
                UnaryOp::Not,
                s(0, 6),
                Expr::boolean(s(1, 5), false)
            ))
        );
        Ok(())
    }

    #[test]
    fn neg() -> MultiResult<()> {
        assert_eq!(
            parse("-5;")?,
            prog(Expr::unary_op(
                UnaryOp::Neg,
                s(0, 2),
                Expr::number(s(1, 1), "5")
            ))
        );
        Ok(())
    }

    #[test]
    fn double_not() -> MultiResult<()> {
        assert_eq!(
            parse("!!false;")?,
            prog(Expr::unary_op(
                UnaryOp::Not,
                s(0, 7),
                Expr::unary_op(UnaryOp::Not, s(1, 6), Expr::boolean(s(2, 5), false))
            ))
        );
        Ok(())
    }

    #[test]
    fn nil() -> MultiResult<()> {
        assert_eq!(parse("nil;")?, prog(Expr::nil(s(0, 3))));
        Ok(())
    }

    #[test]
    fn mul() -> MultiResult<()> {
        assert_eq!(
            parse("1 *2;")?,
            prog(Expr::binary_op(
                BinaryOp::Mul,
                s(0, 4),
                Expr::number(s(0, 1), "1"),
                Expr::number(s(3, 1), "2")
            ))
        );
        Ok(())
    }

    #[test]
    fn sub() -> MultiResult<()> {
        assert_eq!(
            parse("1 -2;")?,
            prog(Expr::binary_op(
                BinaryOp::Sub,
                s(0, 4),
                Expr::number(s(0, 1), "1"),
                Expr::number(s(3, 1), "2")
            ))
        );
        Ok(())
    }

    #[test]
    fn unnecessary_parens() -> MultiResult<()> {
        assert_eq!(
            parse("(1 *2);")?,
            Program::stmts(
                s(0, 7),
                vec![Stmt::expr(
                    s(0, 7),
                    Expr::binary_op(
                        BinaryOp::Mul,
                        s(0, 6),
                        Expr::number(s(1, 1), "1"),
                        Expr::number(s(4, 1), "2")
                    )
                )]
            )
        );
        Ok(())
    }

    #[test]
    fn parens() -> MultiResult<()> {
        assert_eq!(
            parse("3*(1 +2);")?,
            prog(Expr::binary_op(
                BinaryOp::Mul,
                s(0, 8),
                Expr::number(s(0, 1), "3"),
                Expr::binary_op(
                    BinaryOp::Add,
                    s(2, 6),
                    Expr::number(s(3, 1), "1"),
                    Expr::number(s(6, 1), "2")
                )
            ))
        );
        Ok(())
    }

    #[test]
    fn parens_nested() -> MultiResult<()> {
        assert_eq!(
            parse("3*((1+2)+5);")?,
            Program::stmts(
                s(0, 12),
                vec![Stmt::expr(
                    s(0, 12),
                    Expr::binary_op(
                        BinaryOp::Mul,
                        s(0, 11),
                        Expr::number(s(0, 1), "3"),
                        Expr::binary_op(
                            BinaryOp::Add,
                            s(2, 9),
                            Expr::binary_op(
                                BinaryOp::Add,
                                s(3, 5),
                                Expr::number(s(4, 1), "1"),
                                Expr::number(s(6, 1), "2")
                            ),
                            Expr::number(s(9, 1), "5")
                        )
                    )
                )]
            )
        );
        Ok(())
    }

    #[test]
    fn precedence() -> MultiResult<()> {
        assert_eq!(
            parse("1*2+3<4==5;")?,
            prog(Expr::binary_op(
                BinaryOp::Equal,
                s(0, 10),
                Expr::binary_op(
                    BinaryOp::Less,
                    s(0, 7),
                    Expr::binary_op(
                        BinaryOp::Add,
                        s(0, 5),
                        Expr::binary_op(
                            BinaryOp::Mul,
                            s(0, 3),
                            Expr::number(s(0, 1), "1"),
                            Expr::number(s(2, 1), "2")
                        ),
                        Expr::number(s(4, 1), "3"),
                    ),
                    Expr::number(s(6, 1), "4")
                ),
                Expr::number(s(9, 1), "5"),
            ))
        );
        Ok(())
    }

    #[test]
    fn program() -> MultiResult<()> {
        assert_eq!(
            parse("3; print 4;")?,
            Program::stmts(s(0, 11), vec![
            Stmt::expr(s(0, 2), Expr::number(s(0, 1), "3")),
            Stmt::print(s(3, 8), Expr::number(s(9, 1), "4")),
        ]));
        Ok(())
    }
}
