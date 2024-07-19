#![allow(unused_imports, dead_code)]
use crate::ast::parsed::{Declaration, Expr, Program, Stmt};
use crate::ast::{BinaryOp, Node, NodeRef, UnaryOp};
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

    /// Consume a single token of the given type or error out.
    fn consume_token(
        &mut self,
        t: usize,
        exp_ty: TokenType,
    ) -> MultiResult<(usize, Option<Token>)> {
        match self.tokens.get(t) {
            Some(tok) if tok.ty == exp_ty => Ok((t + 1, Some(tok.clone()))),
            Some(tok) => {
                self.errors.add(Error::syntax(
                    format!("Expected {:?}, got {:?}", exp_ty, tok),
                    tok.src,
                ));
                Ok((t + 1, None))
            }
            None => self.unexpected_eof(),
        }
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
        let result = Expr::unary(src + inner.src, UnaryOp::try_from(*ty).unwrap(), inner);
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
            value = Expr::binop(
                value.src + rhs.src,
                BinaryOp::try_from(*ty).unwrap(),
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
            value = Expr::binop(
                value.src + rhs.src,
                BinaryOp::try_from(*ty).unwrap(),
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
            value = Expr::binop(
                value.src + rhs.src,
                BinaryOp::try_from(*ty).unwrap(),
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
            value = Expr::binop(
                value.src + rhs.src,
                BinaryOp::try_from(*ty).unwrap(),
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

    fn parse_declaration(&mut self, mut t: usize) -> MultiResult<(usize, Node<Declaration>)> {
        let Some(Token {
            ty: TokenType::Var,
            src: var_src,
        }) = self.tokens.get(t)
        else {
            let (t, stmt) = self.parse_statement(t)?;
            return Ok((t, Declaration::stmt(stmt.src, stmt)));
        };
        t += 1;

        let (t, Some(ident_tok)) = self.consume_token(t, TokenType::Identifier)? else {
            // Recover by assuming this is the start of a declaration.
            // TODO: scan for `;`
            return self.parse_declaration(t);
        };
        let ident_str: String = ident_tok.src_str(self.program).into();

        // TODO: ( = <expr> ) should be optional.
        let (t, Some(_)) = self.consume_token(t, TokenType::Equal)? else {
            // Recover by assuming this is the start of a declaration.
            // TODO: scan for `;`
            return self.parse_declaration(t);
        };
        let (mut t, expr) = self.parse_expression(t)?;

        let Some(semi_tok) = self.tokens.get(t) else {
            return self.unexpected_eof();
        };
        if semi_tok.ty == TokenType::Semicolon {
            t += 1;
        } else {
            self.errors.add(Error::syntax(
                format!("Expected `;`, got {:?}", semi_tok.ty),
                semi_tok.src,
            ));
            // Synchronize by assuming the `;` was present.
        }
        Ok((
            t,
            Declaration::vardecl(*var_src + semi_tok.src, ident_str, expr),
        ))
    }

    fn parse_program(&mut self, mut t: usize) -> MultiResult<(usize, Node<Program>)> {
        let mut declarations = Vec::new();
        let offset = self.tokens.get(t).map(|tok| tok.src.offset).unwrap_or(0);
        let mut src = Src { offset, len: 0 };
        while t < self.tokens.len() {
            let decl;
            (t, decl) = self.parse_declaration(t)?;
            src += decl.src;
            declarations.push(decl);
        }
        Ok((t, Program::new(src, declarations)))
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
        let decl = Declaration::stmt(src, stmt);
        Program::new(src, vec![decl])
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
            prog(Expr::unary(
                s(0, 6),
                UnaryOp::Not,
                Expr::boolean(s(1, 5), false)
            ))
        );
        Ok(())
    }

    #[test]
    fn neg() -> MultiResult<()> {
        assert_eq!(
            parse("-5;")?,
            prog(Expr::unary(
                s(0, 2),
                UnaryOp::Neg,
                Expr::number(s(1, 1), "5")
            ))
        );
        Ok(())
    }

    #[test]
    fn double_not() -> MultiResult<()> {
        assert_eq!(
            parse("!!false;")?,
            prog(Expr::unary(
                s(0, 7),
                UnaryOp::Not,
                Expr::unary(s(1, 6), UnaryOp::Not, Expr::boolean(s(2, 5), false))
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
            prog(Expr::binop(
                s(0, 4),
                BinaryOp::Mul,
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
            prog(Expr::binop(
                s(0, 4),
                BinaryOp::Sub,
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
            Program::new(
                s(0, 7),
                vec![Declaration::stmt(
                    s(0, 7),
                    Stmt::expr(
                        s(0, 7),
                        Expr::binop(
                            s(0, 6),
                            BinaryOp::Mul,
                            Expr::number(s(1, 1), "1"),
                            Expr::number(s(4, 1), "2")
                        )
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
            prog(Expr::binop(
                s(0, 8),
                BinaryOp::Mul,
                Expr::number(s(0, 1), "3"),
                Expr::binop(
                    s(2, 6),
                    BinaryOp::Add,
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
            Program::new(
                s(0, 12),
                vec![Declaration::stmt(
                    s(0, 12),
                    Stmt::expr(
                        s(0, 12),
                        Expr::binop(
                            s(0, 11),
                            BinaryOp::Mul,
                            Expr::number(s(0, 1), "3"),
                            Expr::binop(
                                s(2, 9),
                                BinaryOp::Add,
                                Expr::binop(
                                    s(3, 5),
                                    BinaryOp::Add,
                                    Expr::number(s(4, 1), "1"),
                                    Expr::number(s(6, 1), "2")
                                ),
                                Expr::number(s(9, 1), "5")
                            )
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
            prog(Expr::binop(
                s(0, 10),
                BinaryOp::Equal,
                Expr::binop(
                    s(0, 7),
                    BinaryOp::Less,
                    Expr::binop(
                        s(0, 5),
                        BinaryOp::Add,
                        Expr::binop(
                            s(0, 3),
                            BinaryOp::Mul,
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
    fn var_decl() -> MultiResult<()> {
        assert_eq!(
            parse("var x = 4;")?,
            Program::new(
                s(0, 10),
                vec![Declaration::vardecl(
                    s(0, 10),
                    "x",
                    Expr::number(s(8, 1), "4")
                )]
            )
        );
        Ok(())
    }

    #[test]
    fn program() -> MultiResult<()> {
        assert_eq!(
            parse("3; var y = 1; print 4;")?,
            Program::new(
                s(0, 22),
                vec![
                    Declaration::stmt(s(0, 2), Stmt::expr(s(0, 2), Expr::number(s(0, 1), "3"))),
                    Declaration::vardecl(s(3, 10), "y", Expr::number(s(11, 1), "1")),
                    Declaration::stmt(s(14, 8), Stmt::print(s(14, 8), Expr::number(s(20, 1), "4"))),
                ]
            )
        );
        Ok(())
    }
}
