#![allow(unused_imports, dead_code)]
use crate::ast::parsed::{Declaration, Expr, Program, Stmt};
use crate::ast::{BinaryOp, LogicalOp, Node, NodeRef, UnaryOp};
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

    /// Peek at the next token, if it has the given token type.
    fn peek_token(&mut self, t: usize, exp_ty: TokenType) -> Option<Token> {
        match self.tokens.get(t) {
            Some(tok @ Token { ty, .. }) if *ty == exp_ty => Some(tok.clone()),
            _ => None,
        }
    }

    // --- parsers

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

    fn parse_logic_and(&mut self, t: usize) -> MultiResult<(usize, Node<Expr>)> {
        let (mut t, mut value) = self.parse_equality(t)?;
        loop {
            let Some(Token {
                ty: TokenType::And, ..
            }) = self.tokens.get(t)
            else {
                return Ok((t, value));
            };
            t += 1;
            let rhs;
            (t, rhs) = self.parse_equality(t)?;
            value = Expr::logop(
                value.src + rhs.src,
                LogicalOp::And,
                Box::new(value),
                Box::new(rhs),
            );
        }
    }

    fn parse_logic_or(&mut self, t: usize) -> MultiResult<(usize, Node<Expr>)> {
        let (mut t, mut value) = self.parse_logic_and(t)?;
        loop {
            let Some(Token {
                ty: TokenType::Or, ..
            }) = self.tokens.get(t)
            else {
                return Ok((t, value));
            };
            t += 1;
            let rhs;
            (t, rhs) = self.parse_logic_and(t)?;
            value = Expr::logop(
                value.src + rhs.src,
                LogicalOp::Or,
                Box::new(value),
                Box::new(rhs),
            );
        }
    }

    fn parse_assignment(&mut self, t: usize) -> MultiResult<(usize, Node<Expr>)> {
        let (mut t, lvalue) = self.parse_logic_or(t)?;
        if self.peek_token(t, TokenType::Equal).is_none() {
            // Not an assignment, so just return the equality.
            return Ok((t, lvalue));
        };
        t += 1;
        let Expr::Variable(name) = lvalue.inner else {
            self.errors.add(Error::syntax(
                format!("Assignment lvalue must be an identifier, got {:?}", lvalue),
                lvalue.src,
            ));
            // TODO: Synchronize to ;
            return self.parse_assignment(t);
        };
        let (t, rvalue) = self.parse_assignment(t)?;
        Ok((t, Expr::assignment(lvalue.src + rvalue.src, name, rvalue)))
    }

    fn parse_expression(&mut self, t: usize) -> MultiResult<(usize, Node<Expr>)> {
        self.parse_assignment(t)
    }

    fn parse_block(&mut self, mut t: usize) -> MultiResult<(usize, Node<Stmt>)> {
        let mut declarations = Vec::new();
        let offset = self.tokens.get(t).map(|tok| tok.src.offset).unwrap_or(0);
        let mut src = Src { offset, len: 0 };
        while self.peek_token(t, TokenType::RightBrace).is_none() {
            let decl;
            (t, decl) = self.parse_declaration(t)?;
            src += decl.src;
            declarations.push(decl);
        }
        if let (_, Some(rbrace_tok)) = self.consume_token(t, TokenType::RightBrace).unwrap() {
            t += 1;
            src += rbrace_tok.src;
        } else {
            self.errors
                .add(Error::syntax("Expected `}}`, got eof", src));
        }
        Ok((t, Stmt::block(src, declarations)))
    }

    fn parse_if(&mut self, t: usize) -> MultiResult<(usize, Node<Stmt>)> {
        let (_, Some(lparen_tok)) = self.consume_token(t, TokenType::LeftParen)? else {
            return self.unexpected_eof();
        };
        let mut src = lparen_tok.src;
        let (t, condition) = self.parse_parenthesized(t)?;

        let (t, Some(lbrace_tok)) = self.consume_token(t, TokenType::LeftBrace)? else {
            return self.unexpected_eof();
        };

        let (t, mut consequent) = self.parse_block(t)?;
        consequent.src += lbrace_tok.src;
        src += consequent.src;

        if self.peek_token(t, TokenType::Else).is_none() {
            // No alternate..
            return Ok((t, Stmt::conditional(src, condition, consequent, None)));
        };
        let t = t + 1;

        let (t, Some(lbrace_tok)) = self.consume_token(t, TokenType::LeftBrace)? else {
            return self.unexpected_eof();
        };

        let (t, mut alternate) = self.parse_block(t)?;
        alternate.src += lbrace_tok.src;
        src += alternate.src;

        Ok((
            t,
            Stmt::conditional(src, condition, consequent, Some(alternate.into())),
        ))
    }

    fn parse_while(&mut self, t: usize) -> MultiResult<(usize, Node<Stmt>)> {
        let (_, Some(lparen_tok)) = self.consume_token(t, TokenType::LeftParen)? else {
            return self.unexpected_eof();
        };
        let mut src = lparen_tok.src;
        let (t, precondition) = self.parse_parenthesized(t)?;

        let (t, Some(lbrace_tok)) = self.consume_token(t, TokenType::LeftBrace)? else {
            return self.unexpected_eof();
        };

        let (t, mut body) = self.parse_block(t)?;
        body.src += lbrace_tok.src;
        src += body.src;

        Ok((t, Stmt::r#while(src, precondition, body)))
    }

    fn parse_for(&mut self, t: usize) -> MultiResult<(usize, Node<Stmt>)> {
        let (t, Some(lparen_tok)) = self.consume_token(t, TokenType::LeftParen)? else {
            return self.unexpected_eof();
        };
        let mut src = lparen_tok.src;

        // Parse the initializer.
        let (t, init) = if self.peek_token(t, TokenType::Var).is_some() {
            let (t, decl) = self.parse_declaration(t)?;
            // parse_declaration consumes the `;`, but we check that below.
            (t - 1, Some(decl))
        } else if self.peek_token(t, TokenType::Semicolon).is_some() {
            (t, None)
        } else {
            let (t, expr) = self.parse_expression(t)?;
            let decl = Some(Declaration::stmt(expr.src, Stmt::expr(expr.src, expr)));
            (t, decl)
        };
        let init = init.map(Box::new);

        // Check for a semicolon.
        let (t, Some(_)) = self.consume_token(t, TokenType::Semicolon)? else {
            return self.unexpected_eof();
        };

        // Parse the condition.
        let (t, condition) = if self.peek_token(t, TokenType::Semicolon).is_some() {
            (t, None)
        } else {
            let (t, expr) = self.parse_expression(t)?;
            (t, Some(expr))
        };

        // Check for a semicolon.
        let (t, Some(_)) = self.consume_token(t, TokenType::Semicolon)? else {
            return self.unexpected_eof();
        };

        // Parse the increment.
        let (t, increment) = if self.peek_token(t, TokenType::RightParen).is_some() {
            (t, None)
        } else {
            let (t, expr) = self.parse_expression(t)?;
            (t, Some(expr))
        };

        // Check for right paren.
        let (t, Some(_)) = self.consume_token(t, TokenType::RightParen)? else {
            return self.unexpected_eof();
        };

        let (t, Some(lbrace_tok)) = self.consume_token(t, TokenType::LeftBrace)? else {
            return self.unexpected_eof();
        };

        // Parse the body.
        let (t, mut body) = self.parse_block(t)?;
        body.src += lbrace_tok.src;
        src += body.src;

        Ok((
            t,
            Stmt::r#for(src, init, condition, increment, body),
        ))
    }

    fn parse_statement(&mut self, t: usize) -> MultiResult<(usize, Node<Stmt>)> {
        let (mut t, mut stmt) = if let Some(tok) = self.peek_token(t, TokenType::Print) {
            let (t, expr) = self.parse_expression(t + 1)?;
            (t, Stmt::print(tok.src + expr.src, expr))
        } else if let Some(tok) = self.peek_token(t, TokenType::If) {
            let (t, mut stmt) = self.parse_if(t + 1)?;
            stmt.src += tok.src;
            return Ok((t, stmt));
        } else if let Some(tok) = self.peek_token(t, TokenType::While) {
            let (t, mut stmt) = self.parse_while(t + 1)?;
            stmt.src += tok.src;
            return Ok((t, stmt));
        } else if let Some(tok) = self.peek_token(t, TokenType::For) {
            let (t, mut stmt) = self.parse_for(t + 1)?;
            stmt.src += tok.src;
            return Ok((t, stmt));
        } else if let Some(tok) = self.peek_token(t, TokenType::LeftBrace) {
            let (t, mut stmt) = self.parse_block(t + 1)?;
            stmt.src += tok.src;
            return Ok((t, stmt));
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

        let (mut t, Some(ident_tok)) = self.consume_token(t, TokenType::Identifier)? else {
            // Recover by assuming this is the start of a declaration.
            return self.parse_declaration(t);
        };
        let ident_str: String = ident_tok.src_str(self.program).into();

        let Some(semi_or_eq_tok) = self.tokens.get(t) else {
            return self.unexpected_eof();
        };
        let expr = if semi_or_eq_tok.ty == TokenType::Equal {
            t += 1;
            let expr_parse = self.parse_expression(t)?;
            t = expr_parse.0;
            Some(expr_parse.1)
        } else {
            None
        };

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
    fn logical_and() -> MultiResult<()> {
        assert_eq!(
            parse("true and false;")?,
            prog(Expr::logop(
                s(0, 14),
                LogicalOp::And,
                Expr::boolean(s(0, 4), true),
                Expr::boolean(s(9, 5), false),
            ))
        );
        Ok(())
    }

    #[test]
    fn logical_or() -> MultiResult<()> {
        assert_eq!(
            parse("true or false;")?,
            prog(Expr::logop(
                s(0, 13),
                LogicalOp::Or,
                Expr::boolean(s(0, 4), true),
                Expr::boolean(s(8, 5), false),
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
    fn r#while() -> MultiResult<()> {
        assert_eq!(
            parse("while (true) { false; }")?,
            Program::new(
                s(0, 23),
                vec![Declaration::stmt(
                    s(0, 23),
                    Stmt::r#while(
                        s(0, 23),
                        Expr::boolean(s(6, 6), true),
                        Stmt::block(
                            s(13, 10),
                            vec![Declaration::stmt(
                                s(15, 6),
                                Stmt::expr(s(15, 6), Expr::boolean(s(15, 5), false))
                            )]
                        )
                    )
                )]
            )
        );
        Ok(())
    }

    #[test]
    fn r#for() -> MultiResult<()> {
        assert_eq!(
            parse("for (a; b; c) { false; }")?,
            Program::new(
                s(0, 24),
                vec![Declaration::stmt(
                    s(0, 24),
                    Stmt::r#for(
                        s(0, 24),
                        Some(
                            Declaration::stmt(
                                s(5, 1),
                                Stmt::expr(s(5, 1), Expr::variable(s(5, 1), "a"))
                            )
                            .into()
                        ),
                        Some(Expr::variable(s(8, 1), "b")),
                        Some(Expr::variable(s(11, 1), "c").into()),
                        Stmt::block(
                            s(14, 10),
                            vec![Declaration::stmt(
                                s(16, 6),
                                Stmt::expr(s(16, 6), Expr::boolean(s(16, 5), false))
                            )]
                        )
                    )
                )]
            )
        );
        Ok(())
    }

    #[test]
    fn for_empty() -> MultiResult<()> {
        assert_eq!(
            parse("for ( ;  ;  ) { false; }")?,
            Program::new(
                s(0, 24),
                vec![Declaration::stmt(
                    s(0, 24),
                    Stmt::r#for(
                        s(0, 24),
                        None,
                        None,
                        None,
                        Stmt::block(
                            s(14, 10),
                            vec![Declaration::stmt(
                                s(16, 6),
                                Stmt::expr(s(16, 6), Expr::boolean(s(16, 5), false))
                            )]
                        )
                    )
                )]
            )
        );
        Ok(())
    }

    #[test]
    fn for_decl() -> MultiResult<()> {
        assert_eq!(
            parse("for (var i = 0;;) { }")?,
            Program::new(
                s(0, 21),
                vec![Declaration::stmt(
                    s(0, 21),
                    Stmt::r#for(
                        s(0, 21),
                        Some(
                            Declaration::vardecl(s(5, 10), "i", Expr::number(s(13, 1), "0")).into()
                        ),
                        None,
                        None,
                        Stmt::block(s(18, 3), vec![])
                    )
                )]
            )
        );
        Ok(())
    }

    #[test]
    fn var_decl_nil() -> MultiResult<()> {
        assert_eq!(
            parse("var x;")?,
            Program::new(s(0, 6), vec![Declaration::vardecl(s(0, 6), "x", None,)])
        );
        Ok(())
    }

    #[test]
    fn block() -> MultiResult<()> {
        assert_eq!(
            parse("1; {2; 3;} 4;")?,
            Program::new(
                s(0, 13),
                vec![
                    Declaration::stmt(s(0, 2), Stmt::expr(s(0, 2), Expr::number(s(0, 1), "1"))),
                    Declaration::stmt(
                        s(3, 7),
                        Stmt::block(
                            s(3, 7),
                            [
                                Declaration::stmt(
                                    s(4, 2),
                                    Stmt::expr(s(4, 2), Expr::number(s(4, 1), "2"))
                                ),
                                Declaration::stmt(
                                    s(7, 2),
                                    Stmt::expr(s(7, 2), Expr::number(s(7, 1), "3"))
                                ),
                            ]
                        )
                    ),
                    Declaration::stmt(s(11, 2), Stmt::expr(s(11, 2), Expr::number(s(11, 1), "4"))),
                ]
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
