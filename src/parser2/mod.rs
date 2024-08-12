#![allow(unused_imports, dead_code)]

#[macro_use]
mod recog;
mod types;

use recog::*;
use types::*;

use crate::ast::parsed::{Declaration, Expr, Program, Stmt};
use crate::ast::{BinaryOp, LogicalOp, Node, NodeRef, UnaryOp};
use crate::error::{Error, Errors, MultiResult, Result};
use crate::scanner::scan;
use crate::src::Src;
use crate::token::{Token, TokenType, Tokens};

use ParseResult::*;

// TODO: catching and re-syncing after error?

fn parse_primary(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    fn strip_quotes(quoted: &str) -> &str {
        &quoted[1..quoted.len() - 1]
    }
    first_success!(
        recognize_token(TokenType::Identifier)
            .map(|tok| Expr::variable(tok.src, tok.src_str(parser.program))),
        recognize_token(TokenType::String)
            .map(|tok| Expr::string(tok.src, strip_quotes(tok.src_str(parser.program)))),
        recognize_token(TokenType::Number)
            .map(|tok| Expr::number(tok.src, tok.src_str(parser.program))),
        recognize_token(TokenType::True).map(|tok| Expr::boolean(tok.src, true)),
        recognize_token(TokenType::False).map(|tok| Expr::boolean(tok.src, false)),
        recognize_token(TokenType::Nil).map(|tok| Expr::nil(tok.src)),
        recognize_token(TokenType::LeftParen)
            .then(recognizer(parse_expression))
            .then(recognize_token(TokenType::RightParen))
            .map(|((l, e), r)| e.with_src(l.src + r.src))
    )
    .recognize(parser, t)
}

fn parse_unary(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    first_success!(
        recognize_token(TokenType::Minus)
            .then(recognizer(parse_unary))
            .map(|(tok, e)| Expr::unary(tok.src + e.src, UnaryOp::Neg, e)),
        recognize_token(TokenType::Bang)
            .then(recognizer(parse_unary))
            .map(|(tok, e)| Expr::unary(tok.src + e.src, UnaryOp::Not, e)),
        recognizer(parse_primary),
    )
    .recognize(parser, t)
}

fn fold_binop(lhs: Node<Expr>, op: Token, rhs: Node<Expr>) -> Node<Expr> {
    Expr::binop(
        lhs.src + rhs.src,
        BinaryOp::try_from(op.ty).unwrap(),
        Box::new(lhs),
        Box::new(rhs),
    )
}

fn parse_factor(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    left_assoc(
        recognizer(parse_unary),
        recognize_token(TokenType::Star).or(recognize_token(TokenType::Slash)),
        fold_binop,
    )
    .recognize(parser, t)
}

fn parse_term(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    left_assoc(
        recognizer(parse_factor),
        recognize_token(TokenType::Plus).or(recognize_token(TokenType::Minus)),
        fold_binop,
    )
    .recognize(parser, t)
}

fn parse_comparison(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    left_assoc(
        recognizer(parse_term),
        recognize_token(TokenType::Less)
            .or(recognize_token(TokenType::LessEqual))
            .or(recognize_token(TokenType::GreaterEqual))
            .or(recognize_token(TokenType::Greater)),
        fold_binop,
    )
    .recognize(parser, t)
}

fn parse_equality(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    left_assoc(
        recognizer(parse_comparison),
        recognize_token(TokenType::BangEqual).or(recognize_token(TokenType::EqualEqual)),
        fold_binop,
    )
    .recognize(parser, t)
}

fn parse_logic_and(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    left_assoc(
        recognizer(parse_equality),
        recognize_token(TokenType::And),
        |lhs, _, rhs| {
            Expr::logop(
                lhs.src + rhs.src,
                LogicalOp::And,
                Box::new(lhs),
                Box::new(rhs),
            )
        },
    )
    .recognize(parser, t)
}

fn parse_logic_or(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    left_assoc(
        recognizer(parse_logic_and),
        recognize_token(TokenType::Or),
        |lhs, _, rhs| {
            Expr::logop(
                lhs.src + rhs.src,
                LogicalOp::Or,
                Box::new(lhs),
                Box::new(rhs),
            )
        },
    )
    .recognize(parser, t)
}

fn parse_assignment(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    first_success!(
        // TODO: support parsing arbitrary lvalue expresions
        recognize_token(TokenType::Identifier)
            .then(recognize_token(TokenType::Equal))
            .then(recognizer(parse_assignment))
            .map(|((ident, _), expr)| {
                Expr::assignment(ident.src + expr.src, ident.src_str(parser.program), expr)
            }),
        recognizer(parse_logic_or),
    )
    .recognize(parser, t)
}

fn parse_expression(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    parse_assignment(parser, t)
}

fn parse_declarations(
    parser: &mut Parser,
    mut t: usize,
) -> ParseResult<(Src, Vec<Node<Declaration>>)> {
    let mut declarations = Vec::new();
    while let Success(decl_t, decl) = dbg!(parse_declaration(parser, t)) {
        declarations.push(decl);
        t = decl_t;
    }
    let src = declarations
        .iter()
        .map(|d| d.src)
        .reduce(|a, b| a + b)
        .unwrap_or_else(Src::default);
    Success(t, (src, declarations))
}

fn paren_expr(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    recognize_token(TokenType::LeftParen)
        .then(recognizer(parse_expression))
        .then(recognize_token(TokenType::RightParen))
        .map(|((l, e), r)| e.with_src(l.src + r.src))
        .recognize(parser, t)
}

fn brace_stmt(parser: &mut Parser, t: usize) -> ParseResult<Node<Stmt>> {
    recognize_token(TokenType::LeftBrace)
        .then(recognizer(parse_declarations).map(|(src, decls)| Stmt::block(src, decls)))
        .then(recognize_token(TokenType::RightBrace))
        .map(|((l, s), r)| s.with_src(l.src + r.src))
        .recognize(parser, t)
}

fn parse_if_stmt(parser: &mut Parser, t: usize) -> ParseResult<Node<Stmt>> {
    first_success!(
        recognize_token(TokenType::If)
            .then(recognizer(paren_expr))
            .then(recognizer(brace_stmt))
            .map(|((r#if, cond), conseq)| Stmt::conditional(
                r#if.src + conseq.src,
                cond,
                conseq,
                None
            )),
        recognize_token(TokenType::If)
            .then(recognizer(paren_expr))
            .then(recognizer(brace_stmt))
            .then(recognize_token(TokenType::Else))
            .then(recognizer(brace_stmt))
            .map(|((((r#if, cond), conseq), _), altern)| Stmt::conditional(
                r#if.src + conseq.src,
                cond,
                conseq,
                Some(altern.into())
            )),
    )
    .recognize(parser, t)
}

fn parse_while_stmt(parser: &mut Parser, t: usize) -> ParseResult<Node<Stmt>> {
    recognize_token(TokenType::While)
        .then(recognizer(paren_expr))
        .then(recognizer(brace_stmt))
        .map(|((r#while, cond), body)| Stmt::r#while(r#while.src + body.src, cond, body))
        .recognize(parser, t)
}

fn parse_for_stmt(parser: &mut Parser, t: usize) -> ParseResult<Node<Stmt>> {
    fn decl_or_nothing(parser: &mut Parser, t: usize) -> ParseResult<Option<NodeRef<Declaration>>> {
        first_success!(
            recognizer(parse_declaration).map(|d| Some(d.into())),
            recognize_token(TokenType::Semicolon).map(|_| None),
        )
        .recognize(parser, t)
    }
    fn expr_or_nothing(parser: &mut Parser, t: usize) -> ParseResult<Option<Node<Expr>>> {
        first_success!(
            recognizer(parse_expression).map(|e| Some(e)),
            recognize_nothing().map(|_| None),
        )
        .recognize(parser, t)
    }
    fn for_triple(
        parser: &mut Parser,
        t: usize,
    ) -> ParseResult<(
        Option<NodeRef<Declaration>>,
        Option<Node<Expr>>,
        Option<Node<Expr>>,
    )> {
        recognize_token(TokenType::LeftParen)
            .then(recognizer(decl_or_nothing))
            .then(recognizer(expr_or_nothing))
            .then(recognize_token(TokenType::Semicolon))
            .then(recognizer(expr_or_nothing))
            .then(recognize_token(TokenType::RightParen))
            .map(|(((((_, e1), e2), _), e3), _)| (e1, e2, e3))
            .recognize(parser, t)
    }

    recognize_token(TokenType::For)
        .then(recognizer(for_triple))
        .then(recognizer(brace_stmt))
        .map(|((r#for, (init, cond, incr)), body)| {
            Stmt::r#for(r#for.src + body.src, init, cond, incr, body)
        })
        .recognize(parser, t)
}

fn parse_statement(parser: &mut Parser, t: usize) -> ParseResult<Node<Stmt>> {
    first_success!(
        recognizer(parse_expression)
            .then(recognize_token(TokenType::Semicolon))
            .map(|(e, semi)| Stmt::expr(e.src + semi.src, e)),
        recognize_token(TokenType::Print)
            .then(recognizer(parse_expression))
            .then(recognize_token(TokenType::Semicolon))
            .map(|((p, e), semi)| Stmt::print(p.src + semi.src, e)),
        recognizer(parse_if_stmt),
        recognizer(parse_while_stmt),
        recognizer(parse_for_stmt),
        recognize_token(TokenType::LeftBrace)
            .then(recognizer(parse_declarations).map(|(src, decls)| Stmt::block(src, decls)))
            .then(recognize_token(TokenType::RightBrace))
            .map(|((l, b), r)| b.with_src(l.src + r.src)),
    )
    .recognize(parser, t)
}

fn parse_declaration(parser: &mut Parser, t: usize) -> ParseResult<Node<Declaration>> {
    first_success!(
        recognizer(parse_statement).map(|s| Declaration::stmt(s.src, s)),
        recognize_token(TokenType::Var)
            .then(recognize_token(TokenType::Identifier))
            .then(recognize_token(TokenType::Equal))
            .then(recognizer(parse_expression))
            .then(recognize_token(TokenType::Semicolon))
            .map(|((((v, i), _), e), semi)| Declaration::vardecl(
                v.src + semi.src,
                i.src_str(parser.program),
                Some(e)
            )),
        recognize_token(TokenType::Var)
            .then(recognize_token(TokenType::Identifier))
            .then(recognize_token(TokenType::Semicolon))
            .map(|((v, i), semi)| Declaration::vardecl(
                v.src + semi.src,
                i.src_str(parser.program),
                None
            )),
    )
    .recognize(parser, t)
}

fn parse_program(parser: &mut Parser, t: usize) -> ParseResult<Node<Program>> {
    recognizer(parse_declarations)
        .map(|(src, decls)| Program::new(src, decls))
        .recognize(parser, t)
}

pub fn parse(program: &str) -> MultiResult<Node<Program>> {
    let tokens = scan(program)?;
    let mut parser = Parser {
        tokens: &tokens[..],
        program,
        errors: Errors::new("parser"),
    };
    let (t, result) = match parse_program(&mut parser, 0) {
        Success(t, v) => (t, v),
        Failure => {
            parser
                .errors
                .add(Error::syntax("unrecgonized program", Src::default()));
            return Err(parser.errors);
        }
        ParseResult::Error(e) => {
            parser
                .errors
                .add(Error::syntax(format!("error: {}", e), Src::default()));
            return Err(parser.errors);
        }
    };
    if t < parser.tokens.len() {
        let tok = &tokens[t];
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

    /// Shortcut to create a Token
    fn tok(src: Src, ty: TokenType) -> Token {
        Token { src, ty }
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
                                s(5, 2),
                                Stmt::expr(s(5, 2), Expr::variable(s(5, 1), "a"))
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

    fn test_parse_expression(program: &str, t: usize, exp: ParseResult<Node<Expr>>) {
        let mut parser = Parser {
            program,
            tokens: &scan(program).unwrap(),
            errors: Errors::new("test"),
        };
        assert_eq!(parse_expression(&mut parser, t), exp);
    }

    #[test]
    fn parseident() {
        test_parse_expression("x", 0, Success(1, Expr::variable(s(0, 1), "x")));
    }

    #[test]
    fn parsestring() {
        test_parse_expression("\"abc\"", 0, Success(1, Expr::string(s(0, 5), "abc")));
    }

    #[test]
    fn parsenumber() {
        test_parse_expression("123", 0, Success(1, Expr::number(s(0, 3), "123")));
    }

    #[test]
    fn parsetrue() {
        test_parse_expression("true", 0, Success(1, Expr::boolean(s(0, 4), true)));
    }

    #[test]
    fn parsefalse() {
        test_parse_expression("false", 0, Success(1, Expr::boolean(s(0, 5), false)));
    }

    #[test]
    fn parsenil() {
        test_parse_expression("nil", 0, Success(1, Expr::nil(s(0, 3))));
    }

    #[test]
    fn parse_unary_zero() {
        test_parse_expression("\"abc\"", 0, Success(1, Expr::string(s(0, 5), "abc")));
    }

    #[test]
    fn parse_unary_one() {
        test_parse_expression(
            "-\"abc\"",
            0,
            Success(
                2,
                Expr::unary(s(0, 6), UnaryOp::Neg, Expr::string(s(1, 5), "abc")),
            ),
        );
    }

    #[test]
    fn parse_unary_two() {
        test_parse_expression(
            "- -\"abc\"",
            0,
            Success(
                3,
                Expr::unary(
                    s(0, 8),
                    UnaryOp::Neg,
                    Expr::unary(s(2, 6), UnaryOp::Neg, Expr::string(s(3, 5), "abc")),
                ),
            ),
        );
    }

    #[test]
    fn parse_mul() {
        test_parse_expression(
            "1*2*3",
            0,
            Success(
                5,
                Expr::binop(
                    s(0, 5),
                    BinaryOp::Mul,
                    Expr::binop(
                        s(0, 3),
                        BinaryOp::Mul,
                        Expr::number(s(0, 1), "1"),
                        Expr::number(s(2, 1), "2"),
                    ),
                    Expr::number(s(4, 1), "3"),
                ),
            ),
        );
    }

    #[test]
    fn parse_mul_with_unary() {
        test_parse_expression(
            "1*-2/3",
            0,
            Success(
                6,
                Expr::binop(
                    s(0, 6),
                    BinaryOp::Div,
                    Expr::binop(
                        s(0, 4),
                        BinaryOp::Mul,
                        Expr::number(s(0, 1), "1"),
                        Expr::unary(s(2, 2), UnaryOp::Neg, Expr::number(s(3, 1), "2")),
                    ),
                    Expr::number(s(5, 1), "3"),
                ),
            ),
        );
    }

    #[test]
    fn parse_binops() {
        test_parse_expression(
            "1*2-3",
            0,
            Success(
                5,
                Expr::binop(
                    s(0, 5),
                    BinaryOp::Sub,
                    Expr::binop(
                        s(0, 3),
                        BinaryOp::Mul,
                        Expr::number(s(0, 1), "1"),
                        Expr::number(s(2, 1), "2"),
                    ),
                    Expr::number(s(4, 1), "3"),
                ),
            ),
        );
    }

    #[test]
    fn parse_multi_assignment() {
        test_parse_expression(
            "x=y=true==false",
            0,
            Success(
                7,
                Expr::assignment(
                    s(0, 15),
                    "x",
                    Expr::assignment(
                        s(2, 13),
                        "y",
                        Expr::binop(
                            s(4, 11),
                            BinaryOp::Equal,
                            Expr::boolean(s(4, 4), true),
                            Expr::boolean(s(10, 5), false),
                        ),
                    ),
                ),
            ),
        );
    }
}
