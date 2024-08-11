#![allow(unused_imports, dead_code)]

#[macro_use]
mod recog;
mod types;

use recog::*;
use types::*;

use crate::ast::parsed::{Declaration, Expr, Program, Stmt};
use crate::ast::{BinaryOp, LogicalOp, Node, NodeRef, UnaryOp};
use crate::error::{Error, Errors, MultiResult, Result};
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
            .map(|((_, e), _)| e),
    )
    .recognize(parser, t)
}

fn parse_unary(parser: &mut Parser, t: usize) -> ParseResult<Node<Expr>> {
    first_success!(
        recognize_token(TokenType::Minus)
            .then(recognizer(parse_unary))
            .map(|(tok, e)| Expr::unary(tok.src + e.src, UnaryOp::Neg, e)),
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::scan;
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
