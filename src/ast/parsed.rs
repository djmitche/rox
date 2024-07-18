//! AST representation of a parsed Rox program.
#![allow(dead_code)]
use crate::ast::node::{Node, NodeRef};
use crate::ast::ops::{BinaryOp, UnaryOp};
use crate::error::Result;
use crate::src::Src;

macros::ast! {
    pub struct Program {
        pub statements: Vec<Node<Declaration>>,
    }

    pub enum Declaration {
        //VarDecl{variable: String, expr: Node<Expr>},
        Stmt(Node<Stmt>)
    }

    pub enum Stmt {
        Expr(Node<Expr>),
        Print(Option<Node<Expr>>),
    }

    pub enum Expr {
        Variable(String),
        String(String),
        Number(String),
        Boolean(bool),
        Nil,
        Unary(UnaryOp, NodeRef<Expr>),
        BinOp(BinaryOp, NodeRef<Expr>, NodeRef<Expr>),
    }
}
