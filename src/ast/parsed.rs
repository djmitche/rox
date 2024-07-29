//! AST representation of a parsed Rox program.
#![allow(dead_code)]
use crate::ast::node::{Node, NodeRef};
use crate::ast::ops::{BinaryOp, UnaryOp, LogicalOp};
use crate::error::Result;
use crate::src::Src;

macros::ast! {
    pub struct Program {
        pub statements: Vec<Node<Declaration>>,
    }

    pub enum Declaration {
        VarDecl {
            variable: String,
            expr: Option<Node<Expr>>,
        },
        Stmt(Node<Stmt>)
    }

    pub enum Stmt {
        Expr(Node<Expr>),
        Block(Vec<Node<Declaration>>),
        Print(Option<Node<Expr>>),
        Conditional{
            condition: Node<Expr>,
            consequent: NodeRef<Stmt>,
            alternate: Option<NodeRef<Stmt>>,
        },
        // TODO: use identifier literals in macro so `Loop` is OK
        Looop {
            precondition: Node<Expr>,
            body: NodeRef<Stmt>,
        },
    }

    pub enum Expr {
        Variable(String),
        String(String),
        Number(String),
        Boolean(bool),
        Nil,
        Assignment(String, NodeRef<Expr>),
        Unary(UnaryOp, NodeRef<Expr>),
        BinOp(BinaryOp, NodeRef<Expr>, NodeRef<Expr>),
        LogOp(LogicalOp, NodeRef<Expr>, NodeRef<Expr>),
    }
}
