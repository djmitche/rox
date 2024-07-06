#![allow(dead_code)]
use crate::ast::{BinaryOp, Node, NodeRef, UnaryOp};
use crate::src::Src;

// _Just_ define a simple AST for me
macros::ast! {
    pub struct Program {
        pub statements: Vec<Node<Stmt>>,
    }

    pub enum Expr {
        Variable(String),
        String(String),
        Number(String),
        Boolean(bool),
        Nil,
        Unary(UnaryOp, NodeRef<Expr>),
        Binop(BinaryOp, NodeRef<Expr>, NodeRef<Expr>),
    }

    pub enum Stmt {
        Expr(Node<Expr>),
        Print(Node<Expr>),
        Conditional{
            cond: Node<Expr>,
            conseq: NodeRef<Stmt>,
            altern: NodeRef<Stmt>,
        }
    }
}
