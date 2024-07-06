use crate::src::Src;
use crate::token::TokenType;

// TODO: consider https://crates.io/crates/syntree

#[derive(PartialEq, Eq)]
pub struct Node<K> {
    pub inner: K,
    pub src: Src,
}

impl<K: std::fmt::Debug> std::fmt::Debug for Node<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("Node@{}+{}", self.src.offset, self.src.len))
            .field(&self.inner)
            .finish()
    }
}

pub type NodeRef<K> = Box<Node<K>>;

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl TryFrom<TokenType> for UnaryOp {
    type Error = ();
    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
            TokenType::Minus => UnaryOp::Neg,
            TokenType::Bang => UnaryOp::Not,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Mul,
    Div,
    Add,
    Sub,
    Less,
    LessEqual,
    Equal,
    GreaterEqual,
    Greater,
}

impl TryFrom<TokenType> for BinaryOp {
    type Error = ();
    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
            TokenType::Star => BinaryOp::Mul,
            TokenType::Slash => BinaryOp::Div,
            TokenType::Plus => BinaryOp::Add,
            TokenType::Minus => BinaryOp::Sub,
            TokenType::Less => BinaryOp::Less,
            TokenType::LessEqual => BinaryOp::LessEqual,
            TokenType::EqualEqual => BinaryOp::Equal,
            TokenType::GreaterEqual => BinaryOp::GreaterEqual,
            TokenType::Greater => BinaryOp::Greater,
            _ => return Err(()),
        })
    }
}

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
