use crate::src::Src;

#[derive(Debug, PartialEq, Eq)]
pub struct Node<K> {
    pub inner: K,
    /// Source for this node, from the input program. For tokens not completely identified by `ty`,
    /// this is further interpreted during parsing.
    pub src: Src,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    /// Variable, carrying the name of the variable.
    Variable(String),
    /// String, containing the string content (without `"` characters).
    String(String),
    /// Number literal, containing its textual representation.
    Number(String),
    /// Boolean literal.
    Boolean(bool),
    /// Nil literal.
    Nil,
    /// Boolean negation.
    Not(NodeRef<Expr>),
    /// Numeric negation.
    Neg(NodeRef<Expr>),
    /// Multiplication.
    Mul(NodeRef<Expr>, NodeRef<Expr>),
    /// Division.
    Div(NodeRef<Expr>, NodeRef<Expr>),
    /// Addition.
    Add(NodeRef<Expr>, NodeRef<Expr>),
    /// Subtraction.
    Sub(NodeRef<Expr>, NodeRef<Expr>),
}

impl Expr {
    pub fn variable(src: Src, value: impl Into<String>) -> Node<Self> {
        Node {
            inner: Expr::Variable(value.into()),
            src,
        }
    }

    pub fn string(src: Src, value: impl Into<String>) -> Node<Self> {
        Node {
            inner: Expr::String(value.into()),
            src,
        }
    }

    pub fn number(src: Src, value: impl Into<String>) -> Node<Self> {
        Node {
            inner: Expr::Number(value.into()),
            src,
        }
    }

    pub fn boolean(src: Src, value: bool) -> Node<Self> {
        Node {
            inner: Expr::Boolean(value),
            src,
        }
    }

    pub fn nil(src: Src) -> Node<Self> {
        Node {
            inner: Expr::Nil,
            src,
        }
    }

    pub fn not(src: Src, child: impl Into<NodeRef<Self>>) -> Node<Self> {
        Node {
            inner: Expr::Not(child.into()),
            src,
        }
    }

    pub fn neg(src: Src, child: impl Into<NodeRef<Self>>) -> Node<Self> {
        Node {
            inner: Expr::Neg(child.into()),
            src,
        }
    }

    pub fn mul(
        src: Src,
        lhs: impl Into<NodeRef<Self>>,
        rhs: impl Into<NodeRef<Self>>,
    ) -> Node<Self> {
        Node {
            inner: Expr::Mul(lhs.into(), rhs.into()),
            src,
        }
    }

    pub fn div(
        src: Src,
        lhs: impl Into<NodeRef<Self>>,
        rhs: impl Into<NodeRef<Self>>,
    ) -> Node<Self> {
        Node {
            inner: Expr::Div(lhs.into(), rhs.into()),
            src,
        }
    }

    pub fn add(
        src: Src,
        lhs: impl Into<NodeRef<Self>>,
        rhs: impl Into<NodeRef<Self>>,
    ) -> Node<Self> {
        Node {
            inner: Expr::Add(lhs.into(), rhs.into()),
            src,
        }
    }

    pub fn sub(
        src: Src,
        lhs: impl Into<NodeRef<Self>>,
        rhs: impl Into<NodeRef<Self>>,
    ) -> Node<Self> {
        Node {
            inner: Expr::Sub(lhs.into(), rhs.into()),
            src,
        }
    }
}

pub type NodeRef<K> = Box<Node<K>>;
