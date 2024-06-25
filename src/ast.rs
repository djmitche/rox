#[derive(Debug, PartialEq, Eq)]
pub struct Node<'p, K> {
    pub inner: K,
    /// Source for this node, from the input program. For tokens not completely identified by `ty`,
    /// this is further interpreted during parsing.
    pub src: &'p str,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'p> {
    /// Variable, carrying the name of the variable.
    Variable(&'p str),
    /// String, containing the string content (without `"` characters).
    String(&'p str),
    /// Number literal, containing its textual representation.
    Number(&'p str),
    /// Boolean literal.
    Boolean(bool),
    /// Nil literal.
    Nil,
    /// Boolean negation.
    Not(NodeRef<'p, Expr<'p>>),
    /// Numeric negation.
    Neg(NodeRef<'p, Expr<'p>>),
    /// Multiplication.
    Mul(NodeRef<'p, Expr<'p>>, NodeRef<'p, Expr<'p>>),
    /// Division.
    Div(NodeRef<'p, Expr<'p>>, NodeRef<'p, Expr<'p>>),
    /// Addition.
    Add(NodeRef<'p, Expr<'p>>, NodeRef<'p, Expr<'p>>),
    /// Subtraction.
    Sub(NodeRef<'p, Expr<'p>>, NodeRef<'p, Expr<'p>>),
}

impl<'p> Expr<'p> {
    pub fn variable(src: &'p str, value: &'p str) -> Node<'p, Self> {
        Node {
            inner: Expr::Variable(value),
            src,
        }
    }

    pub fn string(src: &'p str, value: &'p str) -> Node<'p, Self> {
        Node {
            inner: Expr::String(value),
            src,
        }
    }

    pub fn number(src: &'p str, value: &'p str) -> Node<'p, Self> {
        Node {
            inner: Expr::Number(value),
            src,
        }
    }

    pub fn boolean(src: &'p str, value: bool) -> Node<'p, Self> {
        Node {
            inner: Expr::Boolean(value),
            src,
        }
    }

    pub fn nil(src: &'p str) -> Node<'p, Self> {
        Node {
            inner: Expr::Nil,
            src,
        }
    }

    pub fn not(src: &'p str, child: impl Into<NodeRef<'p, Self>>) -> Node<'p, Self> {
        Node {
            inner: Expr::Not(child.into()),
            src,
        }
    }

    pub fn neg(src: &'p str, child: impl Into<NodeRef<'p, Self>>) -> Node<'p, Self> {
        Node {
            inner: Expr::Neg(child.into()),
            src,
        }
    }

    pub fn mul(src: &'p str, lhs: impl Into<NodeRef<'p, Self>>, rhs: impl Into<NodeRef<'p, Self>>) -> Node<'p, Self> {
        Node {
            inner: Expr::Mul(lhs.into(), rhs.into()),
            src,
        }
    }

    pub fn div(src: &'p str, lhs: impl Into<NodeRef<'p, Self>>, rhs: impl Into<NodeRef<'p, Self>>) -> Node<'p, Self> {
        Node {
            inner: Expr::Div(lhs.into(), rhs.into()),
            src,
        }
    }

    pub fn add(src: &'p str, lhs: impl Into<NodeRef<'p, Self>>, rhs: impl Into<NodeRef<'p, Self>>) -> Node<'p, Self> {
        Node {
            inner: Expr::Add(lhs.into(), rhs.into()),
            src,
        }
    }

    pub fn sub(src: &'p str, lhs: impl Into<NodeRef<'p, Self>>, rhs: impl Into<NodeRef<'p, Self>>) -> Node<'p, Self> {
        Node {
            inner: Expr::Sub(lhs.into(), rhs.into()),
            src,
        }
    }
}

pub type NodeRef<'p, K> = Box<Node<'p, K>>;
