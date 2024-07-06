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

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Node<Stmt>>,
}

impl Program {
    #[allow(dead_code)] // currently only used in tests
    pub fn stmts(src: Src, statements: Vec<Node<Stmt>>) -> Node<Program> {
        Node {
            inner: Program { statements },
            src,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    Expr(Node<Expr>),
    Print(Node<Expr>),
}

impl Stmt {
    pub fn expr(src: Src, value: Node<Expr>) -> Node<Self> {
        Node {
            inner: Stmt::Expr(value),
            src,
        }
    }

    pub fn print(src: Src, value: Node<Expr>) -> Node<Self> {
        Node {
            inner: Stmt::Print(value),
            src,
        }
    }
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
    /// A unary operation.
    Unary(UnaryOp, NodeRef<Expr>),
    /// A binary operation.
    Binop(BinaryOp, NodeRef<Expr>, NodeRef<Expr>),
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

    pub fn unary_op(op: UnaryOp, src: Src, child: impl Into<NodeRef<Self>>) -> Node<Self> {
        Node {
            inner: Expr::Unary(op, child.into()),
            src,
        }
    }

    pub fn binary_op(
        op: BinaryOp,
        src: Src,
        lhs: impl Into<NodeRef<Self>>,
        rhs: impl Into<NodeRef<Self>>,
    ) -> Node<Self> {
        Node {
            inner: Expr::Binop(op, lhs.into(), rhs.into()),
            src,
        }
    }
}
