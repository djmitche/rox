use crate::token::TokenType;

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

#[derive(Debug, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
}

impl TryFrom<TokenType> for LogicalOp {
    type Error = ();
    fn try_from(ty: TokenType) -> Result<Self, Self::Error> {
        Ok(match ty {
            TokenType::And => LogicalOp::And,
            TokenType::Or => LogicalOp::Or,
            _ => return Err(()),
        })
    }
}
