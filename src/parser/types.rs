use crate::token::Token;

pub(super) struct Parser<'p> {
    pub(super) program: &'p str,
    pub(super) tokens: &'p [Token],
}

#[derive(Debug)]
pub(super) enum ParseResult<T> {
    Success(usize, T),
    Failure,
    Error(String),
}

impl<T> std::ops::FromResidual for ParseResult<T> {
    fn from_residual(residual: <Self as std::ops::Try>::Residual) -> Self {
        if let Some(e) = residual {
            ParseResult::Error(e)
        } else {
            ParseResult::Failure
        }
    }
}

impl<T> std::ops::Try for ParseResult<T> {
    type Output = (usize, T);

    type Residual = Option<String>;

    fn from_output(output: Self::Output) -> Self {
        ParseResult::Success(output.0, output.1)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            ParseResult::Success(t, v) => std::ops::ControlFlow::Continue((t, v)),
            ParseResult::Failure => std::ops::ControlFlow::Break(None),
            ParseResult::Error(e) => std::ops::ControlFlow::Break(Some(e)),
        }
    }
}

impl<T: std::cmp::PartialEq> PartialEq for ParseResult<T> {
    fn eq(&self, other: &Self) -> bool {
        use ParseResult::*;
        match (self, other) {
            (Success(lt, lv), Success(rt, rv)) => (lt, lv) == (rt, rv),
            // Errors are never equal
            (Error(_), Error(_)) => false,
            (Failure, Failure) => true,
            _ => false,
        }
    }
}
