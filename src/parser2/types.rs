use crate::{error::Errors, token::Token};

pub(super) struct Parser<'p> {
    pub(super) program: &'p str,
    pub(super) tokens: &'p [Token],
    pub(super) errors: Errors,
}

#[derive(Debug)]
pub(super) enum ParseResult<T> {
    Success(usize, T),
    Failure,
    Error(String),
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
