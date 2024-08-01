mod phases;
use crate::ast::Node;
use crate::error::Result;
use crate::ast::{parsed, desugared};

pub fn desugar(parsed: &Node<parsed::Program>) -> Result<Node<desugared::Program>> {
    phases::desugar::desugar(parsed)
}
