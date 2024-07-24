#[derive(Debug, Clone, PartialEq)]
pub(super) enum Value {
    Nil,
    Bool(bool),
    String(String),
    Number(f64),
}
