#[derive(Debug, Clone, PartialEq)]
pub(super) enum Value {
    Nil,
    Bool(bool),
    String(String),
    Number(f64),
}

impl Value {
    pub(super) fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(v) => *v,
            Value::String(_) => true,
            Value::Number(_) => true,
        }
    }
}
