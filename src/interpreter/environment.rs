use crate::error::{Error, Result};
use crate::interpreter::value::Value;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub(super) struct Environment {
    parent: Option<Box<Environment>>,
    variables: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            parent: None,
            variables: HashMap::new(),
        })
    }

    pub fn push(self: Box<Self>) -> Box<Self> {
        Box::new(Self {
            parent: Some(self),
            variables: HashMap::new(),
        })
    }

    pub fn pop(self: Box<Self>) -> Option<Box<Self>> {
        self.parent
    }

    pub fn define(&mut self, name: impl Into<String>, value: Value) {
        // TODO
        self.variables.insert(name.into(), value);
    }

    pub fn assign(&mut self, name: impl Into<String>, value: Value) -> Result<()> {
        // TODO
        let entry = self.variables.entry(name.into());
        let Entry::Occupied(mut entry) = entry else {
            let name = entry.key();
            return Err(Error::RuntimeError(format!("Undefined variable {name}")));
        };

        entry.insert(value);
        Ok(())
    }

    pub fn get(&self, name: impl AsRef<str>) -> Result<Value> {
        let name = name.as_ref();
        if let Some(v) = self.variables.get(name) {
            Ok(v.clone())
        } else if let Some(p) = &self.parent {
            p.get(name)
        } else {
            Err(Error::RuntimeError(format!("Undefined variable {name}")))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn stack() {
        let mut e = Environment::new();
        e.define("foo", Value::Bool(true));
        let e = e.push();
        let e = e.pop().unwrap();
        assert_eq!(e.get("foo").unwrap(), Value::Bool(true));
        assert!(e.pop().is_none());
    }

    #[test]
    fn get_inherited() {
        let mut e = Environment::new();
        e.define("foo", Value::Bool(true));
        e.define("bar", Value::Bool(false));
        let mut e = e.push();
        e.define("foo", Value::Bool(false));
        assert_eq!(e.get("foo").unwrap(), Value::Bool(false));
        assert_eq!(e.get("bar").unwrap(), Value::Bool(false));
        let e = e.pop().unwrap();
        assert_eq!(e.get("foo").unwrap(), Value::Bool(true));
    }
}
