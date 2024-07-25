use crate::error::{Error, Result};
use crate::interpreter::value::Value;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

pub(super) struct Environment {
    parent: Option<Rc<Environment>>,
    variables: RefCell<HashMap<String, Value>>,
}

impl Environment {
    pub fn new() -> Rc<Self> {
        Rc::new(Self {
            parent: None,
            variables: RefCell::new(HashMap::new()),
        })
    }

    pub fn new_child(self: &Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            parent: Some(self.clone()),
            variables: RefCell::new(HashMap::new()),
        })
    }

    pub fn parent(self: &Rc<Self>) -> Option<Rc<Self>> {
        self.parent.clone()
    }

    pub fn define(&self, name: impl Into<String>, value: Value) {
        self.variables.borrow_mut().insert(name.into(), value);
    }

    pub fn assign(&self, name: impl AsRef<str>, value: Value) -> Result<()> {
        let name = name.as_ref();
        let mut variables = self.variables.borrow_mut();
        let entry = variables.entry(name.to_string());
        let Entry::Occupied(mut entry) = entry else {
            if let Some(parent) = &self.parent {
                return parent.assign(name, value);
            }
            return Err(Error::RuntimeError(format!("Undefined variable {name}")));
        };

        entry.insert(value);
        Ok(())
    }

    pub fn get(&self, name: impl AsRef<str>) -> Result<Value> {
        let name = name.as_ref();
        if let Some(v) = self.variables.borrow().get(name) {
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
        let e = Environment::new();
        e.define("foo", Value::Bool(true));
        let e = e.new_child();
        let e = e.parent().unwrap();
        assert_eq!(e.get("foo").unwrap(), Value::Bool(true));
        assert!(e.parent().is_none());
    }

    #[test]
    fn get_inherited() {
        let e = Environment::new();
        e.define("foo", Value::Bool(true));
        e.define("bar", Value::Bool(false));
        let e = e.new_child();
        e.define("foo", Value::Bool(false));
        assert_eq!(e.get("foo").unwrap(), Value::Bool(false));
        assert_eq!(e.get("bar").unwrap(), Value::Bool(false));
        let e = e.parent().unwrap();
        assert_eq!(e.get("foo").unwrap(), Value::Bool(true));
    }

    #[test]
    fn assign_inherited() {
        let e = Environment::new();
        e.define("outer", Value::Bool(false));
        let e = e.new_child();
        e.define("inner", Value::Bool(false));
        e.assign("outer", Value::Bool(true)).unwrap();
        e.assign("inner", Value::Bool(true)).unwrap();
        assert_eq!(e.get("outer").unwrap(), Value::Bool(true));
        assert_eq!(e.get("inner").unwrap(), Value::Bool(true));
        let e = e.parent().unwrap();
        assert_eq!(e.get("outer").unwrap(), Value::Bool(true));
        assert!(e.get("inner").is_err());
    }
}
