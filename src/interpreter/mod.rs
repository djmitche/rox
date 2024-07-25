mod value;
mod environment;

use std::rc::Rc;

use crate::ast::{self, parsed};
use crate::error::Result;
use environment::Environment;
use value::Value;

pub struct Interpreter {
    environment: Rc<Environment>,
    stack: Vec<Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(),
            stack: Vec::new(),
        }
    }
}
impl parsed::Visitor for Interpreter {
    fn expr_end(&mut self, expr: &mut parsed::Expr) -> Result<()> {
        use parsed::Expr::*;
        match expr {
            Variable(n) => self.stack.push(self.environment.get(n)?),
            String(s) => self.stack.push(Value::String(s.clone())),
            Number(n) => self.stack.push(Value::Number(n.parse().unwrap())),
            Boolean(b) => self.stack.push(Value::Bool(*b)),
            Nil => self.stack.push(Value::Nil),
            Assignment(name, _) => {
                self.environment
                    .assign(name.clone(), self.stack.pop().unwrap())?;
            }
            Unary(op, _) => {
                let v = self.stack.pop().unwrap();
                self.stack.push(match (op, v) {
                    (ast::UnaryOp::Not, Value::Bool(b)) => Value::Bool(!b),
                    (ast::UnaryOp::Neg, Value::Number(n)) => Value::Number(-n),
                    _ => panic!("invalid unary"),
                });
            }
            BinOp(op, _, _) => {
                let r = self.stack.pop().unwrap();
                let l = self.stack.pop().unwrap();
                self.stack.push(match (l, r) {
                    (Value::Number(l), Value::Number(r)) => match op {
                        ast::BinaryOp::Mul => Value::Number(l * r),
                        ast::BinaryOp::Div => Value::Number(l / r),
                        ast::BinaryOp::Add => Value::Number(l + r),
                        ast::BinaryOp::Sub => Value::Number(l - r),
                        ast::BinaryOp::Less => Value::Bool(l < r),
                        ast::BinaryOp::LessEqual => Value::Bool(l <= r),
                        ast::BinaryOp::Equal => Value::Bool(l == r),
                        ast::BinaryOp::GreaterEqual => Value::Bool(l >= r),
                        ast::BinaryOp::Greater => Value::Bool(l > r),
                    },
                    (Value::String(l), Value::String(r)) => match op {
                        ast::BinaryOp::Add => Value::String(format!("{l}{r}")),
                        ast::BinaryOp::Less => Value::Bool(l < r),
                        ast::BinaryOp::LessEqual => Value::Bool(l <= r),
                        ast::BinaryOp::Equal => Value::Bool(l == r),
                        ast::BinaryOp::GreaterEqual => Value::Bool(l >= r),
                        ast::BinaryOp::Greater => Value::Bool(l > r),
                        _ => panic!("invalid binary op"),
                    },
                    _ => panic!("invalid binary op"),
                });
            }
        };
        Ok(())
    }

    fn declaration_end(&mut self, stmt: &mut parsed::Declaration) -> Result<()> {
        use parsed::Declaration::*;
        match stmt {
            Stmt(_) => {}
            VarDecl { variable, expr } => {
                let value = if expr.is_some() {
                    // Expression has left its value on the stack.
                    self.stack.pop().unwrap()
                } else {
                    Value::Nil
                };
                self.environment.define(variable.clone(), value);
            }
        }
        Ok(())
    }

    fn stmt_start(&mut self, stmt: &mut parsed::Stmt) -> Result<bool> {
        use parsed::Stmt::*;
        #[allow(clippy::single_match)]
        match stmt {
            Block(_) => {
                self.environment = self.environment.new_child();
            }
            _ => {},
        }
        Ok(true)
    }

    fn stmt_end(&mut self, stmt: &mut parsed::Stmt) -> Result<()> {
        use parsed::Stmt::*;
        match stmt {
            Expr(_) => {
                // Expression statement discards its result.
                self.stack.pop();
            }
            Block(_) => {
                self.environment = self.environment.parent().unwrap();
            }
            Print(_) => {
                println!("{:?}", self.stack.pop().unwrap());
            }
        }
        Ok(())
    }
}