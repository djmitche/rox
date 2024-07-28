mod environment;
mod value;

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
    fn expr_recurse(&mut self, val: &mut parsed::Expr) -> crate::error::Result<()> {
        #[allow(unused_variables, unreachable_patterns)]
        match val {
            parsed::Expr::Assignment(v0, v1) => {
                v1.traverse(self)?;
            }
            parsed::Expr::Unary(v0, v1) => {
                v1.traverse(self)?;
            }
            parsed::Expr::BinOp(v0, v1, v2) => {
                v1.traverse(self)?;
                v2.traverse(self)?;
            }
            parsed::Expr::LogOp(op, lhs, rhs) => {
                lhs.traverse(self)?;
                let lhs_val = self.stack.last().unwrap();
                // Short-circuit appropriately, leaving the lhs val on the stack
                // if it is enough.
                match (op, lhs_val.is_truthy()) {
                    (ast::LogicalOp::And, false) | (ast::LogicalOp::Or, true) => {
                        return Ok(());
                    }
                    _ => {
                        self.stack.pop().unwrap();
                        rhs.traverse(self)?;
                    }
                }
            }
            _ => {}
        };
        Ok(())
    }

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
                    (ast::UnaryOp::Not, v) => Value::Bool(!v.is_truthy()),
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
            LogOp(_, _, _) => {
                // LogOp is handled in expr_recurse.
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

    fn stmt_recurse(&mut self, val: &mut parsed::Stmt) -> crate::error::Result<()> {
        #[allow(unused_variables, unreachable_patterns)]
        match val {
            parsed::Stmt::Expr(v0) => {
                v0.traverse(self)?;
                // Discard the result of the expression.
                self.stack.pop();
            }
            parsed::Stmt::Block(v0) => {
                self.environment = self.environment.new_child();
                for v in v0 {
                    v.traverse(self)?;
                }
                self.environment = self.environment.parent().unwrap();
            }
            parsed::Stmt::Print(v0) => {
                if let Some(v) = v0 {
                    v.traverse(self)?;
                    println!("{:?}", self.stack.pop().unwrap());
                } else {
                    println!();
                }
            }
            parsed::Stmt::Conditional {
                condition,
                consequent,
                alternate,
            } => {
                // The condition expression will leave a value on the stack.
                condition.traverse(self)?;
                if self.stack.pop().unwrap().is_truthy() {
                    consequent.traverse(self)?;
                } else if let Some(v) = alternate {
                    v.traverse(self)?;
                }
            }
            _ => {}
        };
        Ok(())
    }
}
