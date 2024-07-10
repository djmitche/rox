use crate::ast::{self, parsed};
use crate::error::{MultiResult, Result};
use crate::parser::parse;
use std::fs;
use std::io::{self, BufRead};

#[derive(Debug)]
enum Value {
    Nil,
    Bool(bool),
    String(String),
    Number(f64),
}

#[derive(Default)]
struct Interpreter {
    stack: Vec<Value>,
}

impl parsed::Visitor for Interpreter {
    fn expr_end(&mut self, expr: &mut parsed::Expr) -> Result<()> {
        use parsed::Expr::*;
        match expr {
            Variable(_) => todo!(),
            String(s) => self.stack.push(Value::String(s.clone())),
            Number(n) => self.stack.push(Value::Number(n.parse().unwrap())),
            Boolean(b) => self.stack.push(Value::Bool(*b)),
            Nil => self.stack.push(Value::Nil),
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

    fn stmt_end(&mut self, stmt: &mut parsed::Stmt) -> Result<()> {
        use parsed::Stmt::*;
        match stmt {
            Expr(_) => {
                // Expression statement discards its result.
                self.stack.pop();
            }
            Print(_) => {
                println!("{:?}", self.stack.pop().unwrap());
            }
        }
        Ok(())
    }
}

/// Run the given data as a rox program.
fn run(program: impl AsRef<str>) -> MultiResult<()> {
    let mut ast = parse(program.as_ref())?;
    // TODO: Error -> Errors conversion
    ast.traverse(&mut Interpreter::default()).unwrap();
    Ok(())
}

/// Run a REPL until EOF.
pub fn repl() -> MultiResult<()> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();

    while let Some(Ok(line)) = lines.next() {
        run(line)?;
    }

    Ok(())
}

/// Run a rox program from a file.
pub fn file(filename: impl AsRef<std::path::Path>) -> anyhow::Result<()> {
    let program = fs::read_to_string(filename)?;
    run(program)?;
    Ok(())
}
