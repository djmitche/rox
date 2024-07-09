use crate::error::{Result, MultiResult};
use crate::parser::parse;
use crate::ast::{self, parsed};
use std::fs;
use std::io::{self, BufRead};

#[derive(Default)]
struct Interpreter {
    stack: Vec<String>,
}

impl parsed::Visitor for Interpreter {
    fn expr_end(&mut self, expr: &mut parsed::Expr) -> Result<()> {
        use parsed::Expr::*;
        match expr {
            Variable(_) => todo!(),
            String(s) => self.stack.push(s.clone()),
            Number(n) => self.stack.push(n.clone()),
            Boolean(b) => self.stack.push(b.to_string()),
            Nil => self.stack.push("".into()),
            Unary(op, _) => {
                let v = self.stack.pop().unwrap();
                match op {
                    ast::UnaryOp::Not => todo!(),
                    ast::UnaryOp::Neg => todo!(),
                }
            },
            BinOp(op, _, _) => {
                let r = self.stack.pop().unwrap();
                let l = self.stack.pop().unwrap();
                match op {
                    ast::BinaryOp::Mul => self.stack.push(format!("{l}*{r}")),
                    ast::BinaryOp::Div => self.stack.push(format!("{l}/{r}")),
                    ast::BinaryOp::Add => self.stack.push(format!("{l}+{r}")),
                    ast::BinaryOp::Sub => self.stack.push(format!("{l}-{r}")),
                    ast::BinaryOp::Less => todo!(),
                    ast::BinaryOp::LessEqual => todo!(),
                    ast::BinaryOp::Equal => todo!(),
                    ast::BinaryOp::GreaterEqual => todo!(),
                    ast::BinaryOp::Greater => todo!(),
                }
            },
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
                println!("{}", self.stack.pop().unwrap());
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
