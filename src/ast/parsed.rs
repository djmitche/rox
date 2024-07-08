//! AST representation of a parsed Rox program.
#![allow(dead_code)]
use crate::ast::node::{Node, NodeRef};
use crate::ast::ops::{BinaryOp, UnaryOp};
use crate::error::Result;
use crate::src::Src;

macros::ast! {
    pub struct Program {
        pub statements: Vec<Node<Stmt>>,
    }

    pub enum Expr {
        Variable(String),
        String(String),
        Number(String),
        Boolean(bool),
        Nil,
        Unary(UnaryOp, NodeRef<Expr>),
        Binop(BinaryOp, NodeRef<Expr>, NodeRef<Expr>),
    }

    pub enum Stmt {
        Expr(Node<Expr>),
        Print(Node<Expr>),
    }
}

// `traverse` performs the recursion, while the `_pre/_recurse/_pose` methods only need to be
// overridden when they are of interest.

// TODO: pass &mut src??
// TODO: generate this with `ast!`

impl Node<Program> {
    #[inline]
    pub fn traverse<V: Visitor>(&mut self, v: &mut V) -> Result<()> {
        if !v.program_pre(&mut self.inner)? {
            return Ok(());
        }
        v.program_recurse(&mut self.inner)?;
        v.program_post(&mut self.inner)
    }
}

impl Node<Expr> {
    #[inline]
    pub fn traverse<V: Visitor>(&mut self, v: &mut V) -> Result<()> {
        if !v.expr_pre(&mut self.inner)? {
            return Ok(());
        }
        v.expr_recurse(&mut self.inner)?;
        v.expr_post(&mut self.inner)
    }
}

impl Node<Stmt> {
    #[inline]
    pub fn traverse<V: Visitor>(&mut self, v: &mut V) -> Result<()> {
        if !v.stmt_pre(&mut self.inner)? {
            return Ok(());
        }
        v.stmt_recurse(&mut self.inner)?;
        v.stmt_post(&mut self.inner)
    }
}

#[allow(unused_variables)]
pub trait Visitor: Sized {
    #[inline]
    fn program_pre(&mut self, program: &mut Program) -> Result<bool> {
        Ok(true)
    }

    #[inline]
    fn program_recurse(&mut self, program: &mut Program) -> Result<()> {
        for statement in &mut program.statements {
            statement.traverse(self)?;
        }
        Ok(())
    }

    #[inline]
    fn program_post(&mut self, program: &mut Program) -> Result<()> {
        Ok(())
    }

    #[inline]
    fn expr_pre(&mut self, expr: &mut Expr) -> Result<bool> {
        Ok(true)
    }

    #[inline]
    fn expr_recurse(&mut self, expr: &mut Expr) -> Result<()> {
        #[allow(unused_variables, unreachable_patterns)]
        match expr {
            Expr::Unary(v0, v1) => v1.traverse(self)?,
            Expr::Binop(v0, v1, v2) => {
                v1.traverse(self)?;
                v2.traverse(self)?;
            }
            _ => {}
        };
        Ok(())
    }

    #[inline]
    fn expr_post(&mut self, expr: &mut Expr) -> Result<()> {
        Ok(())
    }

    #[inline]
    fn stmt_pre(&mut self, stmt: &mut Stmt) -> Result<bool> {
        Ok(true)
    }

    #[inline]
    fn stmt_recurse(&mut self, stmt: &mut Stmt) -> Result<()> {
        #[allow(unused_variables, unreachable_patterns)]
        match stmt {
            Stmt::Expr(v0) => v0.traverse(self)?,
            Stmt::Print(v0) => v0.traverse(self)?,
            _ => {}
        };
        Ok(())
    }

    #[inline]
    fn stmt_post(&mut self, stmt: &mut Stmt) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // TODO: move to src, cfg(test)
    fn s(offset: usize, len: usize) -> Src {
        Src { offset, len }
    }

    #[test]
    fn fold_constant_strings() -> crate::error::MultiResult<()> {
        /// Simple constant-folding visitor that only folds strings.
        struct ConstantFold;
        impl Visitor for ConstantFold {
            fn expr_post(&mut self, expr: &mut Expr) -> Result<()> {
                match expr {
                    Expr::Binop(op, l, r) if *op == BinaryOp::Add => {
                        if let (
                            Node {
                                inner: Expr::String(l),
                                ..
                            },
                            Node {
                                inner: Expr::String(r),
                                ..
                            },
                        ) = (l.as_mut(), r.as_mut())
                        {
                            *expr = Expr::String(l.to_owned() + r);
                        }
                    }
                    _ => {}
                };
                Ok(())
            }
        }

        let mut prog = crate::parser::parse(r#"print "a" + "b" + "c";"#)?;
        prog.traverse(&mut ConstantFold).unwrap();
        assert_eq!(
            prog,
            Program::new(
                s(0, 22),
                vec![
                    Stmt::print(s(0, 22), Expr::string(s(6, 15), "abc")),
                ]
            )
        );
        Ok(())
    }
}
