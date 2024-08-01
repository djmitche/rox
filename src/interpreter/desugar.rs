#![allow(unused_variables, dead_code)]
// TODO: move this to a compiler phase

use crate::ast::Node;
use crate::ast::{desugared, parsed};
use crate::error::Result;

struct DesugarProgram(Option<Node<desugared::Program>>);
struct DesugarDeclaration(Option<Node<desugared::Declaration>>);
struct DesugarStmt(Option<Node<desugared::Stmt>>);
struct DesugarExpr(Option<Node<desugared::Expr>>);

pub fn desugar_program(parsed: &Node<parsed::Program>) -> Result<Node<desugared::Program>> {
    Ok(desugared::Program::new(
        parsed.src,
        parsed
            .inner
            .statements
            .iter()
            .map(desugar_declaration)
            .collect::<Result<Vec<_>>>()?,
    ))
}

fn desugar_declaration(parsed: &Node<parsed::Declaration>) -> Result<Node<desugared::Declaration>> {
    Ok(match &parsed.inner {
        parsed::Declaration::VarDecl { variable, expr } => desugared::Declaration::vardecl(
            parsed.src,
            variable,
            expr.as_ref().map(desugar_expr).transpose()?,
        ),
        parsed::Declaration::Stmt(stmt) => {
            desugared::Declaration::stmt(parsed.src, desugar_stmt(stmt)?)
        }
    })
}

fn desugar_stmt(parsed: &Node<parsed::Stmt>) -> Result<Node<desugared::Stmt>> {
    Ok(match &parsed.inner {
        parsed::Stmt::Expr(e) => desugared::Stmt::expr(parsed.src, desugar_expr(e)?),
        parsed::Stmt::Block(b) => desugared::Stmt::block(
            parsed.src,
            b.iter()
                .map(desugar_declaration)
                .collect::<Result<Vec<_>>>()?,
        ),
        parsed::Stmt::Print(e) => {
            desugared::Stmt::print(parsed.src, e.as_ref().map(desugar_expr).transpose()?)
        }
        parsed::Stmt::Conditional {
            condition,
            consequent,
            alternate,
        } => desugared::Stmt::conditional(
            parsed.src,
            desugar_expr(condition)?,
            desugar_stmt(consequent)?,
            alternate
                .as_ref()
                .map(|s| desugar_stmt(s))
                .transpose()?
                .map(Box::new),
        ),
        parsed::Stmt::While { precondition, body } => desugared::Stmt::r#while(
            parsed.src,
            desugar_expr(precondition)?,
            desugar_stmt(body)?,
        ),
        parsed::Stmt::For {
            init,
            condition,
            increment,
            body,
        } => desugared::Stmt::r#for(
            parsed.src,
            init.as_ref()
                .map(|d| desugar_declaration(d))
                .transpose()?
                .map(Box::new),
            condition.as_ref().map(desugar_expr).transpose()?,
            increment
                .as_ref()
                .map(|e| desugar_expr(e))
                .transpose()?
                .map(Box::new),
            desugar_stmt(body)?,
        ),
    })
}

fn desugar_expr(parsed: &Node<parsed::Expr>) -> Result<Node<desugared::Expr>> {
    Ok(match &parsed.inner {
        parsed::Expr::Variable(v) => desugared::Expr::variable(parsed.src, v.clone()),
        parsed::Expr::String(s) => desugared::Expr::string(parsed.src, s.clone()),
        parsed::Expr::Number(n) => desugared::Expr::number(parsed.src, n),
        parsed::Expr::Boolean(b) => desugared::Expr::boolean(parsed.src, *b),
        parsed::Expr::Nil => desugared::Expr::nil(parsed.src),
        parsed::Expr::Assignment(v, e) => {
            desugared::Expr::assignment(parsed.src, v, desugar_expr(e)?)
        }
        parsed::Expr::Unary(o, e) => desugared::Expr::unary(parsed.src, *o, desugar_expr(e)?),
        parsed::Expr::BinOp(o, l, r) => {
            desugared::Expr::binop(parsed.src, *o, desugar_expr(l)?, desugar_expr(r)?)
        }
        parsed::Expr::LogOp(o, l, r) => {
            desugared::Expr::logop(parsed.src, *o, desugar_expr(l)?, desugar_expr(r)?)
        }
    })
}

pub fn desugar(parsed: &Node<parsed::Program>) -> Result<Node<desugared::Program>> {
    desugar_program(parsed)
}
