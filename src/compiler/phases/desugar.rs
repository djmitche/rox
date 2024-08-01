//! De-sugar the parsed rox program:
//!
//!  - Convert `for` loops into a simple `loop`.
//!  - Replace all optional bits with the default.

use crate::ast::Node;
use crate::ast::{desugared, parsed};
use crate::error::Result;

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
            expr.as_ref()
                .map(desugar_expr)
                .transpose()?
                .unwrap_or_else(|| desugared::Expr::nil(parsed.src)),
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
            let v = e
                .as_ref()
                .map(desugar_expr)
                .transpose()?
                .unwrap_or_else(|| desugared::Expr::string(parsed.src, ""));
            desugared::Stmt::print(parsed.src, v)
        }
        parsed::Stmt::Conditional {
            condition,
            consequent,
            alternate,
        } => desugared::Stmt::conditional(
            parsed.src,
            desugar_expr(condition)?,
            desugar_stmt(consequent)?,
            Box::new(
                alternate
                    .as_ref()
                    .map(|s| desugar_stmt(s))
                    .transpose()?
                    .unwrap_or_else(|| desugared::Stmt::block(parsed.src, vec![])),
            ),
        ),
        parsed::Stmt::While { precondition, body } => {
            desugared::Stmt::r#loop(parsed.src, desugar_expr(precondition)?, desugar_stmt(body)?)
        }
        parsed::Stmt::For {
            init,
            condition,
            increment,
            body,
        } => {
            // Convert for loop to `{ #init; loop (#condition) { { #body } #increment } }`
            let mut outer_block = vec![];
            if let Some(init) = init {
                outer_block.push(desugar_declaration(init)?);
            }
            let loop_condition = condition.as_ref().map(desugar_expr).transpose()?;
            let loop_condition =
                loop_condition.unwrap_or_else(|| desugared::Expr::boolean(body.src, true));
            let mut loop_body = vec![];
            loop_body.push(desugared::Declaration::stmt(body.src, desugar_stmt(body)?));
            if let Some(increment) = increment {
                loop_body.push(desugared::Declaration::stmt(
                    increment.src,
                    desugared::Stmt::expr(increment.src, desugar_expr(increment)?),
                ));
            }
            let loop_body = desugared::Stmt::block(body.src, loop_body);
            outer_block.push(desugared::Declaration::stmt(
                parsed.src,
                desugared::Stmt::r#loop(parsed.src, loop_condition, loop_body),
            ));
            desugared::Stmt::block(parsed.src, outer_block)
        }
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
