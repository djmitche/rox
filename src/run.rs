use crate::compiler::desugar;
use crate::error::Result;
use crate::interpreter::Interpreter;
use crate::parser::parse;
use std::fs;
use std::io::{self, BufRead};

/// Run the given data as a rox program.
fn run(program: impl AsRef<str>) -> Result<()> {
    let parsed = parse(program.as_ref())?;
    let mut desugared = desugar(&parsed).unwrap();
    desugared.traverse(&mut Interpreter::new())?;
    Ok(())
}

/// Run a REPL until EOF.
pub fn repl() -> Result<()> {
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
