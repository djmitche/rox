use crate::scanner::scan;
use anyhow::Result;
use std::fs;
use std::io::{self, BufRead};

/// Run the given data as a rox program.
fn run(program: impl AsRef<str>) -> Result<()> {
    let program = program.as_ref();
    for token in scan(program)? {
        println!("{:?}", token);
    }
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
pub fn file(filename: impl AsRef<std::path::Path>) -> Result<()> {
    let program = fs::read_to_string(filename)?;
    run(program)?;
    Ok(())
}
