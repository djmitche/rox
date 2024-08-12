#![feature(try_trait_v2)]

mod ast;
mod error;
mod interpreter;
mod compiler;
mod parser;
mod run;
mod scanner;
mod src;
mod token;

use anyhow::Result;

fn main() -> Result<()> {
    let args: Vec<_> = std::env::args().collect();
    match &args[..] {
        [_, filename] => run::file(filename)?,
        [_] => run::repl()?,
        _ => {
            eprintln!("USAGE: rox [script]");
            std::process::exit(1)
        }
    }
    Ok(())
}
