use std::path::PathBuf;
use std::{env, fs};
use thiserror::Error;

fn main() -> anyhow::Result<()> {
    let mut args = env::args_os();

    let path = match args.nth(1) {
        Some(path) => PathBuf::from(path),
        None => return Err(UsageError::NoArgs.into()),
    };

    if args.next().is_some() {
        return Err(UsageError::TooManyArgs.into());
    }

    let contents = fs::read(path)?;
    unnamedvm::run(&contents)?;

    Ok(())
}

#[derive(Debug, Error)]
enum UsageError {
    #[error("no arguments were provided")]
    NoArgs,
    #[error("too many arguments were provided")]
    TooManyArgs,
}
