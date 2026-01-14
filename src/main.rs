use clap::Parser;

use crate::cli::Cli;

mod cli;
fn main() -> color_eyre::Result<()> {
    // Install error logging mechanism
    color_eyre::install()?;

    let cli = Cli::parse();
    println!("{cli:?}");
    Ok(())
}
