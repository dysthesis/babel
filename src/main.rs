use babel::query::Query;
use clap::Parser;

use crate::cli::Cli;

mod cli;

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

fn main() -> color_eyre::Result<()> {
    // For heap profiling
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    // Install error logging mechanism
    color_eyre::install()?;

    let cli = Cli::parse();
    match cli.command {
        cli::Command::Server { token: _token } => todo!(),
        cli::Command::Fetch { id } => {
            let parsed = Query::parse(&id);
            println!("{parsed:?}")
        }
    }

    Ok(())
}
