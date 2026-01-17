use babel::{bot::Bot, query::Query};
use clap::Parser;
use color_eyre::eyre::eyre;

use crate::cli::Cli;

mod cli;

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

#[tokio::main]
async fn main() -> color_eyre::Result<()> {
    // For heap profiling
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    // Install error logging mechanism
    color_eyre::install()?;

    let cli = Cli::parse();
    match cli.command {
        cli::Command::Server { token, guild_id } => {
            // Set up logging
            // construct a subscriber that prints formatted traces to stdout
            let subscriber = tracing_subscriber::fmt()
                // Display source code file paths
                .with_file(true)
                // Display source code line numbers
                .with_line_number(true)
                // Display the thread ID an event was recorded on
                .with_thread_ids(true)
                // Display the event's target (module path)
                .with_target(true)
                // Build the subscriber
                .finish();

            // use that subscriber to process traces emitted after this point
            tracing::subscriber::set_global_default(subscriber)?;
            let bot = Bot::new(token, guild_id);
            if let Err(why) = bot.await.start().await {
                return Err(eyre!("Client error: {why:?}"));
            }
        }
        cli::Command::Fetch { id } => {
            let parsed = Query::parse(&id);
            println!("{parsed:?}")
        }
    }

    Ok(())
}
