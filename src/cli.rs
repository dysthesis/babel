//! This module is responsible for parsing the command-line environment in which
//! Babel is running.

use clap::{Parser, Subcommand};

/// Babel is a Discord bot which is used to fetch and manage metadata about
/// literature, such as books and papers.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Run in server-mode as a Discord bot
    Server {
        /// The Discord bot token to use
        #[arg(short, long, value_name = "TOKEN")]
        token: String,
    },
    /// Fetch an item.
    Fetch {
        /// The identifier of the item to fetch
        #[arg(short, long, value_name = "IDENTIFIER")]
        id: String,
    },
}
