//! The Amp command line parser, built with Clap.

use clap::Parser;

/// The command for the command line to use.
#[derive(Debug, Parser)]
#[clap(disable_help_flag = true)]
pub enum Command {
    /// Compiles an Amp program into an object file.
    BuildObj(BuildObj),

    /// Prints the version of the installed Amp compiler.
    Version,
}

/// Builds the provided object file.
#[derive(Debug, Parser)]
#[clap(disable_help_flag = true)]
pub struct BuildObj {
    /// The root Amp module to compile.  This will be used as the main entry point for the Amp
    /// program.
    #[clap(required = true)]
    pub root: String,

    /// The path that the generated object file should be outputted to.
    #[clap(required = true, short = 'o')]
    pub output_path: String,
}
