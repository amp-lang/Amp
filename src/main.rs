use std::{process::ExitCode, time::Instant};

use ampc::{
    clif::compile_air_to_obj_file,
    diag::Diag,
    sema,
    syntax::{self, ast::Stmnts},
    Context,
};
use clap::Parser;
use cli::{BuildObj, Command};

pub mod cli;

/// Builds an object file.
fn build_obj(cx: &mut Context, args: BuildObj) -> Result<(), ()> {
    let start_time = Instant::now();
    let file_id = cx.add_file(
        &args.root,
        match std::fs::read_to_string(&args.root) {
            Ok(value) => value,
            Err(_) => {
                cx.report(
                    Diag::new().error(format!("cannot open source file '{}'", &args.root), None),
                );
                return Err(());
            }
        },
    );

    let source = cx.files().get(file_id).unwrap().source().to_owned();
    let tokens = syntax::scan(cx, file_id, &source)?;

    let ast = Stmnts::parse(cx, &mut tokens.iter()).map_err(|_| ())?;

    let unit = sema::Unit::new();
    let air = unit.analyze(cx, ast)?;

    let obj = compile_air_to_obj_file(air);
    match std::fs::write(&args.output_path, obj) {
        Ok(_) => {}
        Err(_) => {
            cx.report(Diag::new().error(
                format!("cannot write output file '{}'", &args.output_path),
                None,
            ));
            return Err(());
        }
    }

    let elapsed = start_time.elapsed().as_nanos() as f64 / 1_000_000f64;
    println!("Finished in {}ms", elapsed);
    Ok(())
}

fn main() -> ExitCode {
    // execute subcommand
    let cmd = Command::parse();
    let mut cx = Context::new();
    let mut res = ExitCode::SUCCESS;

    match cmd {
        Command::BuildObj(args) => match build_obj(&mut cx, args) {
            Err(_) => res = ExitCode::FAILURE,
            _ => {}
        },
        Command::Version => println!("Amp Compiler v{}", env!("CARGO_PKG_VERSION")),
    }

    cx.emit().unwrap();
    res
}
