use std::{process::ExitCode, time::Instant};

use ampc::{
    clif::compile_air_to_obj_file,
    sema,
    syntax::{self, ast::Stmnts},
    Context,
};

fn main() -> ExitCode {
    let start_time = Instant::now();
    let mut cx = Context::new();
    let file_id = cx.add_file(
        "HelloWorld.amp",
        std::fs::read_to_string("HelloWorld.amp").unwrap(),
    );

    let source = cx.files().get(file_id).unwrap().source().to_owned();
    let tokens = {
        let res = syntax::scan(&mut cx, file_id, &source);

        cx.emit().unwrap();

        match res {
            Ok(value) => value,
            Err(_) => return ExitCode::FAILURE,
        }
    };

    let ast = {
        let res = Stmnts::parse(&mut cx, &mut tokens.iter());

        cx.emit().unwrap();

        match res {
            Ok(value) => value,
            Err(_) => return ExitCode::FAILURE,
        }
    };

    let unit = sema::Unit::new();
    let air = {
        let res = unit.analyze(&mut cx, ast);

        cx.emit().unwrap();

        match res {
            Ok(value) => value,
            Err(_) => return ExitCode::FAILURE,
        }
    };

    let obj = compile_air_to_obj_file(air);
    std::fs::write("HelloWorld.o", obj).unwrap();

    let elapsed = start_time.elapsed().as_nanos() as f64 / 1_000_000f64;
    println!("Finished in {}ms", elapsed);

    ExitCode::SUCCESS
}
