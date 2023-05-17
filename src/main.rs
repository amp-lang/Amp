use std::process::ExitCode;

use ampc::{
    sema,
    syntax::{self, ast::Stmnts},
    Context,
};

fn main() -> ExitCode {
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

    let mut unit = sema::Unit::new();
    {
        let res = unit.analyze(&mut cx, ast);

        cx.emit().unwrap();

        match res {
            Ok(_) => {}
            Err(_) => return ExitCode::FAILURE,
        }
    };

    ExitCode::SUCCESS
}
