use ampc::{
    syntax::{self, ast::Id},
    Context,
};

fn main() -> Result<(), ()> {
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
            Err(_) => return Err(()),
        }
    };

    let res = Id::parse(&mut cx, &mut tokens.iter());

    println!("{:#?}", res);

    Ok(())
}
