use ampc::{
    syntax::{self},
    Context,
};

fn main() {
    let mut cx = Context::new();
    let file_id = cx.add_file(
        "HelloWorld.amp",
        std::fs::read_to_string("HelloWorld.amp").unwrap(),
    );

    let source = cx.files().get(file_id).unwrap().source().to_owned();
    let tokens = syntax::scan(&mut cx, file_id, &source);

    cx.emit().unwrap();

    println!("{:#?}", tokens);
}
