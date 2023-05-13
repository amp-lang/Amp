use ampc::{syntax::Scanner, Context};

fn main() {
    let mut cx = Context::new();
    let file_id = cx.add_file(
        "HelloWorld.amp",
        std::fs::read_to_string("HelloWorld.amp").unwrap(),
    );

    let source = cx.files().get(file_id).unwrap().source().to_owned();
    let mut scanner = Scanner::new(&mut cx, file_id, &source);

    while let Some(token) = scanner.next() {
        println!(
            "{:?}: '{}' @ {:?}",
            token,
            scanner.slice().escape_debug(),
            scanner.span()
        )
    }

    cx.emit().unwrap();
}
