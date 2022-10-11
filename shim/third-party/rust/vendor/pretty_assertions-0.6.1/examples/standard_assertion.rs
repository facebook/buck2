fn main() {
    #[derive(Debug, PartialEq)]
    struct Foo {
        lorem: &'static str,
        ipsum: u32,
        dolor: Result<String, String>,
    }

    let x = Some(Foo {
        lorem: "Hello World!",
        ipsum: 42,
        dolor: Ok("hey".to_string()),
    });
    let y = Some(Foo {
        lorem: "Hello Wrold!",
        ipsum: 42,
        dolor: Ok("hey ho!".to_string()),
    });

    assert_eq!(x, y);
}
