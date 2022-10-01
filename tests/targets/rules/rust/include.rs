fn main() {
    assert_eq!("42", env!("FORTY_TWO"));
    assert_eq!("hello", include!(env!("HELLO")));
}
