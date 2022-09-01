#[test]
fn test_expect_works() {
    assert_eq!(1 + 1, 2);
}

#[test]
fn test_expect_fails() {
    assert_eq!(1 + 1, 3);
}
