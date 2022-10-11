#[macro_use(assert_matches)]
extern crate assert_matches;

#[test]
fn test_assert_succeed() {
    let a = 42u32;

    assert_matches!(a, 42);
}
