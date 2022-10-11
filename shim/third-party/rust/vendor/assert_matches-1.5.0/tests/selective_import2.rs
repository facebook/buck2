#[macro_use(debug_assert_matches)]
extern crate assert_matches;

#[test]
fn test_assert_succeed() {
    let a = 42u32;

    debug_assert_matches!(a, 42);
}
