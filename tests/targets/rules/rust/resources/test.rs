extern crate rust_with_resource; // avoid unused dependency lint

use std::fs;

#[test]
fn test_cxx_library() {
    let key = "buck2/tests/targets/rules/rust/resources/resource_cxx_library";
    let resource_path = buck_resources::get_resource(key).unwrap();
    let content = fs::read_to_string(resource_path).unwrap();
    assert_eq!(content, "this is a cxx_library's resource\n");
}

#[test]
fn test_rust_library() {
    let key = "buck2/tests/targets/rules/rust/resources/resource_rust_library";
    let resource_path = buck_resources::get_resource(key).unwrap();
    let content = fs::read_to_string(resource_path).unwrap();
    assert_eq!(content, "this is a rust_library's resource\n");
}

#[test]
fn test_rust_test() {
    let key = "buck2/tests/targets/rules/rust/resources/resource_rust_test";
    let resource_path = buck_resources::get_resource(key).unwrap();
    let content = fs::read_to_string(resource_path).unwrap();
    assert_eq!(content, "this is a rust_test's resource\n");
}
