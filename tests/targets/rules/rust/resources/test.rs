use std::fs;

#[test]
fn test_cxx_library() {
    let key = "buck2/tests/targets/rules/rust/resources/resource_cxx";
    let resource_path = buck_resources::get_resource(key).unwrap();
    let content = fs::read_to_string(resource_path).unwrap();
    assert_eq!(content, "this is a cxx_library's resource\n");
}

#[test]
fn test_rust_test() {
    // FIXME(dtolnay): Is it correct that this is just the filename, or is it
    // supposed to contain a package path like the cxx one?
    let key = "resource_rs";
    let resource_path = buck_resources::get_resource(key).unwrap();
    let content = fs::read_to_string(resource_path).unwrap();
    assert_eq!(content, "this is a rust_test's resource\n");
}
