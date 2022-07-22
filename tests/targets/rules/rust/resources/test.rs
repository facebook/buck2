use std::fs;

#[test]
fn test() {
    let key = "buck2/tests/targets/rules/rust/resources/resource_cxx";
    let resource_path = buck_resources::get_resource(key).unwrap();
    let content = fs::read_to_string(resource_path).unwrap();
    assert_eq!(content, "this is a cxx_library's resource\n");
}
