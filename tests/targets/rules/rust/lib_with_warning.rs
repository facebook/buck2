fn unused_private_function() {
    let mut foo: Vec<u32> = vec![];
}

pub fn clippy_test(v: &Vec<i32>) -> Option<&i32> {
    v.iter().nth(2)
}
