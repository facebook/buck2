extern crate quickcheck;

use quickcheck::{quickcheck, TestResult};

fn main() {
    fn prop(length: usize, index: usize) -> TestResult {
        let v: Vec<_> = (0..length).collect();
        if index < length {
            TestResult::discard()
        } else {
            TestResult::must_fail(move || v[index])
        }
    }
    quickcheck(prop as fn(usize, usize) -> TestResult);
}
