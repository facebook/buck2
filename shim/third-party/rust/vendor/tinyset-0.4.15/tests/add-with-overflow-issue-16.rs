use tinyset::Set64;

#[test]
fn issue_16() {
    let mut a: Set64<u64> = Set64::new();
    let mut b: Set64<u64> = Set64::new();
    a.insert(u64::MAX);
    a.insert(8589934592);
    a.insert(8589934593);
    a.insert(17179869184);
    a.insert(17179869185);
    a.insert(12884901889);
    b.insert(17179869187);
    b.insert(17179869188);
    b.insert(12884901889);
    b.insert(12884901890);
    b.insert(12884901891);
    b.insert(12884901892);
    let _c = &a - &b;
}
