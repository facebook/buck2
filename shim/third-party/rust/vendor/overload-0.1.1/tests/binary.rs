extern crate overload;
use overload::overload;
use std::ops;

#[derive(PartialEq, Debug)]
struct A(i32);

#[derive(PartialEq, Debug)]
struct B(i32);

#[derive(PartialEq, Debug)]
struct C(i32);

overload!((a: A) + (b: B) -> C { C(a.0 + b.0) });
#[test]
fn add() {
    assert_eq!(A(3) + B(4), C(3 + 4));
}

overload!((a: A) - (b: B) -> C { C(a.0 - b.0) });
#[test]
fn sub() {
    assert_eq!(A(3) - B(4), C(3 - 4));
}

overload!((a: A) * (b: B) -> C { C(a.0 * b.0) });
#[test]
fn mul() {
    assert_eq!(A(3) * B(4), C(3 * 4));
}

overload!((a: A) / (b: B) -> C { C(a.0 / b.0) });
#[test]
fn div() {
    assert_eq!(A(6) / B(3), C(6 / 3));
}

overload!((a: A) % (b: B) -> C { C(a.0 % b.0) });
#[test]
fn rem() {
    assert_eq!(A(6) % B(4), C(6 % 4));
}

overload!((a: A) & (b: B) -> C { C(a.0 & b.0) });
#[test]
fn bitand() {
    assert_eq!(A(6) & B(4), C(6 & 4));
}

overload!((a: A) | (b: B) -> C { C(a.0 | b.0) });
#[test]
fn bitor() {
    assert_eq!(A(6) | B(4), C(6 | 4));
}

overload!((a: A) ^ (b: B) -> C { C(a.0 ^ b.0) });
#[test]
fn bitxor() {
    assert_eq!(A(6) ^ B(4), C(6 ^ 4));
}

overload!((a: A) << (b: B) -> C { C(a.0 << b.0) });
#[test]
fn shl() {
    assert_eq!(A(6) << B(4), C(6 << 4));
}

overload!((a: A) >> (b: B) -> C { C(a.0 >> b.0) });
#[test]
fn shr() {
    assert_eq!(A(6) >> B(4), C(6 >> 4));
}
