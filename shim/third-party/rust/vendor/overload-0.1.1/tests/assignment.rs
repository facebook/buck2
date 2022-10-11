extern crate overload;
use overload::overload;
use std::ops;

#[derive(PartialEq, Debug)]
struct A(i32);

#[derive(PartialEq, Debug)]
struct B(i32);

overload!((a: &mut A) += (b: B) { a.0 += b.0; });
#[test]
fn add_assign() {
    let mut a = A(3);
    a += B(4);
    assert_eq!(A(3 + 4), a);
}

overload!((a: &mut A) -= (b: B) { a.0 -= b.0; });
#[test]
fn sub_assign() {
    let mut a = A(3);
    a -= B(4);
    assert_eq!(A(3 - 4), a);
}

overload!((a: &mut A) *= (b: B) { a.0 *= b.0; });
#[test]
fn mul_assign() {
    let mut a = A(3);
    a *= B(4);
    assert_eq!(A(3 * 4), a);
}

overload!((a: &mut A) /= (b: B) { a.0 /= b.0; });
#[test]
fn div_assign() {
    let mut a = A(6);
    a /= B(3);
    assert_eq!(A(6 / 3), a);
}

overload!((a: &mut A) %= (b: B) { a.0 %= b.0; });
#[test]
fn rem_assign() {
    let mut a = A(6);
    a %= B(4);
    assert_eq!(A(6 % 4), a);
}

overload!((a: &mut A) &= (b: B) { a.0 &= b.0; });
#[test]
fn bitand_assign() {
    let mut a = A(6);
    a &= B(4);
    assert_eq!(A(6 & 4), a);
}

overload!((a: &mut A) |= (b: B) { a.0 |= b.0; });
#[test]
fn bitor_assign() {
    let mut a = A(6);
    a |= B(4);
    assert_eq!(A(6 | 4), a);
}

overload!((a: &mut A) ^= (b: B) { a.0 ^= b.0; });
#[test]
fn bitxor_assign() {
    let mut a = A(6);
    a ^= B(4);
    assert_eq!(A(6 ^ 4), a);
}

overload!((a: &mut A) <<= (b: B) { a.0 <<= b.0; });
#[test]
fn shl_assign() {
    let mut a = A(6);
    a <<= B(4);
    assert_eq!(A(6 << 4), a);
}

overload!((a: &mut A) >>= (b: B) { a.0 >>= b.0; });
#[test]
fn shr_assign() {
    let mut a = A(6);
    a >>= B(4);
    assert_eq!(A(6 >> 4), a);
}
