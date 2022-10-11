extern crate overload;
use overload::overload;
use std::ops;

#[derive(PartialEq, Debug)]
struct A(i32);

#[derive(PartialEq, Debug)]
struct B(i32);

overload!(- (a: A) -> B { B(-a.0) });
#[test]
fn neg() {
    assert_eq!(-A(3), B(-3));
}

overload!(! (a: A) -> B { B(!a.0) });
#[test]
fn not() {
    assert_eq!(!A(3), B(!3));
}
