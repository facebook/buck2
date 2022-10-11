#![deny(unsafe_code)]

#[macro_use]
extern crate array_macro;

use std::fmt::Debug;
use std::panic::catch_unwind;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering::Relaxed};

#[test]
fn simple_array() {
    assert_eq!(array![3; 5], [3, 3, 3, 3, 3]);
}

#[test]
fn callback_array() {
    assert_eq!(array![|x| x * 2; 3], [0, 2, 4]);
}

#[test]
fn outer_scope() {
    let x = 1;
    assert_eq!(array![x; 3], [1, 1, 1]);
}

#[test]
fn mutability() {
    let mut x = 1;
    assert_eq!(
        array![|_| {
            x += 1;
            x
        }; 3],
        [2, 3, 4]
    );
}

#[test]
fn big_array() {
    assert_eq!(&array!["x"; 333] as &[_], &["x"; 333] as &[_]);
}

#[test]
fn macro_within_macro() {
    assert_eq!(
        array![|x| array![|y| (x, y); 2]; 3],
        [[(0, 0), (0, 1)], [(1, 0), (1, 1)], [(2, 0), (2, 1)]]
    );
}

#[test]
fn const_expr() {
    const TWO: usize = 2;
    assert_eq!(array![|i| i; 2 + TWO], [0, 1, 2, 3]);
}

#[test]
fn panic_safety() {
    static CALLED_DROP: AtomicBool = AtomicBool::new(false);

    struct DontDrop;
    impl Drop for DontDrop {
        fn drop(&mut self) {
            CALLED_DROP.store(true, Relaxed);
        }
    }
    fn panicky() -> DontDrop {
        panic!();
    }
    assert!(catch_unwind(|| array![panicky(); 2]).is_err());
    assert_eq!(CALLED_DROP.load(Relaxed), false);
}

#[test]
fn panic_safety_part_two() {
    static DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

    struct DropOnlyThrice;
    impl Drop for DropOnlyThrice {
        fn drop(&mut self) {
            DROP_COUNT.fetch_add(1, Relaxed);
        }
    }
    fn panicky(i: usize) -> DropOnlyThrice {
        if i == 3 {
            panic!();
        }
        DropOnlyThrice
    }
    assert!(catch_unwind(|| array![|i| panicky(i); 555]).is_err());
    assert_eq!(DROP_COUNT.load(Relaxed), 3);
}

#[test]
fn array_of_void() {
    fn internal<T: Debug + Eq>(f: fn() -> T) {
        let a: [T; 0] = array![f(); 0];
        assert_eq!(a, []);
    }
    internal(|| -> ! { loop {} });
}

#[should_panic]
#[test]
fn array_of_void_panic_safety() {
    fn internal<T: Debug + Eq>(f: fn() -> T) {
        let _a: [T; 1] = array![f(); 1];
    }
    internal(|| -> ! { panic!() });
}

#[test]
fn malicious_length() {
    trait Evil {
        fn length(&self) -> *mut usize;
    }
    impl<T> Evil for T {
        fn length(&self) -> *mut usize {
            42 as *mut usize
        }
    }
    assert_eq!(array![1; 3], [1, 1, 1]);
}
