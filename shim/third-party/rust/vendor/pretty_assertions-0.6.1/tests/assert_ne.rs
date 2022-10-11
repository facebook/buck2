#[allow(unused_imports)]
use pretty_assertions::{assert_eq, assert_ne};
#[test]
#[should_panic(expected = r#"assertion failed: `(left != right)`

[1mBoth sides[0m:
Some(
    Foo {
        lorem: "Hello World!",
        ipsum: 42,
        dolor: Ok(
            "hey"
        )
    }
)

"#)]
fn assert_ne() {
    #[derive(Debug, PartialEq)]
    struct Foo {
        lorem: &'static str,
        ipsum: u32,
        dolor: Result<String, String>,
    }

    let x = Some(Foo {
        lorem: "Hello World!",
        ipsum: 42,
        dolor: Ok("hey".to_string()),
    });

    assert_ne!(x, x);
}

#[test]
#[should_panic(
    expected = r#"assertion failed: `(left != right)`: custom panic message

[1mBoth sides[0m:
Some(
    Foo {
        lorem: "Hello World!",
        ipsum: 42,
        dolor: Ok(
            "hey"
        )
    }
)

"#
)]
fn assert_ne_custom() {
    #[derive(Debug, PartialEq)]
    struct Foo {
        lorem: &'static str,
        ipsum: u32,
        dolor: Result<String, String>,
    }

    let x = Some(Foo {
        lorem: "Hello World!",
        ipsum: 42,
        dolor: Ok("hey".to_string()),
    });

    assert_ne!(x, x, "custom panic message");
}

#[test]
#[should_panic]
fn assert_ne_non_empty_return() {
    fn not_zero(x: u32) -> u32 {
        assert_ne!(x, 0);
        x
    };
    not_zero(0);
}

#[test]
#[should_panic(expected = r#"assertion failed: `(left != right)`

[1mDiff[0m [31m< left[0m / [32mright >[0m :
[31m<[0m[1;48;5;52;31m-[0m[31m0.0[0m
[32m>[0m[32m0.0[0m

[1;4mNote[0m: According to the `PartialEq` implementation, both of the values are partially equivalent, even if the `Debug` outputs differ.

"#)]
fn assert_ne_partial() {
    // Workaround for https://github.com/rust-lang/rust/issues/47619
    // can be removed, when we require rust 1.25 or higher
    struct Foo(f32);

    use ::std::fmt;
    impl fmt::Debug for Foo {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{:.1?}", self.0)
        }
    }

    impl PartialEq for Foo {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }

    assert_ne!(Foo(-0.0), Foo(0.0));
}

#[test]
#[should_panic(expected = r#"assertion failed: `(left != right)`

[1mBoth sides[0m:
Some(
    Foo {
        lorem: "Hello World!",
        ipsum: 42,
        dolor: Ok(
            "hey"
        )
    }
)

"#)]
fn assert_ne_trailing_comma() {
    #[derive(Debug, PartialEq)]
    struct Foo {
        lorem: &'static str,
        ipsum: u32,
        dolor: Result<String, String>,
    }

    let x = Some(Foo {
        lorem: "Hello World!",
        ipsum: 42,
        dolor: Ok("hey".to_string()),
    });

    assert_ne!(x, x,);
}

#[test]
#[should_panic(
    expected = r#"assertion failed: `(left != right)`: custom panic message

[1mBoth sides[0m:
Some(
    Foo {
        lorem: "Hello World!",
        ipsum: 42,
        dolor: Ok(
            "hey"
        )
    }
)

"#
)]
fn assert_ne_custom_trailing_comma() {
    #[derive(Debug, PartialEq)]
    struct Foo {
        lorem: &'static str,
        ipsum: u32,
        dolor: Result<String, String>,
    }

    let x = Some(Foo {
        lorem: "Hello World!",
        ipsum: 42,
        dolor: Ok("hey".to_string()),
    });

    assert_ne!(x, x, "custom panic message",);
}
