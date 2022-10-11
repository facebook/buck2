<p align="center"><img width="460" src="https://github.com/danaugrs/overload/blob/master/logo.png"></p>
<p align="center">
  <a href="https://docs.rs/overload"><img src="https://docs.rs/overload/badge.svg"/></a>
  <a href="https://crates.io/crates/overload"><img src="https://img.shields.io/crates/v/overload.svg"/></a>
</p>

Provides a macro to simplify operator overloading. See the [documentation](https://docs.rs/overload/) for details and supported operators.

## Example

```rust
extern crate overload;
use overload::overload;
use std::ops; // <- don't forget this or you'll get nasty errors

#[derive(PartialEq, Debug)]
struct Val {
    v: i32
}

overload!((a: ?Val) + (b: ?Val) -> Val { Val { v: a.v + b.v } });
```

The macro call in the snippet above generates the following code:

```rust
impl ops::Add<Val> for Val {
    type Output = Val;
    fn add(self, b: Val) -> Self::Output {
        let a = self;
        Val { v: a.v + b.v }
    }
}
impl ops::Add<&Val> for Val {
    type Output = Val;
    fn add(self, b: &Val) -> Self::Output {
        let a = self;
        Val { v: a.v + b.v }
    }
}
impl ops::Add<Val> for &Val {
    type Output = Val;
    fn add(self, b: Val) -> Self::Output {
        let a = self;
        Val { v: a.v + b.v }
    }
}
impl ops::Add<&Val> for &Val {
    type Output = Val;
    fn add(self, b: &Val) -> Self::Output {
        let a = self;
        Val { v: a.v + b.v }
    }
}
``` 

We are now able to add `Val`s and `&Val`s in any combination:

```rust
assert_eq!(Val{v:3} + Val{v:5}, Val{v:8});
assert_eq!(Val{v:3} + &Val{v:5}, Val{v:8});
assert_eq!(&Val{v:3} + Val{v:5}, Val{v:8});
assert_eq!(&Val{v:3} + &Val{v:5}, Val{v:8});
```
