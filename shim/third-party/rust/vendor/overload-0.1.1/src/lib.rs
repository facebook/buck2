//! Provides a macro to simplify operator overloading.
//! 
//! To use, include the following:
//! ```
//! extern crate overload;
//! use overload::overload;
//! use std::ops; // <- don't forget this or you'll get nasty errors
//! ```
//! 
//! # Introduction
//! 
//! Suppose we have the following `struct` definition:
//! ``` 
//! #[derive(PartialEq, Debug)]
//! struct Val {
//!     v: i32
//! }
//! ```
//! We can overload the addition of `Val`s like so:
//! ```
//! # extern crate overload;
//! # use overload::overload;
//! # use std::ops;
//! # #[derive(PartialEq, Debug)]
//! # struct Val {
//! #   v: i32
//! # }
//! overload!((a: Val) + (b: Val) -> Val { Val { v: a.v + b.v } });
//! ```
//! The macro call above generates the following code:
//! ```ignore
//! impl ops::Add<Val> for Val {
//!     type Output = Val;
//!     fn add(self, b: Val) -> Self::Output {
//!         let a = self;
//!         Val { v: a.v + b.v }
//!     }
//! }
//! ```
//! We are now able to add `Val`s:
//! ```
//! # extern crate overload;
//! # use overload::overload;
//! # use std::ops;
//! # #[derive(PartialEq, Debug)]
//! # struct Val {
//! #   v: i32
//! # }
//! # overload!((a: Val) + (b: Val) -> Val { Val { v: a.v + b.v } });
//! assert_eq!(Val{v:3} + Val{v:5}, Val{v:8});
//! ```
//! 
//! # Owned and borrowed types
//! 
//! If we also wanted to overload addition for the borrowed type `&Val` we could write:
//! ```
//! # extern crate overload;
//! # use overload::overload;
//! # use std::ops;
//! # #[derive(PartialEq, Debug)]
//! # struct Val {
//! #   v: i32
//! # }
//! overload!((a: &Val) + (b: &Val) -> Val { Val { v: a.v + b.v } });
//! ```
//! We might also want to overload addition between the owned and borrowed types:
//! ```
//! # extern crate overload;
//! # use overload::overload;
//! # use std::ops;
//! # #[derive(PartialEq, Debug)]
//! # struct Val {
//! #   v: i32
//! # }
//! overload!((a: Val) + (b: &Val) -> Val { Val { v: a.v + b.v } });
//! overload!((a: &Val) + (b: Val) -> Val { Val { v: a.v + b.v } });
//! ```
//! Let's see how we can write these combinations more concisely.
//! 
//! We can include a `?` in front of a type to indicate that it should stand in for both the owned and borrowed type.
//! 
//! To overload addition for all four combinations between `Val` and `&Val` we can therefore simply include a `?` in front of both types:
//! ```
//! # extern crate overload;
//! # use overload::overload;
//! # use std::ops;
//! # #[derive(PartialEq, Debug)]
//! # struct Val {
//! #   v: i32
//! # }
//! overload!((a: ?Val) + (b: ?Val) -> Val { Val { v: a.v + b.v } });
//! ```
//! The macro call above generates the following code:
//! ```ignore
//! impl ops::Add<Val> for Val {
//!     type Output = Val;
//!     fn add(self, b: Val) -> Self::Output {
//!         let a = self;
//!         Val { v: a.v + b.v }
//!     }
//! }
//! 
//! impl ops::Add<&Val> for Val {
//!     type Output = Val;
//!     fn add(self, b: &Val) -> Self::Output {
//!         let a = self;
//!         Val { v: a.v + b.v }
//!     }
//! }
//! 
//! impl ops::Add<Val> for &Val {
//!     type Output = Val;
//!     fn add(self, b: Val) -> Self::Output {
//!         let a = self;
//!         Val { v: a.v + b.v }
//!     }
//! }
//! 
//! impl ops::Add<&Val> for &Val {
//!     type Output = Val;
//!     fn add(self, b: &Val) -> Self::Output {
//!         let a = self;
//!         Val { v: a.v + b.v }
//!     }
//! }
//! ``` 
//! We are now able to add `Val`s and `&Val`s in any combination:
//! ```
//! # extern crate overload;
//! # use overload::overload;
//! # use std::ops;
//! # #[derive(PartialEq, Debug)]
//! # struct Val {
//! #   v: i32
//! # }
//! # overload!((a: ?Val) + (b: ?Val) -> Val { Val { v: a.v + b.v } });
//! assert_eq!(Val{v:3} + Val{v:5}, Val{v:8});
//! assert_eq!(Val{v:3} + &Val{v:5}, Val{v:8});
//! assert_eq!(&Val{v:3} + Val{v:5}, Val{v:8});
//! assert_eq!(&Val{v:3} + &Val{v:5}, Val{v:8});
//! ```
//!
//! # Binary operators
//! 
//! The general syntax to overload a binary operator between types `<a_type>` and `<b_type>` is:
//! ```ignore
//! overload!((<a_ident>: <a_type>) <op> (<b_ident>: <b_type>) -> <out_type> { /*body*/ });
//! ```
//! Inside the body you can use `<a_ident>` and `<b_ident>` freely to perform any computation.
//! 
//! The last line of the body needs to be an expression (i.e. no `;` at the end of the line) of type `<out_type>`.
//! 
//! | Operator | Example                                                         | Trait  |
//! |----------|-----------------------------------------------------------------|--------|
//! | +        | `overload!((a: A) + (b: B) -> C { /*...*/ );`                   | Add    |           
//! | -        | `overload!((a: A) - (b: B) -> C { /*...*/ );`                   | Sub    |
//! | *        | `overload!((a: A) * (b: B) -> C { /*...*/ );`                   | Mul    |
//! | /        | `overload!((a: A) / (b: B) -> C { /*...*/ );`                   | Div    |
//! | %        | `overload!((a: A) % (b: B) -> C { /*...*/ );`                   | Rem    |
//! | &        | `overload!((a: A) & (b: B) -> C { /*...*/ );`                   | BitAnd |
//! | \|       | <code>overload!((a: A) &vert; (b: B) -> C { /\*...*\/ );</code> | BitOr  |
//! | ^        | `overload!((a: A) ^ (b: B) -> C { /*...*/ );`                   | BitXor |
//! | <<       | `overload!((a: A) << (b: B) -> C { /*...*/ );`                  | Shl    |
//! | >>       | `overload!((a: A) >> (b: B) -> C { /*...*/ );`                  | Shr    |
//! 
//! # Assignment operators
//! 
//! The general syntax to overload an assignment operator between types `<a_type>` and `<b_type>` is:
//! ```ignore
//! overload!((<a_ident>: &mut <a_type>) <op> (<b_ident>: <b_type>) { /*body*/ });
//! ```
//! Inside the body you can use `<a_ident>` and `<b_ident>` freely to perform any computation and mutate `<a_ident>` as desired.
//! 
//! | Operator | Example                                                          | Trait        |
//! |----------|------------------------------------------------------------------|--------------|
//! | +=       | `overload!((a: &mut A) += (b: B) { /*...*/ );`                   | AddAssign    |           
//! | -=       | `overload!((a: &mut A) -= (b: B) { /*...*/ );`                   | SubAssign    |
//! | *=       | `overload!((a: &mut A) *= (b: B) { /*...*/ );`                   | MulAssign    |
//! | /=       | `overload!((a: &mut A) /= (b: B) { /*...*/ );`                   | DivAssign    |
//! | %=       | `overload!((a: &mut A) %= (b: B) { /*...*/ );`                   | RemAssign    |
//! | &=       | `overload!((a: &mut A) &= (b: B) { /*...*/ );`                   | BitAndAssign |
//! | \|=      | <code>overload!((a: &mut A) &vert;= (b: B) { /\*...*\/ );</code> | BitOrAssign  |
//! | ^=       | `overload!((a: &mut A) ^= (b: B) { /*...*/ );`                   | BitXorAssign |
//! | <<=      | `overload!((a: &mut A) <<= (b: B) { /*...*/ );`                  | ShlAssign    |
//! | >>=      | `overload!((a: &mut A) >>= (b: B) { /*...*/ );`                  | ShrAssign    |
//! 
//! # Unary operators
//! 
//! The general syntax to overload a unary operator for type `<a_type>` is:
//! ```ignore
//! overload!(<op> (<a_ident>: <a_type>) -> <out_type> { /*body*/ });
//! ```
//! Inside the body you can use `<a_ident>` freely to perform any computation.
//! 
//! The last line of the body needs to be an expression (i.e. no `;` at the end of the line) of type `<out_type>`.
//! 
//! | Operator | Example                                                 | Trait |
//! |----------|---------------------------------------------------------|-------|
//! | -        | `overload!(- (a: A) -> B { /*...*/ );`                  | Neg   |
//! | !        | `overload!(! (a: A) -> B { /*...*/ );`                  | Not   |  
//! 
//! # Notes
//! 
//! Remember that you can only overload operators between one or more types if at least one of the types is defined in the current crate.

#[macro_use]
mod unary;

#[macro_use]
mod assignment;

#[macro_use]
mod binary;

/// Overloads an operator. See the [module level documentation](index.html) for more information.
#[macro_export(local_inner_macros)]
macro_rules! overload {
    // Unary (both owned and borrowed)
    ($op:tt ($i:ident : ? $t:ty) -> $out:ty $body:block) => (
        _overload_unary!($op, $i, $t, $out, $body);
        _overload_unary!($op, $i, &$t, $out, $body);
    );
    // Unary (either owned or borrowed)
    ($op:tt ($i:ident : $t:ty) -> $out:ty $body:block) => (
        _overload_unary!($op, $i, $t, $out, $body);
    );
    // Assignment (both owned and borrowed)
    (($li:ident : &mut $lt:ty) $op:tt ($ri:ident : ? $rt:ty) $body:block) => (
        _overload_assignment!($op, $li, $lt, $ri, $rt, $body);
        _overload_assignment!($op, $li, $lt, $ri, &$rt, $body);
    );
    // Assignment (either owned or borrowed)
    (($li:ident : &mut $lt:ty) $op:tt ($ri:ident : $rt:ty) $body:block) => (
        _overload_assignment!($op, $li, $lt, $ri, $rt, $body);
    );    
    // Binary (both - both)
    (($li:ident : ? $lt:ty) $op:tt ($ri:ident : ? $rt:ty) -> $out:ty $body:block) => (
        _overload_binary!($op, $li, $lt, $ri, $rt, $out, $body);
        _overload_binary!($op, $li, $lt, $ri, &$rt, $out, $body);
        _overload_binary!($op, $li, &$lt, $ri, $rt, $out, $body);
        _overload_binary!($op, $li, &$lt, $ri, &$rt, $out, $body);
    );
    // Binary (both - either)
    (($li:ident : ? $lt:ty) $op:tt ($ri:ident : $rt:ty) -> $out:ty $body:block) => (
        _overload_binary!($op, $li, $lt, $ri, $rt, $out, $body);
        _overload_binary!($op, $li, &$lt, $ri, $rt, $out, $body);
    );
    // Binary (either - both)
    (($li:ident : $lt:ty) $op:tt ($ri:ident : ? $rt:ty) -> $out:ty $body:block) => (
        _overload_binary!($op, $li, $lt, $ri, $rt, $out, $body);
        _overload_binary!($op, $li, $lt, $ri, &$rt, $out, $body);
    );
    // Binary (either - either)
    (($li:ident : $lt:ty) $op:tt ($ri:ident : $rt:ty) -> $out:ty $body:block) => (
        _overload_binary!($op, $li, $lt, $ri, $rt, $out, $body);
    );
}
