/*
 * Copyright 2018 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! A [Starlark interpreter in Rust](https://github.com/facebookexperimental/starlark-rust).
//! Starlark is a deterministic version of Python, with [a specification](https://github.com/bazelbuild/starlark/blob/master/spec.md),
//! used by (amongst others) the [Buck](https://buck.build) and [Bazel](https://bazel.build) build systems.
//!
//! To evaluate a simple file:
//!
//! ```
//! # fn run() -> anyhow::Result<()> {
//! use starlark::eval::Evaluator;
//! use starlark::environment::{Module, Globals};
//! use starlark::values::Value;
//! use starlark::syntax::{AstModule, Dialect};
//!
//! let content = r#"
//! def hello():
//!    return "hello"
//! hello() + " world!"
//! "#;
//!
//! // We first parse the content, giving a filename and the Starlark
//! // `Dialect` we'd like to use (we pick standard).
//! let ast: AstModule = AstModule::parse("hello_world.star", content.to_owned(), &Dialect::Standard)?;
//!
//! // We create a `Globals`, defining the standard library functions available.
//! // The `standard` function uses those defined in the Starlark specification.
//! let globals: Globals = Globals::standard();
//!
//! // We create a `Module`, which stores the global variables for our calculation.
//! let module: Module = Module::new();
//!
//! // We create an evaluator, which controls how evaluation occurs.
//! let mut eval: Evaluator = Evaluator::new(&module);
//!
//! // And finally we evaluate the code using the evaluator.
//! let res: Value = eval.eval_module(ast, &globals)?;
//! assert_eq!(res.unpack_str(), Some("hello world!"));
//! # Ok(())
//! # }
//! # fn main(){ run().unwrap(); }
//! ```
//!
//! From this example there are lots of ways to extend it, which we do so below.
//!
//! ## Call Rust functions from Starlark
//!
//! We want to define a function in Rust (that computes quadratics), and then call it from Starlark.
//! We define the function using the [`#[starlark_module]`](macro@starlark_module) attribute, and add it to
//! a [`Globals`](environment::Globals) object.
//!
//! ```
//! #![feature(box_syntax)]
//! #[macro_use]
//! extern crate starlark;
//! # fn run() -> anyhow::Result<()> {
//! use starlark::environment::{GlobalsBuilder, Module};
//! use starlark::eval::Evaluator;
//! use starlark::syntax::{AstModule, Dialect};
//! use starlark::values::Value;
//!
//! // This defines the function that is visible to Starlark
//! #[starlark_module]
//! fn starlark_quadratic(builder: &mut GlobalsBuilder) {
//!     fn quadratic(a: i32, b: i32, c: i32, x: i32) -> anyhow::Result<i32> {
//!         Ok(a * x * x + b * x + c)
//!     }
//! }
//!
//! // We build our globals to make the function available in Starlark
//! let globals = GlobalsBuilder::new().with(starlark_quadratic).build();
//! let module = Module::new();
//! let mut eval = Evaluator::new(&module);
//!
//! // Let's test calling the function from Starlark code
//! let starlark_code = r#"
//! quadratic(4, 2, 1, x = 8)
//! "#;
//!
//! let ast = AstModule::parse("quadratic.star", starlark_code.to_owned(), &Dialect::Standard)?;
//! let res = eval.eval_module(ast, &globals)?;
//! assert_eq!(res.unpack_int(), Some(273)); // Verify that we got an `int` return value of 4 * 8^2 + 2 * 8 + 1 = 273
//! # Ok(())
//! # }
//! # fn main(){ run().unwrap(); }
//! ```
//!
//! ## Collect Starlark values
//!
//! If we want to use Starlark as an enhanced JSON, we can define an `emit` function
//! to "write out" a JSON value, and use the [`Evaluator.extra`](eval::Evaluator::extra) field to store it.
//!
//! ```
//! #![feature(box_syntax)]
//! #[macro_use]
//! extern crate starlark;
//! # fn run() -> anyhow::Result<()> {
//! use starlark::environment::{GlobalsBuilder, Module};
//! use starlark::eval::Evaluator;
//! use starlark::syntax::{AstModule, Dialect};
//! use starlark::values::{none::NoneType, Value, ValueLike};
//! use gazebo::any::ProvidesStaticType;
//! use std::cell::RefCell;
//!
//! let content = r#"
//! emit(1)
//! emit(["test"])
//! emit({"x": "y"})
//! "#;
//!
//! // Define a store in which to accumulate JSON strings
//! #[derive(Debug, ProvidesStaticType, Default)]
//! struct Store(RefCell<Vec<String>>);
//!
//! impl Store {
//!     fn add(&self, x: String) {
//!          self.0.borrow_mut().push(x)
//!     }
//! }
//!
//! #[starlark_module]
//! fn starlark_emit(builder: &mut GlobalsBuilder) {
//!     fn emit(x: Value, eval: &mut Evaluator) -> anyhow::Result<NoneType> {
//!         // We modify extra (which we know is a Store) and add the JSON of the
//!         // value the user gave.
//!         eval.extra
//!             .unwrap()
//!             .downcast_ref::<Store>()
//!             .unwrap()
//!             .add(x.to_json()?);
//!         Ok(NoneType)
//!     }
//! }
//!
//! let ast = AstModule::parse("json.star", content.to_owned(), &Dialect::Standard)?;
//! // We build our globals adding some functions we wrote
//! let globals = GlobalsBuilder::new().with(starlark_emit).build();
//! let module = Module::new();
//! let mut eval = Evaluator::new(&module);
//! // We add a reference to our store
//! let store = Store::default();
//! eval.extra = Some(&store);
//! eval.eval_module(ast, &globals)?;
//! assert_eq!(&*store.0.borrow(), &["1", "[\"test\"]", "{\"x\":\"y\"}"]);
//! # Ok(())
//! # }
//! # fn main(){ run().unwrap(); }
//! ```
//!
//! ## Enable Starlark extensions (e.g. types)
//!
//! Our Starlark supports a number of extensions, including type annotations, which are
//! controlled by the [`Dialect`](syntax::Dialect) type.
//!
//! ```
//! # fn run() -> anyhow::Result<()> {
//! use starlark::environment::{Globals, Module};
//! use starlark::eval::Evaluator;
//! use starlark::syntax::{AstModule, Dialect, DialectTypes};
//!
//! let content = r#"
//! def takes_int(x: int.type):
//!     pass
//! takes_int("test")
//! "#;
//!
//! // Make the dialect enable types
//! let dialect = Dialect {enable_types: DialectTypes::Enable, ..Dialect::Standard};
//! // We could equally have done `dialect = Dialect::Extended`.
//! let ast = AstModule::parse("json.star", content.to_owned(), &dialect)?;
//! let globals = Globals::standard();
//! let module = Module::new();
//! let mut eval = Evaluator::new(&module);
//! let res = eval.eval_module(ast, &globals);
//! // We expect this to fail, since it is a type violation
//! assert!(res.unwrap_err().to_string().contains("Value `test` of type `string` does not match the type annotation `int`"));
//! # Ok(())
//! # }
//! # fn main(){ run().unwrap(); }
//! ```
//!
//! ## Enable the `load` statement
//!
//! You can have Starlark load files imported by the user.
//! That requires that the loaded modules are first frozen with [`Module.freeze`](environment::Module::freeze).
//! There is no requirement that the files are on disk, but that would be a common pattern.
//!
//! ```
//! # fn run() -> anyhow::Result<()> {
//! use starlark::environment::{FrozenModule, Globals, Module};
//! use starlark::eval::{Evaluator, ReturnFileLoader};
//! use starlark::syntax::{AstModule, Dialect};
//!
//! // Get the file contents (for the demo), in reality use `AstModule::parse_file`.
//! fn get_source(file: &str) -> &str {
//!     match file {
//!         "a.star" => "a = 7",
//!         "b.star" => "b = 6",
//!         _ => { r#"
//! load('a.star', 'a')
//! load('b.star', 'b')
//! ab = a * b
//! "#
//!         }
//!     }
//! }
//!
//! fn get_module(file: &str) -> anyhow::Result<FrozenModule> {
//!    let ast = AstModule::parse(file, get_source(file).to_owned(), &Dialect::Standard)?;
//!
//!    // We can get the loaded modules from `ast.loads`.
//!    // And ultimately produce a `loader` capable of giving those modules to Starlark.
//!    let mut loads = Vec::new();
//!    for load in ast.loads() {
//!        loads.push((load.to_owned(), get_module(load)?));
//!    }
//!    let modules = loads.iter().map(|(a, b)| (a.as_str(), b)).collect();
//!    let mut loader = ReturnFileLoader { modules: &modules };
//!
//!    let globals = Globals::standard();
//!    let module = Module::new();
//!    let mut eval = Evaluator::new(&module);
//!    eval.set_loader(&mut loader);
//!    eval.eval_module(ast, &globals)?;
//!    // After creating a module we freeze it, preventing further mutation.
//!    // It can now be used as the input for other Starlark modules.
//!    Ok(module.freeze()?)
//! }
//!
//! let ab = get_module("ab.star")?;
//! assert_eq!(ab.get("ab").unwrap().unpack_int(), Some(42));
//! # Ok(())
//! # }
//! # fn main(){ run().unwrap(); }
//! ```
//!
//! ## Call a Starlark function from Rust
//!
//! You can extract functions from Starlark, and call them from Rust, using [`eval_function`](eval::Evaluator::eval_function).
//!
//! ```
//! # fn run() -> anyhow::Result<()> {
//! use starlark::environment::{Globals, Module};
//! use starlark::eval::Evaluator;
//! use starlark::syntax::{AstModule, Dialect};
//! use starlark::values::Value;
//!
//! let content = r#"
//! def quadratic(a, b, c, x):
//!     return a*x*x + b*x + c
//! quadratic
//! "#;
//!
//! let ast = AstModule::parse("quadratic.star", content.to_owned(), &Dialect::Extended)?;
//! let globals = Globals::standard();
//! let module = Module::new();
//! let mut eval = Evaluator::new(&module);
//! let quad = eval.eval_module(ast, &globals)?;
//! let res = eval.eval_function(
//!     quad,
//!     &[Value::new_int(4), Value::new_int(2), Value::new_int(1)],
//!     &[("x", Value::new_int(8))],
//! )?;
//! assert_eq!(res.unpack_int(), Some(273));
//! # Ok(())
//! # }
//! # fn main(){ run().unwrap(); }
//! ```
//!
//! ## Defining Rust objects that are used from Starlark
//!
//! Finally, we can define our own types in Rust which live in the Starlark heap.
//! Such types are relatively complex, see the details at [`StarlarkValue`](values::StarlarkValue).
//!
//! ```
//! # fn run() -> anyhow::Result<()> {
//! use starlark::environment::{Globals, Module};
//! use starlark::eval::Evaluator;
//! use starlark::syntax::{AstModule, Dialect};
//! use starlark::values::{Heap, StarlarkValue, Value, ValueError, ValueLike, ProvidesStaticType, NoSerialize};
//! use starlark::{starlark_type, starlark_simple_value};
//! use std::fmt::{self, Display, Write};
//!
//! // Define complex numbers
//! #[derive(Debug, PartialEq, Eq, ProvidesStaticType, NoSerialize)]
//! struct Complex {
//!     real: i32,
//!     imaginary: i32,
//! }
//! starlark_simple_value!(Complex);
//!
//! impl Display for Complex {
//!     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//!         write!(f, "{} + {}i", self.real, self.imaginary)
//!     }
//! }
//!
//! impl<'v> StarlarkValue<'v> for Complex {
//!     starlark_type!("complex");
//!
//!     // How we add them
//!     fn add(&self, rhs: Value<'v>, heap: &'v Heap)
//!             -> Option<anyhow::Result<Value<'v>>> {
//!         if let Some(rhs) = rhs.downcast_ref::<Self>() {
//!             Some(Ok(heap.alloc(Complex {
//!                 real: self.real + rhs.real,
//!                 imaginary: self.imaginary + rhs.imaginary,
//!             })))
//!         } else {
//!             None
//!         }
//!     }
//! }
//!
//! let content = "str(a + b)";
//!
//! let ast = AstModule::parse("complex.star", content.to_owned(), &Dialect::Standard)?;
//! let globals = Globals::standard();
//! let module = Module::new();
//! // We inject some complex numbers into the module before we start.
//! let a = module.heap().alloc(Complex {real: 1, imaginary: 8});
//! module.set("a", a);
//! let b = module.heap().alloc(Complex {real: 4, imaginary: 2});
//! module.set("b", b);
//! let mut eval = Evaluator::new(&module);
//! let res = eval.eval_module(ast, &globals)?;
//! assert_eq!(res.unpack_str(), Some("5 + 10i"));
//! # Ok(())
//! # }
//! # fn main(){ run().unwrap(); }
//! ```

// Features we use
#![allow(stable_features)]
#![feature(alloc_layout_extra)]
#![feature(backtrace)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(cell_filter_map)]
#![feature(const_fn_fn_ptr_basics)]
#![feature(const_fn_trait_bound)]
#![feature(const_intrinsic_copy)]
#![feature(const_mut_refs)]
#![feature(const_ptr_offset)]
#![feature(const_refs_to_cell)]
#![feature(const_type_id)]
#![feature(core_intrinsics)]
#![feature(generic_associated_types)]
#![feature(hash_set_entry)]
#![feature(iter_advance_by)]
#![feature(maybe_uninit_slice)]
#![feature(maybe_uninit_write_slice)]
#![feature(ptr_metadata)]
#![feature(thread_local)]
#![feature(try_blocks)]
#![feature(iter_intersperse)]
#![feature(array_from_fn)]
#![feature(io_error_more)]
#![feature(result_option_inspect)]
//
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]
//
// Good reasons
#![allow(clippy::needless_return)] // Mixing explicit returns with implicit ones sometimes looks odd
#![allow(clippy::new_ret_no_self)] // We often return Value, even though its morally a Self
// Disagree these are good hints
#![allow(clippy::comparison_chain)]
#![allow(clippy::comparison_to_empty)]
#![allow(clippy::enum_variant_names)]
#![allow(clippy::float_cmp)]
#![allow(clippy::len_without_is_empty)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::needless_lifetimes)]
#![allow(clippy::new_without_default)]
#![allow(clippy::should_implement_trait)]
#![allow(clippy::single_match)]
#![allow(clippy::type_complexity)]
#![allow(clippy::wrong_self_convention)]
// FIXME: Temporary
#![allow(clippy::useless_transmute)] // Seems to be a clippy bug, but we should be using less transmute anyway
#![deny(missing_docs)]

#[macro_use]
extern crate gazebo;
#[macro_use]
extern crate starlark_derive;
#[macro_use]
extern crate maplit;
#[macro_use]
mod macros;

pub use starlark_derive::starlark_module;
pub use starlark_derive::StarlarkDocs;

pub(crate) mod analysis;
pub mod assert;
pub mod codemap;
pub mod collections;
mod debug;
pub mod environment;
pub mod errors;
pub mod eval;
pub mod lsp;
mod private;
pub mod read_line;
mod sealed;

mod stdlib;
pub mod syntax;
pub mod values;

#[cfg(test)]
mod tests;

/// __macro_refs allows us to reference other crates in macro rules without users needing to be
///  aware of those dependencies. We make them public here and then can reference them like
///  `$crate::__macro_refs::foo`.
#[doc(hidden)]
pub mod __macro_refs {
    pub use either::Either;
    pub use gazebo::coerce::coerce;
    pub use paste::item;
}

/// __derive_refs allows us to reference other crates in starlark_derive without users needing to be
///  aware of those dependencies. We make them public here and then can reference them like
///  `starlark::__derive_refs::foo`.
#[doc(hidden)]
pub mod __derive_refs {
    pub mod serde {
        pub use serde::ser::Error;
        pub use serde::Serialize;
        pub use serde::Serializer;
    }
    pub use inventory;
}
