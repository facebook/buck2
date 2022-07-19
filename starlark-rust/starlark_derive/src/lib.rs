/*
 * Copyright 2019 The Starlark in Rust Authors.
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

//! A proc-macro for writing functions in Rust that can be called from Starlark.

#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]
#![feature(box_patterns)]

#[allow(unused_extern_crates)] // proc_macro is very special
extern crate proc_macro;

use proc_macro::TokenStream;

mod attrs;
mod bc;
mod docs;
mod for_each_field;
mod freeze;
mod module;
mod serde;
mod trace;
mod visit_span;
mod vtable;

/// Write Starlark modules concisely in Rust syntax.
///
/// For example:
///
/// ```ignore
/// #[starlark_module]
/// fn global(registry: &mut GlobalsBuilder) {
///     fn cc_binary(name: &str, srcs: Vec<&str>) -> String {
///         Ok(format!("{:?} {:?}", name, srcs))
///     }
/// }
/// ```
///
/// Parameters operate as named parameters of a given type, with six possible tweaks:
///
/// * `this` (or `_this`) as the first argument means the argument is passed as a
///   bound method value, e.g. in `a.f(...)` the `a` would be `this`.
/// * `args` means the argument is the `*args`.
/// * `kwargs` means the argument is the `**kwargs`.
/// * `ref name` means the argument must be passed by position, not by name.
/// * A type of `Option` means the argument is optional.
/// * A annotation `#[starlark(default = foo)] x : bool` means the argument defaults to `foo`
///   if not specified.
///
/// During execution there are two local variables injected into scope:
///
/// * `eval` is the `Evaluator`.
/// * `heap` is the `Heap`, obtained from `eval.heap()`.
///
/// A function with the `#[starlark_module]` attribute can be added to a `GlobalsBuilder` value
/// using the `with` function. Those `Globals` can be passed to `Evaluator` to provide global functions.
/// Alternatively, you can return `Globals` from `get_methods` to _attach_ functions to
/// a specific type (e.g. the `string` type).
///
/// * When unattached, you can define constants with `const`. We define `True`, `False` and
///   `None` that way.
/// * When attached, you can annotate the functions with `#[starlark(attribute)]` to turn the name into
///   an attribute on the value. Such a function must take exactly one argument, namely a value
///   of the type you have attached it to.
/// * The attribute `#[starlark(type = "test")]` causes `f.type` to return `"test"`.
/// * If a member is annotated with `#[starlark(speculative_exec_safe)]`, then a function
///   is considered safe to execute speculatively: the function should have
///   no global side effects, should not panic, and should finish in reasonable time.
///   The evaluator may invoke such functions early to generate more efficient code.
///
/// All these functions interoperate properly with `dir()`, `getattr()` and `hasattr()`.
///
/// If a desired function name is also a Rust keyword, use the `r#` prefix, e.g. `r#type`.
#[proc_macro_attribute]
pub fn starlark_module(attr: TokenStream, input: TokenStream) -> TokenStream {
    module::starlark_module(attr, input)
}

/// Stubs for Starlark bytecode interpreter.
#[proc_macro_attribute]
pub fn starlark_internal_bc(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    bc::starlark_internal_bc(attr, input)
}

#[proc_macro_attribute]
pub fn starlark_internal_vtable(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    vtable::starlark_internal_vtable(attr, input)
}

#[proc_macro_derive(VisitSpanMut)]
pub fn derive_visit_span_mut(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    visit_span::derive_visit_span_mut(input)
}

/// Derive the `Trace` trait.
#[proc_macro_derive(Trace, attributes(trace))]
pub fn derive_trace(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    trace::derive_trace(input)
}

/// Derive the `Freeze` trait.
#[proc_macro_derive(Freeze, attributes(freeze))]
pub fn derive_freeze(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    freeze::derive_freeze(input)
}

/// Derive the `NoSerialize` trait for serde.
#[proc_macro_derive(NoSerialize)]
pub fn derive_no_serialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    serde::derive_no_serialize(input)
}

/// Derive accessor methods that are designed to be used from {has,get,dir}_attr
/// in an `impl StarlarkValue` block. All fields in the struct that are not
/// marked with #[starlark(skip)] are exported to Starlark code as attributes.
/// NOTE: Any usage must also call `starlark_attrs!()` in the impl block for
/// `StarlarkValue`, otherwise the generated attr methods will not be used.
#[proc_macro_derive(StarlarkAttrs, attributes(starlark))]
pub fn derive_starlark_attrs(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    attrs::derive_attrs(input)
}

/// Generate an accessor function on the provided type that returns its documentation
/// based on `StarlarkValue::get_methods()`. This macro requires that the type implements
/// `starlark::StarlarkValue`.
///
/// #[starlark_docs_attrs(key="value", second_key="second_value",...)] can be used to insert
/// arbitrary keys and string values into the generated `Docs::custom_attrs` for use
/// by documentation tooling.
#[proc_macro_derive(StarlarkDocs, attributes(starlark_docs_attrs))]
pub fn derive_starlark_docs(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    docs::derive_docs(input)
}

/// Generate `{has,get,dir}_attr` in the `StarlarkValue` impl block that proxy
/// to the ones generated by `derive(StarlarkAttrs)`
#[proc_macro]
pub fn starlark_attrs(_: proc_macro::TokenStream) -> proc_macro::TokenStream {
    attrs::starlark_attrs()
}
