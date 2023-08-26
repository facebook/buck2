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

//! A module with the standard function and constants that are by default in all
//! dialect of Starlark

use dupe::Dupe;

use crate::environment::GlobalsBuilder;

pub(crate) mod breakpoint;
pub(crate) mod dict;
pub(crate) mod enumeration;
pub(crate) mod extra;
mod funcs;
pub(crate) mod internal;
pub(crate) mod json;
pub(crate) mod list;
pub(crate) mod partial;
pub(crate) mod string;
pub(crate) mod structs;
pub(crate) mod util;

pub use extra::PrintHandler;

use crate::stdlib::funcs::globals::register_globals;
use crate::stdlib::internal::register_internal;
use crate::values::record::globals::register_record;
use crate::values::typing;

/// Return the default global environment, it is not yet frozen so that a caller
/// can refine it.
///
/// For example `stdlib::standard_environment().freeze().child("test")` create a
/// child environment of this global environment that have been frozen.
pub(crate) fn standard_environment() -> GlobalsBuilder {
    GlobalsBuilder::new().with(register_globals)
}

/// The extra library definitions available in this Starlark implementation, but not in the standard.
#[derive(PartialEq, Eq, Copy, Clone, Dupe)]
pub enum LibraryExtension {
    /// Definitions to support the `struct` type, the `struct()` constructor.
    StructType,
    /// Definitions to support the `record` type, the `record()` constructor and `field()` function.
    RecordType,
    /// Definitions to support the `enum` type, the `enum()` constructor.
    EnumType,
    /// A function `map(f, xs)` which applies `f` to each element of `xs` and returns the result.
    Map,
    /// A function `filter(f, xs)` which applies `f` to each element of `xs` and returns those for which `f` returns `True`.
    /// As a special case, `filter(None, xs)` removes all `None` values.
    Filter,
    /// Partially apply a function, `partial(f, *args, **kwargs)` will create a function where those `args` `kwargs`
    /// are already applied to `f`.
    Partial,
    /// Create a regex from a string.
    ExperimentalRegex,
    /// Add a function `debug(x)` which shows the Rust [`Debug`](std::fmt::Debug) representation of a value.
    /// Useful when debugging, but the output should not be considered stable.
    Debug,
    /// Add a function `print(x)` which prints to stderr.
    Print,
    /// Add a function `pprint(x)` which pretty-prints to stderr.
    Pprint,
    /// Add a function `breakpoint()` which will drop into a console-module evaluation prompt.
    Breakpoint,
    /// Add a function `json()` which will generate JSON for a module.
    Json,
    /// Add a function `abs()` which will take the absolute value of an int.
    Abs,
    /// `type_compiled()` function.
    Typing,
    /// Utilities exposing starlark-rust internals.
    /// These are not for production use.
    Internal,
    // Make sure if you add anything new, you add it to `all` below.
}

impl LibraryExtension {
    /// A list of all extensions that will be updated as new methods are added.
    pub(crate) fn all() -> &'static [Self] {
        use LibraryExtension::*;
        &[
            StructType,
            RecordType,
            EnumType,
            Map,
            Filter,
            Partial,
            ExperimentalRegex,
            Debug,
            Print,
            Pprint,
            Breakpoint,
            Json,
            Abs,
            Typing,
            Internal,
        ]
    }

    /// Add a specific extension to a [`GlobalsBuilder`].
    pub fn add(self, builder: &mut GlobalsBuilder) {
        use LibraryExtension::*;
        match self {
            StructType => structs::global(builder),
            RecordType => register_record(builder),
            EnumType => enumeration::global(builder),
            Map => extra::map(builder),
            Filter => extra::filter(builder),
            Partial => partial::partial(builder),
            ExperimentalRegex => extra::regex(builder),
            Debug => extra::debug(builder),
            Print => extra::print(builder),
            Pprint => extra::pprint(builder),
            Breakpoint => breakpoint::global(builder),
            Json => json::json(builder),
            Abs => extra::abs(builder),
            Typing => typing::globals::register_typing(builder),
            Internal => register_internal(builder),
        }
    }
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use derive_more::Display;
    use dupe::Dupe;
    use starlark_derive::starlark_module;
    use starlark_derive::starlark_value;
    use starlark_derive::NoSerialize;

    use crate as starlark;
    use crate::any::ProvidesStaticType;
    use crate::assert::Assert;
    use crate::environment::GlobalsBuilder;
    use crate::environment::Methods;
    use crate::environment::MethodsBuilder;
    use crate::environment::MethodsStatic;
    use crate::starlark_simple_value;
    use crate::values::none::NoneType;
    use crate::values::StarlarkValue;
    use crate::values::UnpackValue;
    use crate::values::Value;
    use crate::values::ValueLike;

    #[test]
    fn test_no_arg() {
        #[starlark_module]
        fn global(builder: &mut GlobalsBuilder) {
            fn nop() -> anyhow::Result<NoneType> {
                Ok(NoneType)
            }
        }

        let env = GlobalsBuilder::new().with(global).build();
        env.get("nop").unwrap();
    }

    #[test]
    fn test_value_attributes() {
        #[derive(
            Copy,
            Clone,
            Debug,
            Dupe,
            PartialEq,
            Display,
            ProvidesStaticType,
            NoSerialize,
            Allocative
        )]
        struct Bool2(bool);
        starlark_simple_value!(Bool2);

        #[starlark_value(type = "bool2")]
        impl<'v> StarlarkValue<'v> for Bool2 {
            fn get_methods() -> Option<&'static Methods> {
                static RES: MethodsStatic = MethodsStatic::new();
                RES.methods(methods)
            }

            fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
                match other.downcast_ref::<Bool2>() {
                    None => Ok(false),
                    Some(v) => Ok(*v == *self),
                }
            }
        }

        impl<'v> UnpackValue<'v> for Bool2 {
            fn unpack_value(value: Value<'v>) -> Option<Self> {
                Some(*value.downcast_ref::<Bool2>().unwrap())
            }
        }

        #[starlark_module]
        fn globals(builder: &mut GlobalsBuilder) {
            const True2: Bool2 = Bool2(true);
            const False2: Bool2 = Bool2(false);
        }

        #[starlark_module]
        fn methods(builder: &mut MethodsBuilder) {
            #[starlark(attribute)]
            fn invert1(this: Bool2) -> anyhow::Result<Bool2> {
                Ok(Bool2(!this.0))
            }

            fn invert2(this: Bool2) -> anyhow::Result<Bool2> {
                Ok(Bool2(!this.0))
            }
        }

        let mut a = Assert::new();
        a.globals_add(globals);
        a.all_true(
            r#"
True2 == True2
True2 != False2
True2.invert1 == False2
False2.invert1 == True2
False2.invert2() == True2
hasattr(True2, "invert1") == True
hasattr(True2, "invert2") == True
hasattr(True2, "invert3") == False
dir(False2) == ["invert1","invert2"]
getattr(False2, "invert1") == True2
getattr(True2, "invert1") == False2
getattr(True2, "invert2")() == False2
"#,
        );
    }
}
