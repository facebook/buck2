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

//! Convert a value implementing [`StarlarkValue`] into a type usable in type expression.

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::marker::PhantomData;

use allocative::Allocative;
use starlark_derive::starlark_value;
use starlark_derive::NoSerialize;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::typing::Ty;
use crate::values::layout::avalue::alloc_static;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::Basic;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::AllocFrozenValue;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::StarlarkValue;

#[derive(Debug, NoSerialize, Allocative, ProvidesStaticType)]
struct StarlarkValueAsTypeStarlarkValue(fn() -> Ty);

#[starlark_value(type = "type")]
impl<'v> StarlarkValue<'v> for StarlarkValueAsTypeStarlarkValue {
    fn eval_type(&self) -> Option<Ty> {
        Some((self.0)())
    }
}

impl Display for StarlarkValueAsTypeStarlarkValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&(self.0)(), f)
    }
}

/// Utility to declare a value usable in type expression.
///
/// # Example
///
/// ```
/// use allocative::Allocative;
/// use starlark::environment::GlobalsBuilder;
/// use starlark::values::starlark_value_as_type::StarlarkValueAsType;
/// use starlark::values::StarlarkValue;
/// use starlark::any::ProvidesStaticType;
/// use starlark::values::{NoSerialize, starlark_value};
/// #[derive(Debug, derive_more::Display, Allocative, ProvidesStaticType, NoSerialize)]
/// struct Temperature;
///
/// #[starlark_value(type = "temperature")]
/// impl<'v> StarlarkValue<'v> for Temperature {
/// }
///
/// fn my_type_globals(globals: &mut GlobalsBuilder) {
///     // This can now be used like:
///     // ```
///     // def f(x: Temperature): pass
///     // ```
///     const Temperature: StarlarkValueAsType<Temperature> = StarlarkValueAsType::new();
/// }
/// ```
pub struct StarlarkValueAsType<T: StarlarkTypeRepr>(PhantomData<fn(&T)>);

impl<T: StarlarkTypeRepr> Debug for StarlarkValueAsType<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StarlarkValueAsType")
            .field(&T::starlark_type_repr())
            .finish()
    }
}

impl<T: StarlarkTypeRepr> Display for StarlarkValueAsType<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&T::starlark_type_repr(), f)
    }
}

impl<T: StarlarkTypeRepr> StarlarkValueAsType<T> {
    /// Constructor.
    pub const fn new() -> Self {
        Self(PhantomData)
    }

    const INSTANCE: AValueRepr<AValueImpl<Basic, StarlarkValueAsTypeStarlarkValue>> = alloc_static(
        Basic,
        StarlarkValueAsTypeStarlarkValue(T::starlark_type_repr),
    );
}

impl<T: StarlarkTypeRepr> Default for StarlarkValueAsType<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: StarlarkTypeRepr> StarlarkTypeRepr for StarlarkValueAsType<T> {
    fn starlark_type_repr() -> Ty {
        // TODO(nga): make it proper type.
        Ty::name_static("type")
    }
}

impl<T: StarlarkTypeRepr> AllocFrozenValue for StarlarkValueAsType<T> {
    fn alloc_frozen_value(self, _heap: &FrozenHeap) -> FrozenValue {
        FrozenValue::new_repr(&Self::INSTANCE)
    }
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use starlark_derive::starlark_module;
    use starlark_derive::starlark_value;
    use starlark_derive::NoSerialize;
    use starlark_derive::ProvidesStaticType;

    use crate as starlark;
    use crate::assert::Assert;
    use crate::environment::GlobalsBuilder;
    use crate::values::types::starlark_value_as_type::tests;
    use crate::values::types::starlark_value_as_type::StarlarkValueAsType;
    use crate::values::AllocValue;
    use crate::values::Heap;
    use crate::values::StarlarkValue;
    use crate::values::Value;

    #[derive(
        derive_more::Display,
        Debug,
        NoSerialize,
        Allocative,
        ProvidesStaticType
    )]
    struct CompilerArgs(String);

    #[starlark_value(type = "compiler_args")]
    impl<'v> StarlarkValue<'v> for CompilerArgs {}

    impl<'v> AllocValue<'v> for CompilerArgs {
        fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
            heap.alloc_simple(self)
        }
    }

    #[starlark_module]
    fn compiler_args_globals(globals: &mut GlobalsBuilder) {
        const CompilerArgs: StarlarkValueAsType<CompilerArgs> = StarlarkValueAsType::new();

        fn compiler_args(x: String) -> anyhow::Result<CompilerArgs> {
            Ok(tests::CompilerArgs(x))
        }
    }

    #[test]
    fn test_pass() {
        let mut a = Assert::new();
        a.globals_add(compiler_args_globals);
        a.pass(
            r#"
def f(x: CompilerArgs): pass

f(compiler_args("hello"))
        "#,
        );
    }

    #[test]
    fn test_fail_compile_time() {
        let mut a = Assert::new();
        a.globals_add(compiler_args_globals);
        a.fail(
            r#"
def g(x: CompilerArgs): pass

def h():
    g([])
"#,
            r#"Expected type `"compiler_args"` but got"#,
        );
    }

    #[test]
    fn test_fail_runtime() {
        let mut a = Assert::new();
        a.globals_add(compiler_args_globals);
        a.fail(
            r#"
def h(x: CompilerArgs): pass

noop(h)(1)
            "#,
            r#"Value `1` of type `int` does not match the type annotation"#,
        );
    }
}
