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

use dupe::Dupe;
use once_cell::sync::OnceCell;
use starlark_map::Hashed;

use crate::__derive_refs::components::NativeCallableComponents;
use crate::collections::symbol::map::SymbolMap;
use crate::collections::symbol::symbol::Symbol;
use crate::docs::DocType;
use crate::environment::common_documentation;
use crate::eval::ParametersSpec;
use crate::typing::Ty;
use crate::values::AllocFrozenValue;
use crate::values::FrozenHeap;
use crate::values::FrozenHeapRef;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::Value;
use crate::values::function::NativeAttribute;
use crate::values::function::NativeMeth;
use crate::values::function::NativeMethFn;
use crate::values::function::NativeMethod;
use crate::values::layout::heap::heap_type::FrozenHeapName;
use crate::values::types::unbound::UnboundValue;

/// Methods of an object.
#[derive(Clone, Debug)]
pub struct Methods {
    /// This field holds the objects referenced in `members`.
    heap: FrozenHeapRef,
    members: SymbolMap<UnboundValue>,
    docstring: Option<String>,
}

/// Heap name for a [`Methods`] object, used for heap graph tracking.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct MethodFrozenHeapName {
    /// A name identifying this methods heap (e.g. type name like "dict",
    /// or a module path like "starlark::values::types::dict::methods::dict_methods").
    pub name: &'static str,
}

impl std::fmt::Display for MethodFrozenHeapName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "methods({})", self.name)
    }
}

/// Used to build a [`Methods`] value.
#[derive(Debug)]
pub struct MethodsBuilder {
    /// The heap everything is allocated in.
    heap: FrozenHeap,
    /// Members, either `NativeMethod` or `NativeAttribute`.
    members: SymbolMap<UnboundValue>,
    /// The raw docstring for the main object.
    ///
    /// FIXME(JakobDegen): This should probably be removed. Not only can these docstrings not be
    /// combined with each other, but having the main documentation for the object on the methods
    /// instead of on the object type directly is extraordinarily confusing.
    docstring: Option<String>,
    /// Heap name for identification in heap graph tracking.
    heap_name: Option<MethodFrozenHeapName>,
}

impl Methods {
    pub(crate) fn get<'v>(&'v self, name: &str) -> Option<Value<'v>> {
        Some(self.members.get_str(name)?.to_frozen_value().to_value())
    }

    /// Gets the type of the member
    ///
    /// In the case of an attribute, this is the type the attribute evaluates to, while in the case
    /// of a method, this is the `TyCallable`
    pub(crate) fn get_ty(&self, name: &str) -> Option<Ty> {
        match self.members.get_str(name)? {
            UnboundValue::Attr(attr) => Some(attr.typ.dupe()),
            UnboundValue::Method(method) => Some(method.ty.dupe()),
        }
    }

    #[inline]
    pub(crate) fn get_hashed(&self, name: Hashed<&str>) -> Option<&UnboundValue> {
        self.members.get_hashed_str(name)
    }

    #[inline]
    pub(crate) fn get_frozen_symbol(&self, name: &Symbol) -> Option<&UnboundValue> {
        self.members.get(name)
    }

    pub(crate) fn names(&self) -> Vec<String> {
        self.members.keys().map(|x| x.as_str().to_owned()).collect()
    }

    pub(crate) fn members(&self) -> impl Iterator<Item = (&str, FrozenValue)> {
        self.members
            .iter()
            .map(|(k, v)| (k.as_str(), v.to_frozen_value()))
    }

    /// Fetch the documentation.
    pub fn documentation(&self, ty: Ty) -> DocType {
        let (docs, members) = common_documentation(
            &self.docstring,
            self.members
                .iter()
                .map(|(n, v)| (n.as_str(), v.to_frozen_value())),
        );

        DocType {
            docs,
            members: members
                .filter_map(|(n, item)| {
                    // This is only `None` if the item is a module, but types shouldn't really have
                    // modules in them anyway, so that seems ok
                    Some((n, item.try_as_member_with_collapsed_object().ok()?))
                })
                .collect(),
            ty,
            constructor: None,
        }
    }
}

impl Methods {
    /// Create an empty [`Methods`], with no functions in scope.
    pub fn new() -> Self {
        MethodsBuilder::new().build()
    }
}

impl MethodsBuilder {
    /// Create an empty [`MethodsBuilder`], with no functions in scope.
    pub fn new() -> Self {
        MethodsBuilder {
            heap: FrozenHeap::new(),
            members: SymbolMap::new(),
            docstring: None,
            heap_name: None,
        }
    }

    /// Called at the end to build a [`Methods`].
    pub fn build(self) -> Methods {
        let heap = self
            .heap
            .into_ref_impl(self.heap_name.map(FrozenHeapName::Method), None);
        Methods {
            heap,
            members: self.members,
            docstring: self.docstring,
        }
    }

    /// A fluent API for modifying [`MethodsBuilder`] and returning the result.
    pub fn with(mut self, f: impl FnOnce(&mut Self)) -> Self {
        f(&mut self);
        self
    }

    /// Set the raw docstring for this object.
    pub fn set_docstring(&mut self, docstring: &str) {
        self.docstring = Some(docstring.to_owned());
    }

    /// Set a constant value in the [`MethodsBuilder`] that will be suitable for use with
    /// [`StarlarkValue::get_methods`](crate::values::StarlarkValue::get_methods).
    pub fn set_attribute<'v, V: AllocFrozenValue>(
        &'v mut self,
        name: &str,
        value: V,
        docstring: Option<String>,
    ) {
        // We want to build an attribute, that ignores its self argument, and does no subsequent allocation.
        let value = self.heap.alloc(value);
        self.members.insert(
            name,
            UnboundValue::Attr(self.heap.alloc_simple_typed_static(NativeAttribute {
                speculative_exec_safe: true,
                docstring,
                typ: V::starlark_type_repr(),
                data: Some(value),
                // SAFETY: Set to `Some` immediately above
                callable: |value, _, _| Ok(unsafe { value.unwrap_unchecked() }.to_value()),
            })),
        );
    }

    /// Set an attribute. Only used by `starlark_module` macro
    #[doc(hidden)]
    pub fn set_attribute_fn(
        &mut self,
        name: &str,
        speculative_exec_safe: bool,
        docstring: Option<String>,
        typ: Ty,
        // The first argument is always `None`
        f: for<'v> fn(Option<FrozenValue>, Value<'v>, Heap<'v>) -> crate::Result<Value<'v>>,
    ) {
        self.members.insert(
            name,
            UnboundValue::Attr(self.heap.alloc_simple_typed_static(NativeAttribute {
                speculative_exec_safe,
                docstring,
                typ,
                data: None,
                callable: f,
            })),
        );
    }

    /// Set a method. Only used by `starlark_module` macro
    #[doc(hidden)]
    pub fn set_method(
        &mut self,
        name: &str,
        components: NativeCallableComponents,
        sig: ParametersSpec<FrozenValue>,
        f: NativeMethFn,
    ) {
        let ty = components.make_type(None);

        self.members.insert(
            name,
            UnboundValue::Method(self.heap.alloc_simple_typed_static(NativeMethod {
                function: NativeMeth(f, sig),
                name: name.to_owned(),
                speculative_exec_safe: components.speculative_exec_safe,
                docs: components.into_docs(None),
                ty,
            })),
        );
    }

    /// Allocate a value using the same underlying heap as the [`MethodsBuilder`]
    pub fn alloc<'v, V: AllocFrozenValue>(&'v self, value: V) -> FrozenValue {
        value.alloc_frozen_value(&self.heap)
    }
}

/// Lazy, named cache for a [`Methods`] value. Created via the
/// [`methods_static!`](crate::methods_static) macro; the methods are built on
/// first access via the supplied initializer.
///
/// ```ignore
/// fn my_methods(builder: &mut MethodsBuilder) { ... }
///
/// starlark::methods_static!(MY_METHODS = my_methods);
///
/// impl StarlarkValue<'_> for Foo {
///     fn get_methods() -> Option<&'static Methods> {
///         Some(MY_METHODS.methods())
///     }
/// }
/// ```
pub struct MethodsStatic {
    cell: OnceCell<Methods>,
    name: &'static str,
    init: fn(&mut MethodsBuilder),
}

impl MethodsStatic {
    /// Create a new [`MethodsStatic`]. Prefer the
    /// [`methods_static!`](crate::methods_static) macro, which fills in `name`
    /// from the call site.
    pub const fn new(name: &'static str, init: fn(&mut MethodsBuilder)) -> MethodsStatic {
        MethodsStatic {
            cell: OnceCell::new(),
            name,
            init,
        }
    }

    /// Get (or build, on first call) the [`Methods`] value.
    pub fn methods(&'static self) -> &'static Methods {
        self.cell.get_or_init(|| {
            let mut builder = MethodsBuilder::new();
            builder.heap_name = Some(MethodFrozenHeapName { name: self.name });
            (self.init)(&mut builder);
            builder.build()
        })
    }

    /// Copy all the methods into another builder. The methods' values stay
    /// owned by the static's heap; `out`'s heap takes a reference so the
    /// dependency is recorded for downstream consumers (e.g. pagable
    /// serialization).
    pub fn populate(&'static self, out: &mut MethodsBuilder) {
        let methods = self.methods();
        for (name, value) in methods.members.iter() {
            out.members.insert(name.as_str(), value.clone());
        }
        out.heap.add_reference(&methods.heap);
        out.docstring = methods.docstring.clone();
    }
}

/// Define a `static` of type [`MethodsStatic`] backed by an init function. The
/// heap is named `<module_path>::<NAME>`.
///
/// ```ignore
/// fn my_methods(builder: &mut MethodsBuilder) { ... }
///
/// starlark::methods_static!(MY_METHODS = my_methods);
/// ```
///
/// Or with an inline closure:
///
/// ```ignore
/// starlark::methods_static!(RES = |b| {
///     b.set_attribute("foo", 42, None);
/// });
/// ```
#[macro_export]
macro_rules! methods_static {
    ($vis:vis $name:ident = $init:expr) => {
        $vis static $name: $crate::__derive_refs::MethodsStatic =
            $crate::__derive_refs::MethodsStatic::new(
                concat!(module_path!(), "::", stringify!($name)),
                $init,
            );
    };
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use derive_more::Display;
    use starlark_derive::NoSerialize;
    use starlark_derive::ProvidesStaticType;
    use starlark_derive::StarlarkPagable;
    use starlark_derive::starlark_value;

    use crate as starlark;
    use crate::assert::Assert;
    use crate::environment::Methods;
    use crate::starlark_simple_value;
    use crate::values::StarlarkValue;

    #[test]
    fn test_set_attribute() {
        #[derive(
            Debug,
            Display,
            ProvidesStaticType,
            NoSerialize,
            Allocative,
            StarlarkPagable
        )]
        #[display("Magic")]
        struct Magic;
        starlark_simple_value!(Magic);

        starlark::methods_static!(
            RES = |x| {
                x.set_attribute("my_type", "magic", None);
                x.set_attribute("my_value", 42, None);
            }
        );

        #[starlark_value(type = "magic", skip_pagable)]
        impl<'v> StarlarkValue<'v> for Magic {
            fn get_methods() -> Option<&'static Methods> {
                Some(RES.methods())
            }
        }

        let mut a = Assert::new();
        a.globals_add(|x| x.set("magic", Magic));
        a.pass(
            r#"
assert_eq(magic.my_type, "magic")
assert_eq(magic.my_value, 42)"#,
        );
    }
}
