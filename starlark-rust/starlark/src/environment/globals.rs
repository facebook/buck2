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

use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use itertools::Itertools;
use once_cell::sync::Lazy;
use once_cell::sync::OnceCell;

use crate::collections::symbol_map::Symbol;
use crate::collections::symbol_map::SymbolMap;
use crate::collections::Hashed;
use crate::collections::SmallMap;
use crate::docs::DocMember;
use crate::docs::DocModule;
use crate::docs::DocObject;
use crate::docs::DocString;
use crate::docs::DocStringKind;
use crate::stdlib;
pub use crate::stdlib::LibraryExtension;
use crate::values::function::NativeAttribute;
use crate::values::function::NativeCallableRawDocs;
use crate::values::function::NativeFunc;
use crate::values::function::NativeMeth;
use crate::values::layout::value_not_special::FrozenValueNotSpecial;
use crate::values::structs::AllocStruct;
use crate::values::types::function::NativeFunction;
use crate::values::types::function::NativeMethod;
use crate::values::AllocFrozenValue;
use crate::values::FrozenHeap;
use crate::values::FrozenHeapRef;
use crate::values::FrozenStringValue;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::Value;

/// The global values available during execution.
#[derive(Clone, Dupe, Debug, Allocative)]
pub struct Globals(Arc<GlobalsData>);

/// Methods of an object.
#[derive(Clone, Debug)]
pub struct Methods {
    /// This field holds the objects referenced in `members`.
    #[allow(dead_code)]
    heap: FrozenHeapRef,
    members: SymbolMap<FrozenValueNotSpecial>,
    docstring: Option<String>,
}

#[derive(Debug, Allocative)]
struct GlobalsData {
    heap: FrozenHeapRef,
    variables: SymbolMap<FrozenValue>,
    variable_names: Vec<FrozenStringValue>,
    docstring: Option<String>,
}

/// Used to build a [`Globals`] value.
#[derive(Debug)]
pub struct GlobalsBuilder {
    // The heap everything is allocated in
    heap: FrozenHeap,
    // Normal top-level variables, e.g. True/hash
    variables: SymbolMap<FrozenValue>,
    // The list of struct fields, pushed to the end
    struct_fields: Vec<SmallMap<FrozenStringValue, FrozenValue>>,
    // The raw docstring for this module
    docstring: Option<String>,
}

/// Used to build a [`Methods`] value.
#[derive(Debug)]
pub struct MethodsBuilder {
    /// The heap everything is allocated in.
    heap: FrozenHeap,
    /// Members, either `NativeMethod` or `NativeAttribute`.
    members: SymbolMap<FrozenValueNotSpecial>,
    /// The raw docstring for the main object.
    docstring: Option<String>,
}

impl Globals {
    /// Create an empty [`Globals`], with no functions in scope.
    pub fn new() -> Self {
        GlobalsBuilder::new().build()
    }

    /// Create a [`Globals`] following the
    /// [Starlark standard](https://github.com/bazelbuild/starlark/blob/master/spec.md#built-in-constants-and-functions).
    pub fn standard() -> Self {
        GlobalsBuilder::standard().build()
    }

    /// Create a [`Globals`] combining those functions in the Starlark standard plus
    /// all those defined in [`LibraryExtension`].
    pub fn extended() -> Self {
        GlobalsBuilder::extended().build()
    }

    /// Empty globals.
    pub(crate) fn empty() -> &'static Globals {
        static EMPTY: Lazy<Globals> = Lazy::new(|| GlobalsBuilder::new().build());
        &EMPTY
    }

    /// Create a [`Globals`] combining those functions in the Starlark standard plus
    /// all those given in the [`LibraryExtension`] arguments.
    pub fn extended_by(extensions: &[LibraryExtension]) -> Self {
        GlobalsBuilder::extended_by(extensions).build()
    }

    /// This function is only safe if you first call `heap` and keep a reference to it.
    /// Therefore, don't expose it on the public API.
    pub(crate) fn get<'v>(&'v self, name: &str) -> Option<Value<'v>> {
        self.get_frozen(name).map(FrozenValue::to_value)
    }

    /// This function is only safe if you first call `heap` and keep a reference to it.
    /// Therefore, don't expose it on the public API.
    pub(crate) fn get_frozen(&self, name: &str) -> Option<FrozenValue> {
        self.0.variables.get_str(name).copied()
    }

    /// Get all the names defined in this environment.
    pub fn names(&self) -> impl Iterator<Item = FrozenStringValue> + '_ {
        self.0.variable_names.iter().copied()
    }

    /// Iterate over all the items in this environment.
    /// Note returned values are owned by this globals.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (&str, FrozenValue)> {
        self.0.variables.iter().map(|(n, v)| (n.as_str(), *v))
    }

    pub(crate) fn heap(&self) -> &FrozenHeapRef {
        &self.0.heap
    }

    /// Print information about the values in this object.
    pub fn describe(&self) -> String {
        self.0
            .variables
            .iter()
            .map(|(name, val)| val.to_value().describe(name.as_str()))
            .join("\n")
    }

    /// Get the documentation for both the object itself, and its members.
    pub fn documentation(&self) -> DocModule {
        let DocObject { docs, members } = common_documentation(
            &self.0.docstring,
            self.0.variables.iter().map(|(n, v)| (n.as_str(), *v)),
        );
        DocModule { docs, members }
    }
}

impl Methods {
    pub(crate) fn get<'v>(&'v self, name: &str) -> Option<Value<'v>> {
        self.get_frozen(name).map(FrozenValueNotSpecial::to_value)
    }

    pub(crate) fn get_frozen(&self, name: &str) -> Option<FrozenValueNotSpecial> {
        self.members.get_str(name).copied()
    }

    pub(crate) fn get_hashed(&self, name: Hashed<&str>) -> Option<FrozenValueNotSpecial> {
        self.members.get_hashed_str(name).copied()
    }

    pub(crate) fn get_frozen_symbol(&self, name: &Symbol) -> Option<FrozenValueNotSpecial> {
        self.members.get(name).copied()
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
    pub fn documentation(&self) -> DocObject {
        common_documentation(
            &self.docstring,
            self.members
                .iter()
                .map(|(n, v)| (n.as_str(), v.to_frozen_value())),
        )
    }
}

impl GlobalsBuilder {
    /// Create an empty [`GlobalsBuilder`], with no functions in scope.
    pub fn new() -> Self {
        Self {
            heap: FrozenHeap::new(),
            variables: SymbolMap::new(),
            struct_fields: Vec::new(),
            docstring: None,
        }
    }

    /// Create a [`GlobalsBuilder`] following the
    /// [Starlark standard](https://github.com/bazelbuild/starlark/blob/master/spec.md#built-in-constants-and-functions).
    pub fn standard() -> Self {
        stdlib::standard_environment()
    }

    /// Create a [`GlobalsBuilder`] combining those functions in the Starlark standard plus
    /// all those defined in [`LibraryExtension`].
    pub fn extended() -> Self {
        Self::extended_by(LibraryExtension::all())
    }

    /// Create a [`GlobalsBuilder`] combining those functions in the Starlark standard plus
    /// all those defined in [`LibraryExtension`].
    pub fn extended_by(extensions: &[LibraryExtension]) -> Self {
        let mut res = Self::standard();
        for x in extensions {
            x.add(&mut res);
        }
        res
    }

    /// Add a nested struct to the builder. If `f` adds the definition `foo`,
    /// it will end up on a struct `name`, accessible as `name.foo`.
    pub fn struct_(&mut self, name: &str, f: impl FnOnce(&mut GlobalsBuilder)) {
        self.struct_fields.push(SmallMap::new());
        f(self);
        let fields = self.struct_fields.pop().unwrap();
        self.set(name, AllocStruct(fields));
    }

    /// A fluent API for modifying [`GlobalsBuilder`] and returning the result.
    pub fn with(mut self, f: impl FnOnce(&mut Self)) -> Self {
        f(&mut self);
        self
    }

    /// A fluent API for modifying [`GlobalsBuilder`] using [`struct_`](GlobalsBuilder::struct_).
    pub fn with_struct(mut self, name: &str, f: impl Fn(&mut GlobalsBuilder)) -> Self {
        self.struct_(name, f);
        self
    }

    /// Called at the end to build a [`Globals`].
    pub fn build(self) -> Globals {
        let variable_names = self
            .variables
            .keys()
            .map(|x| self.heap.alloc_str_intern(x.as_str()))
            .collect();
        Globals(Arc::new(GlobalsData {
            heap: self.heap.into_ref(),
            variables: self.variables,
            variable_names,
            docstring: self.docstring,
        }))
    }

    /// Set a value in the [`GlobalsBuilder`].
    pub fn set<'v, V: AllocFrozenValue>(&'v mut self, name: &str, value: V) {
        let value = value.alloc_frozen_value(&self.heap);
        match self.struct_fields.last_mut() {
            None => self.variables.insert(name, value),
            Some(fields) => {
                let name = self.heap.alloc_str(name);
                fields.insert(name, value)
            }
        };
    }

    /// Set a method. This function is usually called from code
    /// generated by `starlark_derive` and rarely needs to be called manually.
    pub fn set_function<F>(
        &mut self,
        name: &str,
        speculative_exec_safe: bool,
        raw_docs: NativeCallableRawDocs,
        typ: Option<FrozenStringValue>,
        f: F,
    ) where
        F: NativeFunc,
    {
        self.set(
            name,
            NativeFunction {
                function: Box::new(f),
                name: name.to_owned(),
                speculative_exec_safe,
                typ,
                raw_docs: Some(raw_docs),
            },
        )
    }

    /// Heap where globals are allocated. Can be used to allocate additional values.
    pub fn frozen_heap(&self) -> &FrozenHeap {
        &self.heap
    }

    /// Allocate a value using the same underlying heap as the [`GlobalsBuilder`],
    /// only intended for values that are referred to by those which are passed
    /// to [`set`](GlobalsBuilder::set).
    pub fn alloc<'v, V: AllocFrozenValue>(&'v self, value: V) -> FrozenValue {
        value.alloc_frozen_value(&self.heap)
    }

    /// Set per module docstring.
    ///
    /// This function is called by the `starlark_derive` generated code
    /// and rarely needs to be called manually.
    pub fn set_docstring(&mut self, docstring: &str) {
        self.docstring = Some(docstring.to_owned());
    }
}

impl Methods {
    /// Create an empty [`Globals`], with no functions in scope.
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
        }
    }

    /// Called at the end to build a [`Methods`].
    pub fn build(self) -> Methods {
        Methods {
            heap: self.heap.into_ref(),
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
        self.set_attribute_fn(
            name,
            true,
            docstring,
            value.to_value().describe(name),
            move |_, _| Ok(value.to_value()),
        );
    }

    /// Set an attribute. This function is usually called from code
    /// generated by `starlark_derive` and rarely needs to be called manually.
    pub fn set_attribute_fn<F>(
        &mut self,
        name: &str,
        speculative_exec_safe: bool,
        docstring: Option<String>,
        typ: String,
        f: F,
    ) where
        F: for<'v> Fn(Value<'v>, &'v Heap) -> anyhow::Result<Value<'v>> + Send + Sync + 'static,
    {
        self.members.insert(
            name,
            FrozenValueNotSpecial::new(self.heap.alloc(NativeAttribute {
                function: Box::new(f),
                speculative_exec_safe,
                docstring,
                typ,
            }))
            .unwrap(),
        );
    }

    /// Set a method. This function is usually called from code
    /// generated by `starlark_derive` and rarely needs to be called manually.
    pub fn set_method<F>(
        &mut self,
        name: &str,
        speculative_exec_safe: bool,
        raw_docs: NativeCallableRawDocs,
        typ: Option<FrozenValue>,
        f: F,
    ) where
        F: NativeMeth,
    {
        self.members.insert(
            name,
            FrozenValueNotSpecial::new(self.heap.alloc(NativeMethod {
                function: Box::new(f),
                name: name.to_owned(),
                typ,
                speculative_exec_safe,
                raw_docs,
            }))
            .unwrap(),
        );
    }

    /// Allocate a value using the same underlying heap as the [`MethodsBuilder`]
    pub fn alloc<'v, V: AllocFrozenValue>(&'v self, value: V) -> FrozenValue {
        value.alloc_frozen_value(&self.heap)
    }
}

/// Used to create methods for a [`StarlarkValue`](crate::values::StarlarkValue).
///
/// To define a method `foo()` on your type, define
///  usually written as:
///
/// ```ignore
/// fn my_methods(builder: &mut GlobalsBuilder) {
///     fn foo(me: ARef<Foo>) -> anyhow::Result<NoneType> {
///         ...
///     }
/// }
///
/// impl StarlarkValue<'_> for Foo {
///     ...
///     fn get_methods(&self) -> Option<&'static Globals> {
///         static RES: GlobalsStatic = GlobalsStatic::new();
///         RES.methods(module_creator)
///     }
///     ...
/// }
/// ```
pub struct GlobalsStatic(OnceCell<Globals>);

/// Similar to [`GlobalsStatic`], but for methods.
pub struct MethodsStatic(OnceCell<Methods>);

impl GlobalsStatic {
    /// Create a new [`GlobalsStatic`].
    pub const fn new() -> Self {
        Self(OnceCell::new())
    }

    fn globals(&'static self, x: impl FnOnce(&mut GlobalsBuilder)) -> &'static Globals {
        self.0.get_or_init(|| GlobalsBuilder::new().with(x).build())
    }

    /// Get a function out of the object. Requires that the function passed only set a single
    /// value. If populated via a `#[starlark_module]`, that means a single function in it.
    pub fn function(&'static self, x: impl FnOnce(&mut GlobalsBuilder)) -> FrozenValue {
        let globals = self.globals(x);
        assert!(
            globals.0.variables.len() == 1,
            "GlobalsBuilder.function must have exactly 1 member, you had {}",
            globals
                .names()
                .map(|s| format!("`{}`", s.as_str()))
                .join(", ")
        );

        *globals.0.variables.values().next().unwrap()
    }

    /// Move all the globals in this [`GlobalsBuilder`] into a new one. All variables will
    /// only be allocated once (ensuring things like function comparison works properly).
    pub fn populate(&'static self, x: impl FnOnce(&mut GlobalsBuilder), out: &mut GlobalsBuilder) {
        let globals = self.globals(x);
        for (name, value) in globals.0.variables.iter() {
            out.set(name.as_str(), *value)
        }
        out.docstring = globals.0.docstring.clone();
    }
}

impl MethodsStatic {
    /// Create a new [`MethodsStatic`].
    pub const fn new() -> Self {
        Self(OnceCell::new())
    }

    /// Populate the globals with a builder function. Always returns `Some`, but using this API
    /// to be a better fit for [`StarlarkValue.get_methods`](crate::values::StarlarkValue::get_methods).
    pub fn methods(&'static self, x: impl FnOnce(&mut MethodsBuilder)) -> Option<&'static Methods> {
        Some(self.0.get_or_init(|| MethodsBuilder::new().with(x).build()))
    }

    /// Move all the globals in this [`GlobalsBuilder`] into a new one. All variables will
    /// only be allocated once (ensuring things like function comparison works properly).
    pub fn populate(&'static self, x: impl FnOnce(&mut MethodsBuilder), out: &mut MethodsBuilder) {
        let methods = self.methods(x).unwrap();
        for (name, value) in methods.members.iter() {
            out.members.insert(name.as_str(), *value);
        }
        out.docstring = methods.docstring.clone();
    }
}

fn common_documentation<'a>(
    docstring: &Option<String>,
    members: impl IntoIterator<Item = (&'a str, FrozenValue)>,
) -> DocObject {
    let main_docs = docstring
        .as_ref()
        .and_then(|ds| DocString::from_docstring(DocStringKind::Rust, ds));
    let member_docs = members
        .into_iter()
        .map(|(name, val)| (name.to_owned(), DocMember::from_value(val.to_value())))
        .sorted_by(|(l, _), (r, _)| Ord::cmp(l, r))
        .collect();

    DocObject {
        docs: main_docs,
        members: member_docs,
    }
}

#[cfg(test)]
mod tests {
    use derive_more::Display;

    use super::*;
    use crate as starlark;
    use crate::any::ProvidesStaticType;
    use crate::assert::Assert;
    use crate::starlark_simple_value;
    use crate::starlark_type;
    use crate::values::NoSerialize;
    use crate::values::StarlarkValue;

    #[test]
    fn test_send_sync()
    where
        Globals: Send + Sync,
    {
    }

    #[test]
    fn test_set_attribute() {
        #[derive(Debug, Display, ProvidesStaticType, NoSerialize, Allocative)]
        #[display(fmt = "Magic")]
        struct Magic;
        starlark_simple_value!(Magic);
        impl<'v> StarlarkValue<'v> for Magic {
            starlark_type!("magic");
            fn get_methods() -> Option<&'static Methods> {
                static RES: MethodsStatic = MethodsStatic::new();
                RES.methods(|x| {
                    x.set_attribute("my_type", "magic", None);
                    x.set_attribute("my_value", 42, None);
                })
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
