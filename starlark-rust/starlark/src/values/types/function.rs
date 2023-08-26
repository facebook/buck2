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

//! Function types, including native functions and `object.member` functions.

use std::collections::HashMap;

use allocative::Allocative;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use starlark_derive::starlark_value;
use starlark_derive::NoSerialize;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::coerce::Coerce;
use crate::docs::DocFunction;
use crate::docs::DocItem;
use crate::docs::DocProperty;
use crate::docs::DocString;
use crate::docs::DocStringKind;
use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::eval::ParametersParser;
use crate::eval::ParametersSpec;
use crate::private::Private;
use crate::starlark_complex_value;
use crate::starlark_simple_value;
use crate::typing::Ty;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::Freeze;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::FrozenValueTyped;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Value;
use crate::values::ValueError;
use crate::values::ValueLike;

/// Return value of `type(any function)`.
pub const FUNCTION_TYPE: &str = "function";

/// Marker trait for function types.
pub(crate) enum StarlarkFunction {}

impl StarlarkTypeRepr for StarlarkFunction {
    fn starlark_type_repr() -> Ty {
        Ty::any_function()
    }
}

#[derive(Debug, Allocative, Clone, Copy, Dupe)]
#[doc(hidden)]
pub enum SpecialBuiltinFunction {
    List,
    Dict,
}

/// A native function that can be evaluated.
///
/// This trait is implemented by generated code and rarely needed to be implemented manually.
pub trait NativeFunc: Send + Sync + 'static {
    /// Invoke the function.
    fn invoke<'v>(
        &self,
        eval: &mut Evaluator<'v, '_>,
        args: &Arguments<'v, '_>,
    ) -> anyhow::Result<Value<'v>>;
}

impl<T> NativeFunc for T
where
    T: for<'v> Fn(&mut Evaluator<'v, '_>, &Arguments<'v, '_>) -> anyhow::Result<Value<'v>>
        + Send
        + Sync
        + 'static,
{
    fn invoke<'v>(
        &self,
        eval: &mut Evaluator<'v, '_>,
        args: &Arguments<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        (*self)(eval, args)
    }
}

/// Native method implementation.
///
/// This trait is implemented by generated code and rarely needed to be implemented manually.
pub trait NativeMeth: Send + Sync + 'static {
    /// Invoke the method.
    fn invoke<'v>(
        &self,
        eval: &mut Evaluator<'v, '_>,
        this: Value<'v>,
        args: &Arguments<'v, '_>,
    ) -> anyhow::Result<Value<'v>>;
}

impl<T> NativeMeth for T
where
    T: for<'v> Fn(
            &mut Evaluator<'v, '_>,
            Value<'v>,
            &Arguments<'v, '_>,
        ) -> anyhow::Result<Value<'v>>
        + Send
        + Sync
        + 'static,
{
    fn invoke<'v>(
        &self,
        eval: &mut Evaluator<'v, '_>,
        this: Value<'v>,
        args: &Arguments<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        (*self)(eval, this, args)
    }
}

/// A native function that can be evaluated.
pub trait NativeAttr:
    for<'v> Fn(Value<'v>, &'v Heap) -> anyhow::Result<Value<'v>> + Send + Sync + 'static
{
}

impl<T> NativeAttr for T where
    T: for<'v> Fn(Value<'v>, &'v Heap) -> anyhow::Result<Value<'v>> + Send + Sync + 'static
{
}

/// Enough details to get the documentation for a callable ([`NativeFunction`] or [`NativeMethod`])
#[doc(hidden)]
#[derive(Allocative)]
pub struct NativeCallableRawDocs {
    pub rust_docstring: Option<&'static str>,
    pub signature: ParametersSpec<FrozenValue>,
    pub parameter_types: Vec<Ty>,
    pub return_type: Ty,
    pub as_type: Option<Ty>,
}

#[doc(hidden)]
impl NativeCallableRawDocs {
    pub fn documentation(&self) -> DocFunction {
        DocFunction::from_docstring(
            DocStringKind::Rust,
            self.signature
                .documentation(self.parameter_types.clone(), HashMap::new()),
            self.return_type.clone(),
            self.rust_docstring,
            self.as_type.clone(),
        )
    }
}

/// Starlark representation of native (Rust) functions.
///
/// Almost always created with [`#[starlark_module]`](macro@crate::starlark_module).
#[derive(Derivative, ProvidesStaticType, Display, NoSerialize, Allocative)]
#[derivative(Debug)]
#[display(fmt = "{}", name)]
pub struct NativeFunction {
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    pub(crate) function: Box<dyn NativeFunc>,
    pub(crate) name: String,
    /// `.type` attribute and a type when this function is used in type expression.
    pub(crate) type_attr: Option<Ty>,
    pub(crate) ty: Option<Ty>,
    /// Safe to evaluate speculatively.
    pub(crate) speculative_exec_safe: bool,
    #[derivative(Debug = "ignore")]
    pub(crate) raw_docs: Option<NativeCallableRawDocs>,
    pub(crate) special_builtin_function: Option<SpecialBuiltinFunction>,
}

impl AllocFrozenValue for NativeFunction {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        heap.alloc_simple(self)
    }
}

impl NativeFunction {
    /// Create a new [`NativeFunction`] from the Rust function which works directly on the parameters.
    /// The called function is responsible for validating the parameters are correct.
    pub fn new_direct<F>(function: F, name: String) -> Self
    where
        // If I switch this to the trait alias then it fails to resolve the usages
        F: for<'v> Fn(&mut Evaluator<'v, '_>, &Arguments<'v, '_>) -> anyhow::Result<Value<'v>>
            + Send
            + Sync
            + 'static,
    {
        NativeFunction {
            function: Box::new(function),
            name,
            type_attr: None,
            ty: None,
            speculative_exec_safe: false,
            raw_docs: None,
            special_builtin_function: None,
        }
    }

    /// Create a new [`NativeFunction`] from the Rust function, plus the parameter specification.
    pub fn new<F>(function: F, name: String, parameters: ParametersSpec<FrozenValue>) -> Self
    where
        F: for<'v> Fn(
                &mut Evaluator<'v, '_>,
                ParametersParser<'v, '_>,
            ) -> anyhow::Result<Value<'v>>
            + Send
            + Sync
            + 'static,
    {
        Self::new_direct(
            move |eval, params| {
                parameters.parser(params, eval, |parser, eval| function(eval, parser))
            },
            name,
        )
    }
}

impl<'v> AllocValue<'v> for NativeFunction {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_simple(self)
    }
}

/// Define the function type
#[starlark_value(type = FUNCTION_TYPE)]
impl<'v> StarlarkValue<'v> for NativeFunction {
    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        self.function.invoke(eval, args)
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        if let Some(s) = self.type_attr.as_ref().map(|t| t.as_name()) {
            if attribute == "type" {
                return Some(heap.alloc(s));
            }
        }
        None
    }

    #[allow(clippy::manual_map)]
    fn eval_type(&self) -> Option<Ty> {
        self.type_attr.clone()
    }

    fn has_attr(&self, _attribute: &str, _heap: &'v Heap) -> bool {
        // TODO(nga): implement properly.
        false
    }

    fn dir_attr(&self) -> Vec<String> {
        if self.type_attr.is_some() {
            vec!["type".to_owned()]
        } else {
            Vec::new()
        }
    }

    fn documentation(&self) -> Option<DocItem> {
        self.raw_docs
            .as_ref()
            .map(|raw_docs| DocItem::Function(raw_docs.documentation()))
    }

    fn typechecker_ty(&self) -> Option<Ty> {
        self.ty.clone()
    }

    fn at(&self, index: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match &self.special_builtin_function {
            Some(SpecialBuiltinFunction::List) => {
                let index = TypeCompiled::new(index, heap)?;
                Ok(TypeCompiled::type_list_of(index, heap).to_inner())
            }
            _ => ValueError::unsupported(self, "[]"),
        }
    }

    fn at2(
        &self,
        index0: Value<'v>,
        index1: Value<'v>,
        heap: &'v Heap,
        _private: Private,
    ) -> anyhow::Result<Value<'v>> {
        match &self.special_builtin_function {
            Some(SpecialBuiltinFunction::Dict) => {
                let index0 = TypeCompiled::new(index0, heap)?;
                let index1 = TypeCompiled::new(index1, heap)?;
                Ok(TypeCompiled::type_dict_of(index0, index1, heap).to_inner())
            }
            _ => ValueError::unsupported(self, "[,]"),
        }
    }
}

#[derive(Derivative, Display, NoSerialize, ProvidesStaticType, Allocative)]
#[derivative(Debug)]
#[display(fmt = "{}", name)]
pub(crate) struct NativeMethod {
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    pub(crate) function: Box<dyn NativeMeth>,
    pub(crate) name: String,
    /// Safe to evaluate speculatively.
    pub(crate) speculative_exec_safe: bool,
    #[derivative(Debug = "ignore")]
    pub(crate) raw_docs: NativeCallableRawDocs,
}

starlark_simple_value!(NativeMethod);

#[starlark_value(type = "native_method")]
impl<'v> StarlarkValue<'v> for NativeMethod {
    fn invoke_method(
        &self,
        this: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
        _: Private,
    ) -> anyhow::Result<Value<'v>> {
        self.function.invoke(eval, this, args)
    }

    fn documentation(&self) -> Option<DocItem> {
        Some(DocItem::Function(self.raw_docs.documentation()))
    }
}

/// Used by the `#[starlark(attribute)]` tag of [`#[starlark_module]`](macro@starlark_module)
/// to define a function that pretends to be an attribute.
#[derive(Derivative, Display, NoSerialize, ProvidesStaticType, Allocative)]
#[display(fmt = "Attribute")]
#[derivative(Debug)]
pub(crate) struct NativeAttribute {
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    pub(crate) function: Box<dyn NativeAttr>,
    /// Safe to evaluate speculatively.
    pub(crate) speculative_exec_safe: bool,
    pub(crate) docstring: Option<String>,
    pub(crate) typ: Ty,
}

starlark_simple_value!(NativeAttribute);

impl NativeAttribute {
    pub(crate) fn call<'v>(&self, value: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        (self.function)(value, heap)
    }
}

#[starlark_value(type = "attribute")]
impl<'v> StarlarkValue<'v> for NativeAttribute {
    fn invoke_method(
        &self,
        this: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
        _: Private,
    ) -> anyhow::Result<Value<'v>> {
        let method = self.call(this, eval.heap())?;
        method.invoke(args, eval)
    }

    fn documentation(&self) -> Option<DocItem> {
        let ds = self
            .docstring
            .as_ref()
            .and_then(|ds| DocString::from_docstring(DocStringKind::Rust, ds));
        let typ = self.typ.clone();
        Some(DocItem::Property(DocProperty { docs: ds, typ }))
    }
}

/// A wrapper for a method with a self object already bound.
#[derive(
    Clone,
    Debug,
    Trace,
    Coerce,
    Display,
    Freeze,
    NoSerialize,
    ProvidesStaticType,
    Allocative
)]
#[repr(C)]
#[display(fmt = "{}", method)]
pub(crate) struct BoundMethodGen<V> {
    pub(crate) method: FrozenValueTyped<'static, NativeMethod>,
    pub(crate) this: V,
}

starlark_complex_value!(pub(crate) BoundMethod);

impl<'v, V: ValueLike<'v>> BoundMethodGen<V> {
    /// Create a new [`BoundMethod`]. Given the expression `object.function`,
    /// the first argument would be `object`, and the second would be `getattr(object, "function")`.
    pub(crate) fn new(this: V, method: FrozenValueTyped<'static, NativeMethod>) -> Self {
        BoundMethodGen { method, this }
    }
}

#[starlark_value(type = FUNCTION_TYPE)]
impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for BoundMethodGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        self.method
            .invoke_method(self.this.to_value(), args, eval, Private)
    }

    fn documentation(&self) -> Option<DocItem> {
        self.method.documentation()
    }
}
