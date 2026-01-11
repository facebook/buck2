/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::OnceCell;
use std::cell::RefCell;
use std::sync::LazyLock;

use allocative::Allocative;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::provider::ty::abstract_provider::AbstractProvider;
use buck2_error::BuckErrorContext;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Arguments;
use starlark::eval::Evaluator;
use starlark::eval::ParametersSpec;
use starlark::eval::ParametersSpecParam;
use starlark::starlark_module;
use starlark::typing::ParamIsRequired;
use starlark::typing::ParamSpec;
use starlark::typing::Ty;
use starlark::util::ArcStr;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::list::ListType;
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::typing::FrozenStarlarkCallable;
use starlark::values::typing::StarlarkCallable;
use starlark::values::typing::StarlarkCallableParamSpec;
use starlark_map::small_map::SmallMap;

use crate::dynamic::attrs::DynamicAttrType;
use crate::dynamic::attrs::DynamicAttrValues;
use crate::dynamic::dynamic_actions::StarlarkDynamicActions;
use crate::dynamic::dynamic_actions::StarlarkDynamicActionsData;

pub struct DynamicActionsCallbackParamSpec;

pub struct DynamicActionsCallbackParam {
    pub name: &'static str,
    pub ty: LazyLock<Ty>,
}

pub(crate) static P_ACTIONS: DynamicActionsCallbackParam = DynamicActionsCallbackParam {
    name: "actions",
    ty: LazyLock::new(AnalysisActions::starlark_type_repr),
};

impl StarlarkCallableParamSpec for DynamicActionsCallbackParamSpec {
    fn params() -> ParamSpec {
        ParamSpec::new_parts(
            [],
            [],
            None,
            [(
                ArcStr::new_static(P_ACTIONS.name),
                ParamIsRequired::Yes,
                P_ACTIONS.ty.dupe(),
            )],
            Some(Ty::any()),
        )
        .unwrap()
    }
}

pub type DynamicActionsCallbackReturnType = ListType<AbstractProvider>;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum DynamicActionCallableError {
    #[error("DynamicActionCallable can be called only if frozen")]
    NotFrozen,
    #[error("DynamicActionCallable must be exported (assigned to global variable)")]
    NotExported,
}

#[derive(
    Debug,
    NoSerialize,
    ProvidesStaticType,
    Allocative,
    derive_more::Display,
    Trace
)]
#[display(
    "DynamicActionCallable[{}]",
    self.name.get().map(|s| s.as_str()).unwrap_or("(unbound)")
)]
pub struct DynamicActionsCallable<'v> {
    pub(crate) self_ty: Ty,
    pub(crate) implementation:
        StarlarkCallable<'v, DynamicActionsCallbackParamSpec, DynamicActionsCallbackReturnType>,
    pub(crate) attrs: SmallMap<String, DynamicAttrType>,
    pub(crate) name: OnceCell<String>,
}

#[derive(
    Debug,
    NoSerialize,
    ProvidesStaticType,
    Allocative,
    derive_more::Display
)]
#[display("DynamicActionsCallable[{}]", name)]
pub struct FrozenStarlarkDynamicActionsCallable {
    pub(crate) self_ty: Ty,
    pub(crate) implementation:
        FrozenStarlarkCallable<DynamicActionsCallbackParamSpec, DynamicActionsCallbackReturnType>,
    pub(crate) attrs: SmallMap<String, DynamicAttrType>,
    name: String,
    signature: ParametersSpec<FrozenValue>,
}

#[starlark_value(type = "DynamicActionCallable")]
impl<'v> StarlarkValue<'v> for DynamicActionsCallable<'v> {
    type Canonical = FrozenStarlarkDynamicActionsCallable;

    fn export_as(
        &self,
        variable_name: &str,
        _eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<()> {
        // First wins.
        self.name.get_or_init(|| variable_name.to_owned());
        Ok(())
    }

    fn invoke(
        &self,
        _me: Value<'v>,
        _args: &Arguments<'v, '_>,
        _eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Value<'v>> {
        Err(buck2_error::Error::from(DynamicActionCallableError::NotFrozen).into())
    }

    fn typechecker_ty(&self) -> Option<Ty> {
        Some(self.self_ty.dupe())
    }

    // used for docs of `DynamicActionCallable`
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(dynamic_action_callable_methods)
    }
}

#[starlark_value(type = "DynamicActionCallable")]
impl<'v> StarlarkValue<'v> for FrozenStarlarkDynamicActionsCallable {
    type Canonical = Self;

    fn invoke(
        &self,
        me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Value<'v>> {
        let me = me.unpack_frozen().internal_error("me must be frozen")?;
        let me = FrozenValueTyped::new_err(me)?;
        let attr_values: DynamicAttrValues<Value<'v>> =
            self.signature.parser(args, eval, |parser, _eval| {
                let mut attr_values = Vec::with_capacity(self.attrs.len());
                for (name, attr_ty) in &self.attrs {
                    let value = attr_ty
                        .coerce(parser.next()?)
                        .with_buck_error_context(|| format!("Error coercing attribute `{name}`"))?;
                    attr_values.push(value);
                }
                Ok(DynamicAttrValues {
                    values: attr_values.into_boxed_slice(),
                })
            })?;
        Ok(eval.heap().alloc(StarlarkDynamicActions {
            data: RefCell::new(Some(StarlarkDynamicActionsData {
                callable: me,
                attr_values,
            })),
        }))
    }

    fn typechecker_ty(&self) -> Option<Ty> {
        Some(self.self_ty.dupe())
    }
}

impl<'v> AllocValue<'v> for DynamicActionsCallable<'v> {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl<'v> Freeze for DynamicActionsCallable<'v> {
    type Frozen = FrozenStarlarkDynamicActionsCallable;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        let DynamicActionsCallable {
            self_ty,
            implementation,
            name,
            attrs,
        } = self;

        let name = name
            .into_inner()
            .ok_or(DynamicActionCallableError::NotExported)
            .map_err(|e| FreezeError::new(e.to_string()))?;

        let signature = ParametersSpec::new_named_only(
            &name,
            attrs
                .keys()
                .map(|s| (s.as_str(), ParametersSpecParam::Required)),
        );

        Ok(FrozenStarlarkDynamicActionsCallable {
            self_ty,
            implementation: implementation.freeze(freezer)?,
            name,
            attrs,
            signature,
        })
    }
}

/// A factory function that creates `DynamicActions` instances.
///
/// `DynamicActionsCallable` is returned by calling `dynamic_actions()` with an implementation
/// function and attribute definitions. When invoked with concrete artifact values, it produces
/// a `DynamicActions` instance ready to be executed by `ctx.actions.dynamic_output_new()`.
///
/// This type must be assigned to a global variable before it can be called
///
/// # Usage
///
/// ```python
/// # Create a DynamicActionsCallable
/// _my_action = dynamic_actions(
///     impl = _my_impl,
///     attrs = {
///         "config": dynattrs.artifact_value(),
///         "out": dynattrs.output(),
///     },
/// )
///
/// # Later, call it with concrete values to create a DynamicActions
/// dynamic_action = _my_action(
///     config = config_file,
///     out = output.as_output(),
/// )
/// ```
///
/// See `dynamic_actions()` and `ctx.actions.dynamic_output_new()` for complete workflow.
#[starlark_module]
fn dynamic_action_callable_methods(builder: &mut MethodsBuilder) {}
