/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::OnceCell;
use std::iter;

use buck2_error::buck2_error;
use buck2_error::BuckErrorContext;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::typing::ParamIsRequired;
use starlark::typing::ParamSpec;
use starlark::typing::Ty;
use starlark::util::ArcStr;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::typing::StarlarkCallableChecked;
use starlark_map::small_map::SmallMap;

use crate::dynamic::attrs::DynamicAttrType;
use crate::dynamic::attrs_starlark::StarlarkDynamicAttrType;
use crate::dynamic::dynamic_actions::StarlarkDynamicActions;
use crate::dynamic::dynamic_actions_callable::DynamicActionsCallable;
use crate::dynamic::dynamic_actions_callable::DynamicActionsCallbackParam;
use crate::dynamic::dynamic_actions_callable::DynamicActionsCallbackParamSpec;
use crate::dynamic::dynamic_actions_callable::DynamicActionsCallbackReturnType;
use crate::dynamic::dynamic_actions_callable::FrozenStarlarkDynamicActionsCallable;
use crate::dynamic::dynamic_actions_callable::P_ACTIONS;

pub fn new_dynamic_actions_callable<'v>(
    r#impl: StarlarkCallableChecked<
        'v,
        DynamicActionsCallbackParamSpec,
        DynamicActionsCallbackReturnType,
    >,
    attrs: SmallMap<String, &'v StarlarkDynamicAttrType>,
    callback_param: &DynamicActionsCallbackParam,
) -> buck2_error::Result<DynamicActionsCallable<'v>> {
    if attrs.contains_key(callback_param.name) {
        return Err(buck2_error!(
            buck2_error::ErrorTag::Input,
            "Cannot define `actions` attribute"
        ));
    }
    let attrs: SmallMap<String, DynamicAttrType> = attrs
        .into_iter()
        .map(|(name, ty)| (name, ty.ty.clone()))
        .collect();

    let attr_args = attrs
        .iter()
        .map(|(name, ty)| (name.as_str(), ty.impl_param_ty()))
        .collect::<Vec<_>>();

    r#impl
        .0
        .check_callable_with(
            [],
            iter::once((callback_param.name, &*callback_param.ty))
                .chain(attr_args.iter().map(|(name, ty)| (*name, ty))),
            None,
            None,
            &DynamicActionsCallbackReturnType::starlark_type_repr(),
        )
        .buck_error_context("`impl` function must be callable with given params")?;

    let callable_ty = Ty::function(
        ParamSpec::new_named_only(attrs.iter().map(|(name, ty)| {
            (
                ArcStr::from(name.as_str()),
                ParamIsRequired::Yes,
                ty.callable_param_ty(),
            )
        }))
        .internal_error("Signature must be correct")?,
        StarlarkDynamicActions::starlark_type_repr(),
    );

    Ok(DynamicActionsCallable {
        self_ty: callable_ty,
        implementation: r#impl.to_unchecked(),
        name: OnceCell::new(),
        attrs,
    })
}

#[starlark_module]
pub(crate) fn register_dynamic_actions(globals: &mut GlobalsBuilder) {
    /// Create new dynamic action callable. Returned object will be callable,
    /// and the result of calling it can be passed to `ctx.actions.dynamic_output_new`.
    fn dynamic_actions<'v>(
        #[starlark(require = named)] r#impl: StarlarkCallableChecked<
            'v,
            DynamicActionsCallbackParamSpec,
            DynamicActionsCallbackReturnType,
        >,
        #[starlark(require = named)] attrs: SmallMap<String, &'v StarlarkDynamicAttrType>,
    ) -> starlark::Result<DynamicActionsCallable<'v>> {
        Ok(new_dynamic_actions_callable(r#impl, attrs, &P_ACTIONS)?)
    }

    const DynamicActions: StarlarkValueAsType<StarlarkDynamicActions> = StarlarkValueAsType::new();
    const DynamicActionsCallable: StarlarkValueAsType<FrozenStarlarkDynamicActionsCallable> =
        StarlarkValueAsType::new();
}
