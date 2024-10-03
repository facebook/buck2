/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::OnceCell;
use std::cell::RefCell;
use std::sync::LazyLock;

use allocative::Allocative;
use anyhow::Context;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::unpack_artifact::UnpackArtifactOrDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::provider::ty::abstract_provider::AbstractProvider;
use buck2_error::BuckErrorContext;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::eval::Arguments;
use starlark::eval::Evaluator;
use starlark::eval::ParametersSpec;
use starlark::eval::ParametersSpecParam;
use starlark::typing::ParamIsRequired;
use starlark::typing::ParamSpec;
use starlark::typing::Ty;
use starlark::util::ArcStr;
use starlark::values::dict::DictType;
use starlark::values::list::ListType;
use starlark::values::list::UnpackList;
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::typing::FrozenStarlarkCallable;
use starlark::values::typing::StarlarkCallable;
use starlark::values::typing::StarlarkCallableParamSpec;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;

use crate::dynamic::dynamic_actions::StarlarkDynamicActions;
use crate::dynamic::dynamic_actions::StarlarkDynamicActionsData;
use crate::dynamic::dynamic_value::StarlarkDynamicValue;
use crate::dynamic::resolved_dynamic_value::StarlarkResolvedDynamicValue;

pub(crate) struct DynamicActionsCallbackParamSpec;

pub(crate) struct DynamicActionsCallbackParam {
    pub(crate) name: &'static str,
    pub(crate) ty: LazyLock<Ty>,
}

pub(crate) static P_ACTIONS: DynamicActionsCallbackParam = DynamicActionsCallbackParam {
    name: "actions",
    ty: LazyLock::new(AnalysisActions::starlark_type_repr),
};
pub(crate) static P_ARTIFACT_VALUES: DynamicActionsCallbackParam = DynamicActionsCallbackParam {
    name: "artifact_values",
    ty: LazyLock::new(DictType::<StarlarkArtifact, StarlarkArtifactValue>::starlark_type_repr),
};
pub(crate) static P_DYNAMIC_VALUES: DynamicActionsCallbackParam = DynamicActionsCallbackParam {
    name: "dynamic_values",
    ty: LazyLock::new(
        DictType::<StarlarkDynamicValue, StarlarkResolvedDynamicValue>::starlark_type_repr,
    ),
};
pub(crate) static P_OUTPUTS: DynamicActionsCallbackParam = DynamicActionsCallbackParam {
    name: "outputs",
    ty: LazyLock::new(DictType::<StarlarkArtifact, StarlarkDeclaredArtifact>::starlark_type_repr),
};

impl StarlarkCallableParamSpec for DynamicActionsCallbackParamSpec {
    fn params() -> ParamSpec {
        ParamSpec::new_parts(
            [],
            [],
            None,
            [
                (
                    ArcStr::new_static(P_ACTIONS.name),
                    ParamIsRequired::Yes,
                    P_ACTIONS.ty.dupe(),
                ),
                (
                    ArcStr::new_static(P_ARTIFACT_VALUES.name),
                    ParamIsRequired::Yes,
                    P_ARTIFACT_VALUES.ty.dupe(),
                ),
                (
                    ArcStr::new_static(P_DYNAMIC_VALUES.name),
                    ParamIsRequired::Yes,
                    P_DYNAMIC_VALUES.ty.dupe(),
                ),
                (
                    ArcStr::new_static(P_OUTPUTS.name),
                    ParamIsRequired::Yes,
                    P_OUTPUTS.ty.dupe(),
                ),
                (ArcStr::new_static("arg"), ParamIsRequired::Yes, Ty::any()),
            ],
            None,
        )
        .unwrap()
    }
}

pub(crate) type DynamicActionsCallbackReturnType = ListType<AbstractProvider>;

#[derive(Debug, thiserror::Error)]
enum DynamicActionCallableError {
    #[error("DynamicActionCallable can be called only if frozen")]
    NotFrozen,
    #[error("DynamicActionCallable must be exported (assigned to global variable)")]
    NotExported,
}

/// Result of `dynamic_actions` rule invocation.
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
pub(crate) struct DynamicActionsCallable<'v> {
    pub(crate) implementation:
        StarlarkCallable<'v, DynamicActionsCallbackParamSpec, DynamicActionsCallbackReturnType>,
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
pub(crate) struct FrozenStarlarkDynamicActionsCallable {
    pub(crate) implementation:
        FrozenStarlarkCallable<DynamicActionsCallbackParamSpec, DynamicActionsCallbackReturnType>,
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
        Err(starlark::Error::new_other(
            DynamicActionCallableError::NotFrozen,
        ))
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
        let (artifact_values, dynamic_values, outputs, arg) =
            self.signature.parser(args, eval, |parser, _eval| {
                // TODO(nga): we are not checking that what we parse here actually matches signature.
                let artifact_values: UnpackList<UnpackArtifactOrDeclaredArtifact> =
                    parser.next()?;
                let dynamic_values: UnpackList<&StarlarkDynamicValue> =
                    parser.next_opt()?.unwrap_or_default();
                let outputs: UnpackList<&StarlarkOutputArtifact> = parser.next()?;
                let arg: Value = parser.next()?;
                Ok((artifact_values, dynamic_values, outputs, arg))
            })?;
        let artifact_values = artifact_values
            .into_iter()
            .map(|a| a.artifact())
            .collect::<anyhow::Result<_>>()?;
        let dynamic_values = dynamic_values
            .items
            .into_iter()
            .map(|a| a.dynamic_value.dupe())
            .collect();
        let outputs = outputs
            .into_iter()
            .map(|a| a.artifact())
            .collect::<anyhow::Result<_>>()?;
        Ok(eval.heap().alloc(StarlarkDynamicActions {
            data: RefCell::new(Some(StarlarkDynamicActionsData {
                artifact_values,
                dynamic_values,
                outputs,
                arg,
                callable: me,
            })),
        }))
    }
}

impl<'v> AllocValue<'v> for DynamicActionsCallable<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl<'v> Freeze for DynamicActionsCallable<'v> {
    type Frozen = FrozenStarlarkDynamicActionsCallable;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let DynamicActionsCallable {
            implementation,
            name,
        } = self;

        let name = name
            .into_inner()
            .context(DynamicActionCallableError::NotExported)?;

        let signature = ParametersSpec::new_named_only(
            &name,
            [
                (P_ARTIFACT_VALUES.name, ParametersSpecParam::Required),
                (P_DYNAMIC_VALUES.name, ParametersSpecParam::Optional),
                (P_OUTPUTS.name, ParametersSpecParam::Required),
                ("arg", ParametersSpecParam::Required),
            ],
        );

        Ok(FrozenStarlarkDynamicActionsCallable {
            implementation: implementation.freeze(freezer)?,
            name,
            signature,
        })
    }
}
