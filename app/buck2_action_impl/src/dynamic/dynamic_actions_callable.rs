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

use allocative::Allocative;
use anyhow::Context;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::unpack_artifact::UnpackArtifactOrDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_error::BuckErrorContext;
use starlark::any::ProvidesStaticType;
use starlark::eval::Arguments;
use starlark::eval::Evaluator;
use starlark::eval::ParametersSpec;
use starlark::typing::Param;
use starlark::typing::ParamSpec;
use starlark::typing::Ty;
use starlark::values::list::UnpackList;
use starlark::values::none::NoneType;
use starlark::values::starlark_value;
use starlark::values::type_repr::DictType;
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

pub(crate) struct DynamicActionsCallbackParamSpec;

impl StarlarkCallableParamSpec for DynamicActionsCallbackParamSpec {
    fn params() -> ParamSpec {
        ParamSpec::new(vec![
            Param::name_only("actions", AnalysisActions::starlark_type_repr()),
            Param::name_only(
                "artifacts",
                DictType::<StarlarkArtifact, StarlarkArtifactValue>::starlark_type_repr(),
            ),
            Param::name_only(
                "outputs",
                DictType::<StarlarkArtifact, StarlarkDeclaredArtifact>::starlark_type_repr(),
            ),
            Param::name_only("arg", Ty::any()),
        ])
    }
}

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
    fmt = "DynamicActionCallable[{}]",
    "self.name.get().map(|s| s.as_str()).unwrap_or(\"(unbound)\")"
)]
pub(crate) struct DynamicActionsCallable<'v> {
    pub(crate) implementation: StarlarkCallable<'v, DynamicActionsCallbackParamSpec, NoneType>,
    pub(crate) name: OnceCell<String>,
}

#[derive(
    Debug,
    NoSerialize,
    ProvidesStaticType,
    Allocative,
    derive_more::Display
)]
#[display(fmt = "DynamicActionsCallable[{}]", "name")]
pub(crate) struct FrozenStarlarkDynamicActionsCallable {
    pub(crate) implementation: FrozenStarlarkCallable<DynamicActionsCallbackParamSpec, NoneType>,
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
        let (dynamic, outputs, arg) = self.signature.parser(args, eval, |mut parser, _eval| {
            // TODO(nga): we are not checking that what we parse here actually matches signature.
            let dynamic: UnpackList<UnpackArtifactOrDeclaredArtifact> = parser.next("dynamic")?;
            let outputs: UnpackList<&StarlarkOutputArtifact> = parser.next("outputs")?;
            let arg: Value = parser.next("arg")?;
            Ok((dynamic, outputs, arg))
        })?;
        let dynamic = dynamic
            .into_iter()
            .map(|a| a.artifact())
            .collect::<anyhow::Result<_>>()?;
        let outputs = outputs
            .into_iter()
            .map(|a| a.artifact())
            .collect::<anyhow::Result<_>>()?;
        Ok(eval.heap().alloc(StarlarkDynamicActions {
            data: RefCell::new(Some(StarlarkDynamicActionsData {
                dynamic,
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

        let mut signature = ParametersSpec::with_capacity(name.clone(), 3);
        signature.no_more_positional_args();
        signature.required("dynamic");
        signature.required("outputs");
        signature.required("arg");
        let signature = signature.finish();

        Ok(FrozenStarlarkDynamicActionsCallable {
            implementation: implementation.freeze(freezer)?,
            name,
            signature,
        })
    }
}
