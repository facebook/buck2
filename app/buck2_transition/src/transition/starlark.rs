/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::collections::HashSet;
use std::sync::Arc;

use allocative::Allocative;
use buck2_build_api::interpreter::rule_defs::provider::builtin::platform_info::PlatformInfo;
use buck2_core::bzl::ImportPath;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::target::label::label::TargetLabel;
use buck2_interpreter::build_context::starlark_path_from_build_context;
use buck2_interpreter::coerce::COERCE_TARGET_LABEL_FOR_BZL;
use buck2_interpreter::downstream_crate_starlark_defs::REGISTER_BUCK2_TRANSITION_GLOBALS;
use buck2_interpreter::types::transition::TransitionValue;
use derive_more::Display;
use dupe::Dupe;
use either::Either;
use gazebo::prelude::*;
use itertools::Itertools;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_complex_values;
use starlark::starlark_module;
use starlark::typing::ParamIsRequired;
use starlark::typing::ParamSpec;
use starlark::util::ArcStr;
use starlark::values::dict::DictType;
use starlark::values::dict::UnpackDictEntries;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::starlark_value;
use starlark::values::structs::StructRef;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::typing::StarlarkCallable;
use starlark::values::typing::StarlarkCallableParamSpec;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenStringValue;
use starlark::values::FrozenValue;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Value;

#[derive(Debug, buck2_error::Error)]
enum TransitionError {
    #[error("Transition must be assigned to a variable, e.g. `android_cpus = transition(...)`")]
    TransitionNotAssigned,
    #[error("`transition` can only be declared in .bzl files")]
    OnlyBzl,
    #[error(
        "`transition` implementation must be def with two parameters: `platform` and `refs`, \
        but it is not a def"
    )]
    MustBeDefNotDef,
    #[error(
        "`impl` parameter for `transition` call must be a function with the following parameters: {}, \
        but it is a function with a signature `{0}`",
        _1.iter().map(|s| format!("`{}`", s)).join(", "))]
    MustBeDefWrongSig(String, &'static [&'static str]),
    #[error("Non-unique list of attrs")]
    NonUniqueAttrs,
}

/// Wrapper for `TargetLabel` which is `Trace`.
#[derive(Trace, Debug, Allocative)]
struct TargetLabelTrace(TargetLabel);

#[derive(Debug, Display, Trace, ProvidesStaticType, NoSerialize, Allocative)]
#[display("transition")]
pub(crate) struct Transition<'v> {
    /// The name of this transition, filled in by `export_as()`. This must be set before this
    /// object can be used.
    id: RefCell<Option<Arc<TransitionId>>>,
    /// The path where this `Transition` is created and assigned.
    path: ImportPath,
    implementation: Value<'v>,
    /// Providers needed for the transition function. A map by target label.
    refs: SmallMap<StringValue<'v>, TargetLabelTrace>,
    /// Transition function accesses theses attributes.
    attrs: Option<Vec<StringValue<'v>>>,
    /// Is this split transition? I. e. transition to multiple configurations.
    split: bool,
}

#[derive(Debug, Display, ProvidesStaticType, NoSerialize, Allocative)]
#[display("transition")]
pub(crate) struct FrozenTransition {
    id: Arc<TransitionId>,
    pub(crate) implementation: FrozenValue,
    pub(crate) refs: SmallMap<FrozenStringValue, TargetLabel>,
    pub(crate) attrs_names_starlark: Option<Vec<FrozenStringValue>>,
    // the same as `attrs_names_starlark` but String representation
    pub(crate) attrs_names: Option<Arc<[String]>>,
    pub(crate) split: bool,
}

#[starlark_value(type = "transition")]
impl<'v> StarlarkValue<'v> for Transition<'v> {
    fn export_as(
        &self,
        variable_name: &str,
        _eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<()> {
        let mut id = self.id.borrow_mut();
        // First export wins
        if id.is_none() {
            *id = Some(Arc::new(TransitionId {
                path: self.path.clone(),
                name: variable_name.to_owned(),
            }));
        }
        Ok(())
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn TransitionValue>(self);
    }
}

#[starlark_value(type = "transition")]
impl<'v> StarlarkValue<'v> for FrozenTransition {
    type Canonical = Transition<'v>;

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn TransitionValue>(self);
    }
}

impl<'v> Freeze for Transition<'v> {
    type Frozen = FrozenTransition;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<FrozenTransition> {
        let implementation = freezer.freeze(self.implementation)?;
        let id = self
            .id
            .into_inner()
            .ok_or(TransitionError::TransitionNotAssigned)?;
        let refs = self
            .refs
            .into_iter()
            .map(|(k, v)| Ok((k.freeze(freezer)?, v.0)))
            .collect::<anyhow::Result<_>>()?;
        let attrs = self
            .attrs
            .map(|a| a.into_try_map(|a| a.freeze(freezer)))
            .transpose()?;
        let attrs_names = attrs.as_ref().map(|attrs| {
            Arc::from(
                attrs
                    .iter()
                    .map(|a| a.as_str().to_owned())
                    .collect::<Vec<_>>(),
            )
        });
        let split = self.split;
        Ok(FrozenTransition {
            id,
            implementation,
            refs,
            attrs_names_starlark: attrs,
            attrs_names,
            split,
        })
    }
}

starlark_complex_values!(Transition);

impl<'v> TransitionValue for Transition<'v> {
    fn transition_id(&self) -> anyhow::Result<Arc<TransitionId>> {
        self.id
            .borrow()
            .as_ref()
            .map(Dupe::dupe)
            .ok_or_else(|| TransitionError::TransitionNotAssigned.into())
    }
}

impl TransitionValue for FrozenTransition {
    fn transition_id(&self) -> anyhow::Result<Arc<TransitionId>> {
        Ok(self.id.dupe())
    }
}

struct TransitionImplParams;

impl StarlarkCallableParamSpec for TransitionImplParams {
    fn params() -> ParamSpec {
        ParamSpec::new_parts(
            [],
            [],
            None,
            [
                (
                    ArcStr::new_static("platform"),
                    ParamIsRequired::Yes,
                    PlatformInfo::starlark_type_repr(),
                ),
                (
                    ArcStr::new_static("refs"),
                    ParamIsRequired::Yes,
                    StructRef::starlark_type_repr(),
                ),
                (
                    ArcStr::new_static("attrs"),
                    ParamIsRequired::No,
                    StructRef::starlark_type_repr(),
                ),
            ],
            None,
        )
        .unwrap()
    }
}

#[starlark_module]
fn register_transition_function(builder: &mut GlobalsBuilder) {
    fn transition<'v>(
        // Note that precise function type is not checked by static or runtime typechecker,
        // and exists here only for documentation purposes.
        #[starlark(require = named)] r#impl: StarlarkCallable<
            'v,
            TransitionImplParams,
            Either<PlatformInfo, DictType<String, PlatformInfo>>,
        >,
        #[starlark(require = named)] refs: UnpackDictEntries<StringValue<'v>, StringValue<'v>>,
        #[starlark(require = named)] attrs: Option<UnpackListOrTuple<StringValue<'v>>>,
        #[starlark(require = named, default = false)] split: bool,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> anyhow::Result<Transition<'v>> {
        let implementation = r#impl.0;

        let refs = refs
            .entries
            .into_iter()
            .map(|(n, r)| {
                Ok((
                    n,
                    TargetLabelTrace((COERCE_TARGET_LABEL_FOR_BZL.get()?)(eval, &r)?),
                ))
            })
            .collect::<anyhow::Result<_>>()?;

        let path: ImportPath = (*starlark_path_from_build_context(eval)?
            .unpack_load_file()
            .ok_or(TransitionError::OnlyBzl)?)
        .clone();

        let parameters_spec = match implementation.parameters_spec() {
            Some(parameters_spec) => parameters_spec,
            None => return Err(TransitionError::MustBeDefNotDef.into()),
        };
        let expected_params: &[&str] = if let Some(attrs) = &attrs {
            let attrs_set: HashSet<StringValue> = attrs.items.iter().copied().collect();
            if attrs_set.len() != attrs.items.len() {
                return Err(TransitionError::NonUniqueAttrs.into());
            }
            &["platform", "refs", "attrs"]
        } else {
            &["platform", "refs"]
        };
        if !parameters_spec.can_fill_with_args(0, expected_params) {
            return Err(TransitionError::MustBeDefWrongSig(
                parameters_spec.parameters_str(),
                expected_params,
            )
            .into());
        }

        Ok(Transition {
            id: RefCell::new(None),
            path,
            implementation,
            refs,
            attrs: attrs.map(|a| a.items),
            split,
        })
    }
}

pub(crate) fn init_register_transition() {
    REGISTER_BUCK2_TRANSITION_GLOBALS.init(register_transition_function);
}
