/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{cell::RefCell, sync::Arc};

use buck2_core::target::TargetLabel;
use buck2_interpreter::{common::ImportPath, extra::BuildContext};
use derive_more::Display;
use gazebo::{any::AnyLifetime, dupe::Dupe};
use starlark::{
    collections::SmallMap,
    environment::GlobalsBuilder,
    eval::Evaluator,
    values::{
        dict::DictOf, Freeze, Freezer, FrozenStringValue, FrozenValue, NoSerialize, StarlarkValue,
        StringValue, Trace, Value, ValueLike,
    },
};

use crate::{
    attrs::AttrCoercionContext,
    interpreter::rule_defs::{attr::get_attr_coercion_context, transition::id::TransitionId},
};

#[derive(Debug, thiserror::Error)]
enum TransitionError {
    #[error("cfg parameter is not a transition object: {}", _0)]
    WrongType(String),
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
        "`transition` implementation must be def with two parameters: `platform` and `refs`, \
        but it is a def with signature `{0}`"
    )]
    MustBeDefWrongSig(String),
}

/// Wrapper for `TargetLabel` which is `Trace`.
#[derive(Trace, Debug)]
struct TargetLabelTrace(#[trace(unsafe_ignore)] TargetLabel);

#[derive(Debug, Display, Trace, AnyLifetime, NoSerialize)]
#[display(fmt = "transition")]
pub(crate) struct Transition<'v> {
    /// The name of this transition, filled in by `export_as()`. This must be set before this
    /// object can be used.
    #[trace(unsafe_ignore)]
    id: RefCell<Option<Arc<TransitionId>>>,
    /// The path where this `Transition` is created and assigned.
    #[trace(unsafe_ignore)]
    path: ImportPath,
    implementation: Value<'v>,
    /// Providers needed for the transition function. A map by target label.
    refs: SmallMap<StringValue<'v>, TargetLabelTrace>,
    /// Is this split transition? I. e. transition to multiple configurations.
    split: bool,
}

#[derive(Debug, Display, AnyLifetime, NoSerialize)]
#[display(fmt = "transition")]
pub struct FrozenTransition {
    id: Arc<TransitionId>,
    pub(crate) implementation: FrozenValue,
    pub(crate) refs: SmallMap<FrozenStringValue, TargetLabel>,
    pub(crate) split: bool,
}

impl<'v> StarlarkValue<'v> for Transition<'v> {
    starlark_type!("transition");

    fn export_as(&self, variable_name: &str, _eval: &mut Evaluator<'v, '_>) {
        let mut id = self.id.borrow_mut();
        // First export wins
        if id.is_none() {
            *id = Some(Arc::new(TransitionId {
                path: self.path.clone(),
                name: variable_name.to_owned(),
            }));
        }
    }
}

impl<'v> StarlarkValue<'v> for FrozenTransition {
    starlark_type!("transition");
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
        let split = self.split;
        Ok(FrozenTransition {
            id,
            implementation,
            refs,
            split,
        })
    }
}

starlark_complex_values!(Transition);

impl<'v> Transition<'v> {
    pub fn id_from_value(value: Value) -> anyhow::Result<Arc<TransitionId>> {
        if let Some(transition) = value.downcast_ref::<FrozenTransition>() {
            Ok(transition.id.dupe())
        } else if let Some(transition) = value.downcast_ref::<Transition>() {
            transition
                .id
                .borrow()
                .as_ref()
                .map(Dupe::dupe)
                .ok_or_else(|| TransitionError::TransitionNotAssigned.into())
        } else {
            Err(TransitionError::WrongType(value.to_repr()).into())
        }
    }
}

#[starlark_module]
fn register_transition_function(builder: &mut GlobalsBuilder) {
    fn transition<'v>(
        implementation: Value,
        refs: DictOf<'v, StringValue<'v>, StringValue<'v>>,
        split @ false: bool,
    ) -> anyhow::Result<Transition<'v>> {
        let context = get_attr_coercion_context(eval)?;

        let refs = refs
            .collect_entries()
            .into_iter()
            .map(|(n, r)| Ok((n, TargetLabelTrace(context.coerce_target(&r)?))))
            .collect::<anyhow::Result<_>>()?;

        let path = (*BuildContext::from_context(eval)?
            .starlark_path
            .unpack_load_file()
            .ok_or(TransitionError::OnlyBzl)?)
        .clone();

        let parameters_spec = match implementation.parameters_spec() {
            Some(parameters_spec) => parameters_spec,
            None => return Err(TransitionError::MustBeDefNotDef.into()),
        };
        if parameters_spec.parameters_str() != "platform, refs" {
            return Err(
                TransitionError::MustBeDefWrongSig(parameters_spec.parameters_str()).into(),
            );
        }

        Ok(Transition {
            id: RefCell::new(None),
            path,
            implementation,
            refs,
            split,
        })
    }
}

pub fn register_transition_defs(globals: &mut GlobalsBuilder) {
    register_transition_function(globals);
}
