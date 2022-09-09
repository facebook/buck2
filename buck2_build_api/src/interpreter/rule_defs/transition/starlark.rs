/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::sync::Arc;

use buck2_core::bzl::ImportPath;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::target::TargetLabel;
use buck2_interpreter::extra::BuildContext;
use buck2_interpreter_for_build::transition::TransitionValue;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::dupe::Dupe;
use gazebo::prelude::*;
use hashbrown::HashSet;
use itertools::Itertools;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::dict::DictOf;
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

use crate::interpreter::rule_defs::attr::get_attr_coercion_context;

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
        "`transition` implementation must be def with parameters: {}, \
        but it is a def with signature `{0}`",
        _1.iter().map(|s| format!("`{}`", s)).join(", "))]
    MustBeDefWrongSig(String, &'static [&'static str]),
    #[error("Non-unique list of attrs")]
    NonUniqueAttrs,
}

/// Wrapper for `TargetLabel` which is `Trace`.
#[derive(Trace, Debug)]
struct TargetLabelTrace(#[trace(unsafe_ignore)] TargetLabel);

#[derive(Debug, Display, Trace, ProvidesStaticType, NoSerialize)]
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
    /// Transition function accesses theses attributes.
    attrs: Option<Vec<StringValue<'v>>>,
    /// Is this split transition? I. e. transition to multiple configurations.
    split: bool,
}

#[derive(Debug, Display, ProvidesStaticType, NoSerialize)]
#[display(fmt = "transition")]
pub struct FrozenTransition {
    id: Arc<TransitionId>,
    pub(crate) implementation: FrozenValue,
    pub(crate) refs: SmallMap<FrozenStringValue, TargetLabel>,
    pub(crate) attrs: Option<Vec<FrozenStringValue>>,
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

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn TransitionValue>(self);
    }
}

impl<'v> StarlarkValue<'v> for FrozenTransition {
    starlark_type!("transition");

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
        let split = self.split;
        Ok(FrozenTransition {
            id,
            implementation,
            refs,
            attrs,
            split,
        })
    }
}

starlark_complex_values!(Transition);

impl<'v> Transition<'v> {
    pub fn id_from_value(value: Value) -> anyhow::Result<Arc<TransitionId>> {
        match value.request_value::<&dyn TransitionValue>() {
            Some(has) => has.transition_id(),
            None => Err(TransitionError::WrongType(value.to_repr()).into()),
        }
    }
}

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

#[starlark_module]
fn register_transition_function(builder: &mut GlobalsBuilder) {
    fn transition<'v>(
        #[starlark(require = named)] r#impl: Value<'v>,
        #[starlark(require = named)] refs: DictOf<'v, StringValue<'v>, StringValue<'v>>,
        #[starlark(require = named)] attrs: Option<Vec<StringValue<'v>>>,
        #[starlark(require = named, default = false)] split: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Transition<'v>> {
        let context = get_attr_coercion_context(eval)?;
        let implementation = r#impl;

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
        let expected_params: &[&str] = if let Some(attrs) = &attrs {
            let attrs_set: HashSet<StringValue> = attrs.iter().copied().collect();
            if attrs_set.len() != attrs.len() {
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
            attrs,
            split,
        })
    }
}

pub fn register_transition_defs(globals: &mut GlobalsBuilder) {
    register_transition_function(globals);
}
