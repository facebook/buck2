/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_hash::BuckFixedSortedMap;
use dupe::Dupe;
use pagable::Pagable;
use strong_hash::StrongHash;

use crate::attrs::arena::semantics::impl_semantics;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::json_label;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::strong_hash_without_config::StrongHashWithoutConfig;
use crate::provider_id_set::ProviderIdSet;

#[derive(Debug, Pagable, PartialEq, Eq, Hash, Allocative, StrongHash)]
pub struct SplitTransitionDepAttrType {
    pub required_providers: ProviderIdSet,
    pub transition: Arc<TransitionId>,
}

impl SplitTransitionDepAttrType {
    pub fn new(required_providers: ProviderIdSet, transition: Arc<TransitionId>) -> Self {
        SplitTransitionDepAttrType {
            required_providers,
            transition,
        }
    }

    pub(crate) fn configure(
        &self,
        dep: &ProvidersLabel,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<ConfiguredAttr> {
        let configured_providers = ctx.configure_split_transition_target(dep, &self.transition)?;
        Ok(ConfiguredAttr::SplitTransitionDep(Box::new(
            ConfiguredSplitTransitionDep {
                deps: configured_providers,
                required_providers: self.required_providers.dupe(),
            },
        )))
    }
}

#[derive(Hash, StrongHash, PartialEq, Eq, Debug, Clone, Allocative, Pagable)]
pub struct ConfiguredSplitTransitionDep {
    pub deps: BuckFixedSortedMap<String, ConfiguredProvidersLabel>,
    pub required_providers: ProviderIdSet,
}

impl StrongHashWithoutConfig for ConfiguredSplitTransitionDep {
    fn strong_hash_without_config<H: Hasher>(&self, state: &mut H) {
        let Self {
            deps,
            required_providers,
        } = self;
        (deps.len() as u64).strong_hash(state);
        deps.iter().for_each(|(key, label)| {
            key.strong_hash(state);
            label.strong_hash_without_config(state);
        });
        required_providers.strong_hash(state);
    }
}

impl Display for ConfiguredSplitTransitionDep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (i, dep) in self.deps.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}: {}", dep.0, dep.1)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl ConfiguredSplitTransitionDep {
    pub(crate) fn to_json(&self) -> buck2_error::Result<serde_json::Value> {
        let mut map = serde_json::Map::with_capacity(self.deps.len());
        for (label, target) in &self.deps {
            map.insert(label.clone(), serde_json::to_value(target.to_string())?);
        }
        Ok(serde_json::Value::Object(map))
    }

    pub(crate) fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        for (label, target) in &self.deps {
            if filter(label)? || filter(&target.to_string())? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl_semantics!(SplitTransitionDep {
    traverse(v, (l,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::SplitTransitionDep(ty) => tr.split_transition_dep(l, &ty.transition),
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (l,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::SplitTransitionDep(ty) => ResultMaybeCompatible::Compatible(ty.configure(l, ctx)?),
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (l,), ctx) { json_label(l) }
    fmt(v, (l,), ctx, f) { std::fmt::Display::fmt(l, f) }
    any_matches(v, (l,), filter) { filter(&l.to_string()) }
    json_key(v, (l,), ctx) { Ok(Cow::Owned(l.to_string())) }
    serialize<S>(v, (l,), ctx, ser) { ser.serialize_str(&l.to_string()) }
});
