/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::BTreeMap;
use std::fmt::Display;

use allocative::Allocative;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use dupe::Dupe;
use pagable::Pagable;
use strong_hash::StrongHash;

use crate::attrs::attr_type::arg::QueryExpansion;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::provider_id_set::ProviderIdSet;

/// Attribute type created with `attrs.query(...)`.
#[derive(Debug, Pagable, Eq, PartialEq, Hash, Allocative)]
pub struct QueryAttrType {
    pub inner: DepAttrType,
}

impl QueryAttrType {
    pub fn new(inner: DepAttrType) -> Self {
        Self { inner }
    }
}

/// Attribute value of type `attrs.query(...)`.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable)]
pub struct QueryAttr<P: ProvidersLabelMaybeConfigured> {
    pub providers: ProviderIdSet,
    pub query: QueryAttrBase<P>,
}

impl QueryAttr<ConfiguredProvidersLabel> {
    pub(crate) fn traverse(
        &self,
        traversal: &mut dyn ConfiguredAttrTraversal,
    ) -> buck2_error::Result<()> {
        self.query.traverse(traversal)
    }
}

impl QueryAttr<ProvidersLabel> {
    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<QueryAttr<ConfiguredProvidersLabel>> {
        Ok(QueryAttr {
            query: self.query.configure(ctx)?,
            providers: self.providers.dupe(),
        })
    }

    pub(crate) fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        self.query.traverse(traversal)
    }
}

/// Query in target node attribute, like `$(query_outputs ...)`.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable, StrongHash)]
pub struct QueryMacroBase<P: ProvidersLabelMaybeConfigured> {
    pub expansion_type: QueryExpansion,
    pub query: QueryAttrBase<P>,
}

impl<P: ProvidersLabelMaybeConfigured> Display for QueryMacroBase<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", &self.expansion_type, self.query.query)?;
        Ok(())
    }
}

impl QueryMacroBase<ConfiguredProvidersLabel> {
    pub(crate) fn traverse(
        &self,
        traversal: &mut dyn ConfiguredAttrTraversal,
    ) -> buck2_error::Result<()> {
        self.query.traverse(traversal)
    }
}

impl QueryMacroBase<ProvidersLabel> {
    pub(crate) fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        self.query.traverse(traversal)
    }

    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<QueryMacroBase<ConfiguredProvidersLabel>> {
        Ok(QueryMacroBase {
            expansion_type: self.expansion_type.clone(),
            query: self.query.configure(ctx)?,
        })
    }
}

/// Query in target node.
///
/// Used in either:
/// * Attribute created with `attrs.query(...)`
/// * Query inside macros like `$(query_targets ...)`
#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable, StrongHash)]
pub struct QueryAttrBase<P: ProvidersLabelMaybeConfigured> {
    pub query: String,
    pub resolved_literals: ResolvedQueryLiterals<P>,
}

type OffsetAndLength = (usize, usize);

#[derive(
    Debug,
    Eq,
    PartialEq,
    Hash,
    Clone,
    Allocative,
    strong_hash::StrongHash,
    Pagable
)]
pub struct ResolvedQueryLiterals<P: ProvidersLabelMaybeConfigured>(
    pub BTreeMap<OffsetAndLength, P>,
);

impl QueryAttrBase<ConfiguredProvidersLabel> {
    pub(crate) fn traverse(
        &self,
        traversal: &mut dyn ConfiguredAttrTraversal,
    ) -> buck2_error::Result<()> {
        // queries have no inputs.
        for dep in self.resolved_literals.0.values() {
            traversal.dep(dep)?;
        }
        traversal.query(&self.query, &self.resolved_literals)?;
        Ok(())
    }
}

impl QueryAttrBase<ProvidersLabel> {
    fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<QueryAttrBase<ConfiguredProvidersLabel>> {
        Ok(QueryAttrBase {
            query: self.query.clone(),
            resolved_literals: ResolvedQueryLiterals(
                self.resolved_literals
                    .0
                    .iter()
                    .map(|(key, value)| Ok((*key, ctx.configure_target(value))))
                    .collect::<buck2_error::Result<_>>()?,
            ),
        })
    }

    fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        // queries don't have any configuration_deps or inputs currently.
        for dep in self.resolved_literals.0.values() {
            traversal.dep(dep)?;
        }
        Ok(())
    }
}
