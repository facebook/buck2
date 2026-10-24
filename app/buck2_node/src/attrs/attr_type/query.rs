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
use std::hash::Hasher;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use pagable::Pagable;
use serde::Serializer;
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::AttrToken;
use crate::attrs::arena::CoercedAttrArenaBuilder;
use crate::attrs::arena::CoercedAttrRef;
use crate::attrs::arena::CoercedAttrRefKind;
use crate::attrs::arena::QueryExprRef;
use crate::attrs::arena::fields::ExprChild;
use crate::attrs::arena::kinds;
use crate::attrs::arena::schema::AttrKind;
use crate::attrs::arena::schema::SchemaKind;
use crate::attrs::arena::schema::encode_kind;
use crate::attrs::arena::semantics::Configure;
use crate::attrs::arena::semantics::Fmt;
use crate::attrs::arena::semantics::MatchesAny;
use crate::attrs::arena::semantics::Semantics;
use crate::attrs::arena::semantics::SerializeKind;
use crate::attrs::arena::semantics::ToJson;
use crate::attrs::arena::semantics::Traverse;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::arena::semantics::query_view;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::arg::QueryExpansion;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::strong_hash_without_config::StrongHashWithoutConfig;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::provider_id_set::ProviderIdSet;

/// Attribute type created with `attrs.query(...)`.
#[derive(Debug, Pagable, Eq, PartialEq, Hash, Allocative, StrongHash)]
pub struct QueryAttrType {
    pub inner: DepAttrType,
}

impl QueryAttrType {
    pub fn new(inner: DepAttrType) -> Self {
        Self { inner }
    }
}

/// Attribute value of type `attrs.query(...)`.
#[derive(Debug, Eq, PartialEq, Hash, StrongHash, Clone, Allocative, Pagable)]
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

impl StrongHashWithoutConfig for QueryAttr<ConfiguredProvidersLabel> {
    fn strong_hash_without_config<H: Hasher>(&self, state: &mut H) {
        let Self { providers, query } = self;
        providers.strong_hash(state);
        query.strong_hash_without_config(state);
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
        write!(f, "{} {}", self.expansion_type, self.query.query)?;
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

impl StrongHashWithoutConfig for QueryMacroBase<ConfiguredProvidersLabel> {
    fn strong_hash_without_config<H: Hasher>(&self, state: &mut H) {
        let Self {
            expansion_type,
            query,
        } = self;
        expansion_type.strong_hash(state);
        query.strong_hash_without_config(state);
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

impl StrongHashWithoutConfig for ResolvedQueryLiterals<ConfiguredProvidersLabel> {
    fn strong_hash_without_config<H: Hasher>(&self, state: &mut H) {
        let Self(resolved_literals) = self;
        (resolved_literals.len() as u64).strong_hash(state);
        resolved_literals
            .iter()
            .for_each(|(&(offset, length), label)| {
                (offset as u64).strong_hash(state);
                (length as u64).strong_hash(state);
                label.strong_hash_without_config(state);
            });
    }
}

impl StrongHashWithoutConfig for QueryAttrBase<ConfiguredProvidersLabel> {
    fn strong_hash_without_config<H: Hasher>(&self, state: &mut H) {
        let Self {
            query,
            resolved_literals,
        } = self;
        query.strong_hash(state);
        resolved_literals.strong_hash_without_config(state);
    }
}

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

impl SchemaKind for kinds::Query {
    const KIND: AttrKind = AttrKind::Query;
    type Fields = ExprChild;

    fn wrap<'a>(v: CoercedAttrRef<'a>, _: QueryExprRef<'a>) -> CoercedAttrRefKind<'a> {
        CoercedAttrRefKind::Query(query_view(v))
    }
}

impl CoercedAttrArenaBuilder {
    pub fn encode_parsed_query(&mut self, expr: AttrToken) -> buck2_error::Result<AttrToken> {
        encode_kind::<kinds::Query>(self, expr)
    }
}

impl Semantics for kinds::Query {
    fn traverse<'a>(
        expr: Self::Parts<'a>,
        Traverse { v, t, tr, .. }: Traverse<'_, 'a>,
    ) -> buck2_error::Result<()> {
        // Queries have no inputs or configuration deps: literal labels only,
        // in expression order.
        if !matches!(&inner_type(t).0.inner, AttrTypeInner::Query(_)) {
            return Err(mismatch(v, t));
        }
        let mut labels = Vec::new();
        expr.labels(&mut labels);
        for label in labels {
            tr.dep(label)?;
        }
        Ok(())
    }

    fn configure(
        _: Self::Parts<'_>,
        Configure { v, t, ctx, .. }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        match &inner_type(t).0.inner {
            AttrTypeInner::Query(_) => ResultMaybeCompatible::Compatible(ConfiguredAttr::Query(
                Box::new(query_view(v).configure(ctx)),
            )),
            _ => mismatch(v, t).into(),
        }
    }

    fn to_json(
        _: Self::Parts<'_>,
        ToJson { v, .. }: ToJson<'_, '_>,
    ) -> buck2_error::Result<serde_json::Value> {
        Ok(to_value(query_view(v).query().0)?)
    }

    fn fmt(_: Self::Parts<'_>, Fmt { v, f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        write!(f, "\"{}\"", query_view(v).query().0)
    }

    fn any_matches(
        _: Self::Parts<'_>,
        MatchesAny { v, filter }: MatchesAny<'_, '_>,
    ) -> buck2_error::Result<bool> {
        filter(&query_view(v).query().0)
    }

    fn serialize<S: Serializer>(
        _: Self::Parts<'_>,
        SerializeKind { v, ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        ser.serialize_str(&query_view(v).query().0)
    }
}
