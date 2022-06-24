/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::sync::Arc;

use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::result::SharedResult;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::query::QueryAttr;
use buck2_node::attrs::attr_type::query::QueryAttrBase;
use buck2_node::attrs::attr_type::query::QueryAttrType;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::traversal::CoercedAttrTraversal;
use buck2_query::query::syntax::simple::eval::error::QueryError;
use buck2_query::query::syntax::simple::functions::QueryFunctionsExt;
use buck2_query::query::syntax::simple::functions::QueryLiteralVisitor;
use buck2_query_parser::parse_expr;
use gazebo::prelude::*;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::analysis::AnalysisQueryResult;
use crate::attrs::analysis::AttrResolutionContext;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::attr_literal::ConfiguredAttrTraversal;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::attr_type::dep::DepAttrTypeExt;
use crate::attrs::configurable::AttrIsConfigurable;
use crate::attrs::AttrCoercionContext;
use crate::attrs::CoercedAttr;
use crate::attrs::ConfiguredAttr;
use crate::query::analysis::environment::ConfiguredGraphQueryEnvironment;

pub(crate) trait QueryAttrTypeExt {
    fn coerce(
        ctx: &dyn AttrCoercionContext,
        query: String,
    ) -> anyhow::Result<QueryAttrBase<CoercedAttr>>;
}

impl QueryAttrTypeExt for QueryAttrType {
    fn coerce(
        ctx: &dyn AttrCoercionContext,
        query: String,
    ) -> anyhow::Result<QueryAttrBase<CoercedAttr>> {
        // parse the expr to do validation and to extract the literals.
        let parsed_query = parse_expr(&query)?;

        struct Collector<'a> {
            ctx: &'a dyn AttrCoercionContext,
            literals: BTreeMap<String, ProvidersLabel>,
        }

        impl QueryLiteralVisitor for Collector<'_> {
            fn target_pattern(&mut self, pattern: &str) -> anyhow::Result<()> {
                // TODO(cjhopman): We could probably parse the pattern first. This would likely at least give a better error message when the query contains a non-literal target pattern.
                // We could optimize this to do less work for duplicates, but it's generally not helpful.
                if pattern == "$declared_deps" || pattern == "$declared" {
                    return Ok(());
                }
                let label = self.ctx.coerce_label(pattern)?;

                /*
                if label.name() != &ProvidersName::Default {
                    return Err(
                        MacroError::ProviderNameUnsupportedInQuery(pattern.to_owned()).into(),
                    );
                }*/

                self.literals.insert(pattern.to_owned(), label);
                Ok(())
            }
        }

        let mut collector = Collector {
            ctx,
            literals: BTreeMap::new(),
        };

        ConfiguredGraphQueryEnvironment::functions()
            .visit_literals(&mut collector, &parsed_query)
            .map_err(|e| QueryError::convert_error(e, &query))?;

        Ok(QueryAttrBase {
            query,
            resolved_literals: collector.literals,
        })
    }
}

impl AttrTypeCoerce for QueryAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let query = value
            .unpack_str()
            .ok_or_else(|| CoercionError::type_error(STRING_TYPE, value))?;

        Ok(AttrLiteral::Query(box QueryAttr {
            query: Self::coerce(ctx, query.to_owned())?,
            providers: None,
        }))
    }

    fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}

pub(crate) trait UnconfiguredQueryAttrBaseExt {
    fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<QueryAttrBase<ConfiguredAttr>>;

    fn traverse<'a>(&'a self, traversal: &mut dyn CoercedAttrTraversal<'a>) -> anyhow::Result<()>;
}

impl UnconfiguredQueryAttrBaseExt for QueryAttrBase<CoercedAttr> {
    fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<QueryAttrBase<ConfiguredAttr>> {
        Ok(QueryAttrBase {
            query: self.query.clone(),
            resolved_literals: self
                .resolved_literals
                .iter()
                .map(|(key, value)| Ok((key.clone(), ctx.configure_target(value))))
                .collect::<anyhow::Result<_>>()?,
        })
    }

    fn traverse<'a>(&'a self, traversal: &mut dyn CoercedAttrTraversal<'a>) -> anyhow::Result<()> {
        // queries don't have any configuration_deps or inputs currently.
        for dep in self.resolved_literals.values() {
            traversal.dep(dep.target())?;
        }
        Ok(())
    }
}

pub(crate) trait ConfiguredQueryAttrBaseExt {
    fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()>;

    fn resolve(&self, ctx: &dyn AttrResolutionContext) -> SharedResult<Arc<AnalysisQueryResult>>;
}

impl ConfiguredQueryAttrBaseExt for QueryAttrBase<ConfiguredAttr> {
    fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        // queries have no inputs.
        for dep in self.resolved_literals.values() {
            traversal.dep(dep)?;
        }
        traversal.query_macro(&self.query, &self.resolved_literals)?;
        Ok(())
    }

    fn resolve(&self, ctx: &dyn AttrResolutionContext) -> SharedResult<Arc<AnalysisQueryResult>> {
        ctx.resolve_query(&self.query)
    }
}

pub(crate) trait UnconfiguredQueryAttrExt {
    fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<QueryAttr<ConfiguredAttr>>;

    fn traverse<'a>(&'a self, traversal: &mut dyn CoercedAttrTraversal<'a>) -> anyhow::Result<()>;
}

impl UnconfiguredQueryAttrExt for QueryAttr<CoercedAttr> {
    fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<QueryAttr<ConfiguredAttr>> {
        Ok(QueryAttr {
            query: self.query.configure(ctx)?,
            providers: self.providers.dupe(),
        })
    }

    fn traverse<'a>(&'a self, traversal: &mut dyn CoercedAttrTraversal<'a>) -> anyhow::Result<()> {
        self.query.traverse(traversal)
    }
}

pub(crate) trait ConfiguredQueryAttrExt {
    fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()>;

    fn resolve<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>>;
}

impl ConfiguredQueryAttrExt for QueryAttr<ConfiguredAttr> {
    fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        self.query.traverse(traversal)
    }

    fn resolve<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>> {
        let query_results = self.query.resolve(ctx)?;
        let mut dependencies = Vec::new();

        for (target, providers) in &*query_results {
            let providers_label =
                ConfiguredProvidersLabel::new(target.dupe(), ProvidersName::Default);
            if let Some(provider_ids) = &self.providers {
                let provider_collection = providers.provider_collection();

                DepAttrType::check_providers(provider_ids, provider_collection, &providers_label)?;
            }
            dependencies.push(DepAttrType::alloc_dependency(
                ctx.starlark_module(),
                &providers_label,
                providers,
            ));
        }
        Ok(ctx.heap().alloc(dependencies))
    }
}
