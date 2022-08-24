/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;

use buck2_core::provider::label::ProvidersLabel;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::query::QueryAttr;
use buck2_node::attrs::attr_type::query::QueryAttrBase;
use buck2_node::attrs::attr_type::query::QueryAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_query::query::syntax::simple::eval::error::QueryError;
use buck2_query::query::syntax::simple::functions::QueryFunctionsExt;
use buck2_query::query::syntax::simple::functions::QueryLiteralVisitor;
use buck2_query_parser::parse_expr;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::coerce::error::CoercionError;
use crate::attrs::coerce::AttrTypeCoerce;
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
