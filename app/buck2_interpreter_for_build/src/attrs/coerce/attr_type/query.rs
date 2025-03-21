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
use buck2_node::attrs::attr_type::query::QueryAttr;
use buck2_node::attrs::attr_type::query::QueryAttrBase;
use buck2_node::attrs::attr_type::query::QueryAttrType;
use buck2_node::attrs::attr_type::query::ResolvedQueryLiterals;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::provider_id_set::ProviderIdSet;
use buck2_query::query::syntax::simple::functions::QueryLiteralVisitor;
use buck2_query_parser::parse_expr;
use starlark::typing::Ty;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::ty_maybe_select::TyMaybeSelect;
use crate::attrs::coerce::error::CoercionError;
use crate::attrs::coerce::AttrTypeCoerce;

pub trait QueryAttrTypeExt {
    fn coerce(
        ctx: &dyn AttrCoercionContext,
        query: String,
    ) -> buck2_error::Result<QueryAttrBase<ProvidersLabel>>;
}

impl QueryAttrTypeExt for QueryAttrType {
    fn coerce(
        ctx: &dyn AttrCoercionContext,
        query: String,
    ) -> buck2_error::Result<QueryAttrBase<ProvidersLabel>> {
        // parse the expr to do validation and to extract the literals.
        let parsed_query = parse_expr(&query)?;

        struct Collector<'a> {
            ctx: &'a dyn AttrCoercionContext,
            literals: BTreeMap<String, ProvidersLabel>,
        }

        impl QueryLiteralVisitor for Collector<'_> {
            fn target_pattern(&mut self, pattern: &str) -> buck2_error::Result<()> {
                // TODO(cjhopman): We could probably parse the pattern first. This would likely at least give a better error message when the query contains a non-literal target pattern.
                // We could optimize this to do less work for duplicates, but it's generally not helpful.
                let label = self.ctx.coerce_providers_label(pattern)?;

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

        ctx.visit_query_function_literals(&mut collector, &parsed_query, &query)?;

        Ok(QueryAttrBase {
            query,
            resolved_literals: ResolvedQueryLiterals(collector.literals),
        })
    }
}

impl AttrTypeCoerce for QueryAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> buck2_error::Result<CoercedAttr> {
        let query = value
            .unpack_str()
            .ok_or_else(|| CoercionError::type_error(STRING_TYPE, value))?;

        Ok(CoercedAttr::Query(Box::new(QueryAttr {
            query: Self::coerce(ctx, query.to_owned())?,
            providers: ProviderIdSet::EMPTY,
        })))
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        TyMaybeSelect::Basic(Ty::string())
    }
}
