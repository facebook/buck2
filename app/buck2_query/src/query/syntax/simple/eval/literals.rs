/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the cli and query_* attr query language.

use buck2_query_parser::parse_expr;
use buck2_query_parser::placeholder::QUERY_PERCENT_S_PLACEHOLDER;
use starlark_map::small_set::SmallSet;

use crate::query::syntax::simple::eval::values::QueryResultExt;
use crate::query::syntax::simple::functions::QueryFunctions;
use crate::query::syntax::simple::functions::QueryFunctionsVisitLiterals;
use crate::query::syntax::simple::functions::QueryLiteralVisitor;

/// Look through the expression to find all the target literals.
/// Adds those that are found to `result` set.
pub fn extract_target_literals<F: QueryFunctions>(
    functions: &F,
    query: &str,
) -> buck2_error::Result<Vec<String>> {
    let parsed = parse_expr(query)?;
    struct LiteralExtractor {
        literals: SmallSet<String>,
    }
    impl QueryLiteralVisitor for LiteralExtractor {
        fn target_pattern(&mut self, pattern: &str) -> buck2_error::Result<()> {
            if pattern != QUERY_PERCENT_S_PLACEHOLDER {
                self.literals.get_or_insert_owned(pattern);
            }
            Ok(())
        }
    }
    let mut visitor = LiteralExtractor {
        literals: SmallSet::new(),
    };
    functions
        .visit_literals(&mut visitor, &parsed)
        .into_buck2_error(query)?;
    Ok(Vec::from_iter(visitor.literals))
}
