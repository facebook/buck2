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
use starlark_map::small_set::SmallSet;

use crate::query::environment::QueryEnvironment;
use crate::query::syntax::simple::eval::values::QueryResultExt;
use crate::query::syntax::simple::functions::QueryFunctions;
use crate::query::syntax::simple::functions::QueryFunctionsExt;
use crate::query::syntax::simple::functions::QueryLiteralVisitor;

/// Look through the expression to find all the target literals.
/// Adds those that are found to `result` set.
pub fn extract_target_literals<Env: QueryEnvironment, F: QueryFunctions<Env>>(
    functions: &F,
    query: &str,
    result: &mut SmallSet<String>,
) -> anyhow::Result<()> {
    let parsed = parse_expr(query)?;
    struct LiteralExtractor<'a> {
        literals: &'a mut SmallSet<String>,
    }
    impl QueryLiteralVisitor for LiteralExtractor<'_> {
        fn target_pattern(&mut self, pattern: &str) -> anyhow::Result<()> {
            if pattern != "%s" {
                self.literals.get_or_insert_owned(pattern);
            }
            Ok(())
        }
    }
    let mut visitor = LiteralExtractor { literals: result };
    functions
        .visit_literals(&mut visitor, &parsed)
        .into_anyhow(query)?;
    Ok(())
}
