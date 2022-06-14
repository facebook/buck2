/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the cli and query_* attr query language.
use std::collections::HashSet;

use buck2_query_parser::parse_expr;

use crate::query::{
    environment::QueryEnvironment,
    syntax::simple::{
        eval::values::QueryResultExt,
        functions::{QueryFunctions, QueryFunctionsExt, QueryLiteralVisitor},
    },
};

pub fn extract_target_literals<Env: QueryEnvironment, F: QueryFunctions<Env>>(
    functions: &F,
    query: &str,
) -> anyhow::Result<HashSet<String>> {
    let parsed = parse_expr(query)?;
    struct LiteralExtractor {
        literals: HashSet<String>,
    }
    impl QueryLiteralVisitor for LiteralExtractor {
        fn target_pattern(&mut self, pattern: &str) -> anyhow::Result<()> {
            if pattern != "%s" {
                self.literals.get_or_insert_owned(pattern);
            }
            Ok(())
        }
    }
    let mut visitor = LiteralExtractor {
        literals: HashSet::new(),
    };
    functions
        .visit_literals(&mut visitor, &parsed)
        .into_anyhow(query)?;
    Ok(visitor.literals)
}
