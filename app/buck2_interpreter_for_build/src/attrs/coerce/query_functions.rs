/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_query::query::syntax::simple::functions::QueryFunctionsVisitLiterals;
use once_cell::sync::OnceCell;

/// Dependency injection for query functions.
///
/// Query functions implementation lives in downstream crate.
/// This field is initialized at program start, so this crate can visit query functions.
pub static QUERY_FUNCTIONS: OnceCell<Arc<dyn QueryFunctionsVisitLiterals>> = OnceCell::new();

pub(crate) fn query_functions() -> &'static dyn QueryFunctionsVisitLiterals {
    &**QUERY_FUNCTIONS.get().expect("QUERY_FUNCTIONS not set")
}
