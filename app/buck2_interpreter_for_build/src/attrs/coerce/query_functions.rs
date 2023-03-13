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
use buck2_util::late_binding::LateBinding;

pub static QUERY_FUNCTIONS: LateBinding<Arc<dyn QueryFunctionsVisitLiterals>> =
    LateBinding::new("QUERY_FUNCTIONS");
