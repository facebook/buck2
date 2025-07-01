/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_query::query::syntax::simple::functions::QueryFunctionsVisitLiterals;
use buck2_util::late_binding::LateBinding;

pub static CONFIGURED_GRAPH_QUERY_FUNCTIONS: LateBinding<Arc<dyn QueryFunctionsVisitLiterals>> =
    LateBinding::new("QUERY_FUNCTIONS");
