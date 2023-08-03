/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_util::late_binding::LateBinding;

use crate::query::syntax::simple::functions::docs::QueryEnvironmentDescription;

pub enum QueryType {
    Uquery,
    Cquery,
    Aquery,
}

pub static QUERY_ENVIRONMENT_DESCRIPTION_BY_TYPE: LateBinding<
    fn(QueryType) -> QueryEnvironmentDescription,
> = LateBinding::new("QUERY_ENVIRONMENT_DESCRIPTION_BY_TYPE");
