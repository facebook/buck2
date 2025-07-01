/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_query::query::syntax::simple::functions::description::QUERY_ENVIRONMENT_DESCRIPTION_BY_TYPE;
use buck2_query::query::syntax::simple::functions::description::QueryType;

use crate::aquery::environment::AqueryEnvironment;
use crate::cquery::environment::CqueryEnvironment;
use crate::uquery::environment::UqueryEnvironment;

pub(crate) fn init_query_environment_description_by_type() {
    QUERY_ENVIRONMENT_DESCRIPTION_BY_TYPE.init(|query_type| match query_type {
        QueryType::Uquery => UqueryEnvironment::describe(),
        QueryType::Cquery => CqueryEnvironment::describe(),
        QueryType::Aquery => AqueryEnvironment::describe(),
    })
}
