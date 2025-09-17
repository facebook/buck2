/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![allow(dead_code)] // TODO(rajneeshl) : Remove this in further diffs

use fbinit::FacebookInit;
use graphql_client::GraphQLQuery;
use graphql_client::Response;

use crate::health_checks::graphql::common_graphql_client;

#[derive(GraphQLQuery, PartialEq, Eq)]
#[graphql(
    schema_path = "intern-schema.graphql",
    query_path = "src/health_checks/stable_revision/historical_stables_query.graphql",
    response_derives = "Debug, Clone, PartialEq"
)]
pub(crate) struct HistoricalStablesQuery;

pub async fn get_recent_revisions_for_bookmark(
    fb: FacebookInit,
    bookmark: &str,
    lookback_hours: u64,
) -> buck2_error::Result<Option<Vec<String>>> {
    let mut graphql_client = common_graphql_client::get_graphql_client(fb)?;
    let request_body = HistoricalStablesQuery::build_query(historical_stables_query::Variables {
        stable_name: bookmark.to_owned(),
        lookback_hours: lookback_hours.try_into().unwrap_or(0),
    });
    graphql_client
        .param("doc", request_body.query)
        .param(
            "variables",
            serde_json::to_string(&request_body.variables).unwrap_or("".to_owned()),
        )
        .post("/graphql")
        .await
        .map(|response| Ok(parse_response(response)))
        .map_err(|e| {
            buck2_error::conversion::from_any_with_tag(e, buck2_error::ErrorTag::HealthCheck)
        })?
}

fn parse_response(
    response: Response<historical_stables_query::ResponseData>,
) -> Option<Vec<String>> {
    response.data.map(|data| {
        data.xfb_all_stable_commits_lookback
            .into_iter()
            .map(|commit| commit.revision.unwrap_or_default())
            .collect()
    })
}
