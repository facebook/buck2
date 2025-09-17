/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use fbinit::FacebookInit;
use graphql_client::GraphQLQuery;
use graphql_client::Response;

use crate::health_checks::graphql::common_graphql_client;

#[derive(GraphQLQuery, PartialEq, Eq)]
#[graphql(
    schema_path = "intern-schema.graphql",
    query_path = "src/health_checks/slowness/buildmate_slowness_detection_query.graphql",
    response_derives = "Debug, Clone, PartialEq"
)]
pub(crate) struct BuildmateSlownessDetectionQuery;

#[derive(Debug, Clone, PartialEq)]
pub struct BuildmateSlownessConfiguration {
    pub enable_slowness_detection: bool,
    pub slowness_threshold_minutes: i64,
    pub buildmate_link: String,
}

pub async fn get_buildmate_slowness_configuration(
    fb: FacebookInit,
    build_uuid: String,
    target_patterns: Vec<String>,
    is_incremental_build: bool,
) -> buck2_error::Result<Option<BuildmateSlownessConfiguration>> {
    let mut graphql_client = common_graphql_client::get_graphql_client(fb)?;

    let request_body = BuildmateSlownessDetectionQuery::build_query(
        buildmate_slowness_detection_query::Variables {
            build_uuid,
            target_patterns,
            is_incremental_build,
        },
    );

    graphql_client
        .param("doc", request_body.query)
        .param(
            "variables",
            serde_json::to_string(&request_body.variables).unwrap_or_default(),
        )
        .post("/graphql")
        .await
        .map(parse_response)
        .map_err(|e| {
            buck2_error::conversion::from_any_with_tag(e, buck2_error::ErrorTag::HealthCheck)
        })
}

fn parse_response(
    response: Response<buildmate_slowness_detection_query::ResponseData>,
) -> Option<BuildmateSlownessConfiguration> {
    response.data.and_then(|data| {
        data.xfb_buildmate_slowness_detection_configuration
            .map(|config| BuildmateSlownessConfiguration {
                enable_slowness_detection: config.enable_slowness_detection,
                slowness_threshold_minutes: config.slowness_threshold_minutes,
                buildmate_link: config.buildmate_link,
            })
    })
}
