/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_error::conversion::from_any_with_tag;
use cpe::x2p;
use cpe::x2p::CpeUserX2pInfo;
use fbinit::FacebookInit;
use interngraph::Authentication;
use interngraph::Client as InternGraphClient;
use interngraph::Subdomain;

pub(crate) const APP_ID: i64 = 927885601255280; // TODO(rajneeshl): Move this to a common place
const DEFAULT_RETRIES: u32 = 1;

/// Creates a GraphQL client configured for intern graph
pub fn get_graphql_client(fb: FacebookInit) -> buck2_error::Result<InternGraphClient> {
    // Auth token, subdomain, and client creation largely copied from turbocache https://fburl.com/code/oc6446g0
    let (subdomain, intern_auth) = get_interngraph_client_subdomain_and_auth(fb)?;
    let mut client = InternGraphClient::new(intern_auth);
    client.subdomain(subdomain).retries(DEFAULT_RETRIES);
    Ok(client)
}

/// Gets the subdomain and authentication configuration for intern graph
pub(crate) fn get_interngraph_client_subdomain_and_auth(
    fb: FacebookInit,
) -> buck2_error::Result<(Subdomain, Authentication)> {
    match x2p::current()
        .ok()
        .filter(|user| user.supports_vpnless())
        .and_then(CpeUserX2pInfo::http1_proxy_port_url)
    {
        Some(proxy_port_url) => Ok((
            Subdomain::InternmcX2pProxyProtocol(proxy_port_url),
            Authentication::x2p_cat_token_for_current_user(fb, APP_ID)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::HealthCheck))?,
        )),
        // Use default auth CAT and INTERN subdomain if x2p is not available for whatever reason
        // For example `support_vpnless()` has dependencies on several GKs: https://fburl.com/code/i3694xbx
        None => Ok((
            Subdomain::Intern,
            Authentication::cat_token_for_current_user(fb, APP_ID)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::HealthCheck))?,
        )),
    }
}

/// Helper function to execute a GraphQL request
#[expect(dead_code)]
pub async fn execute_graphql_request<T>(
    fb: FacebookInit,
    query: &str,
    variables: &str,
) -> buck2_error::Result<T>
where
    T: serde::de::DeserializeOwned,
{
    let mut graphql_client = get_graphql_client(fb)?;
    graphql_client
        .param("doc", query)
        .param("variables", variables)
        .post("/graphql")
        .await
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::HealthCheck))
}
