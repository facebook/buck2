/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::manifold::Ttl;
use buck2_core::buck2_env;
use buck2_events::metadata::username;

// Copied from "Is User Command" from scuba buck2_builds
const ROBOTS: &[&str] = &[
    "twsvcscm",
    "svcscm",
    "facebook",
    "root",
    "svc-si_admin",
    "svc-fbsi_datamgr",
];

const USER_TTL_DAYS: u64 = 365;
const DEFAULT_TTL_DAYS: u64 = 60;

pub fn manifold_event_log_ttl() -> anyhow::Result<Ttl> {
    manifold_event_log_ttl_impl(ROBOTS, username().ok().flatten())
}

fn manifold_event_log_ttl_impl(robots: &[&str], username: Option<String>) -> anyhow::Result<Ttl> {
    // 1. return if this is a test
    let env = buck2_env!("BUCK2_TEST_MANIFOLD_TTL_S", type=u64, applicability=testing)?;
    if let Some(env) = env {
        return Ok::<Ttl, anyhow::Error>(Ttl::from_secs(env));
    }

    // 2. return if this is a user
    if let Some(username) = username {
        if !robots.contains(&(username.as_str())) {
            return Ok::<Ttl, anyhow::Error>(Ttl::from_days(USER_TTL_DAYS));
        }
    }

    Ok::<Ttl, anyhow::Error>(Ttl::from_days(DEFAULT_TTL_DAYS))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_a_user() -> anyhow::Result<()> {
        assert_eq!(
            manifold_event_log_ttl_impl(&["twsvcscm"], Some("random_person".to_owned()))?.as_secs(),
            365 * 24 * 60 * 60,
        );
        Ok(())
    }

    #[test]
    fn test_not_a_user() -> anyhow::Result<()> {
        assert_eq!(
            manifold_event_log_ttl_impl(&["twsvcscm"], Some("twsvcscm".to_owned()))?.as_secs(),
            60 * 24 * 60 * 60,
        );
        Ok(())
    }
}
