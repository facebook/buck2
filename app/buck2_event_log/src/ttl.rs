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
// diff signal retention is 4 weeks
const CI_EXCEPT_CONTINUOUS_TTL_DAYS: u64 = 28;

struct ScheduleType {
    schedule_type: Option<&'static str>,
}

impl ScheduleType {
    const SCHEDULE_TYPE_CONTINUOUS: &'static str = "continuous";

    pub fn new() -> anyhow::Result<Self> {
        // Same as RE does https://fburl.com/code/sj13r130
        let schedule_type =
            if let Some(env) = buck2_env!("SCHEDULE_TYPE", applicability = internal)? {
                Some(env)
            } else {
                buck2_env!("SANDCASTLE_SCHEDULE_TYPE", applicability = internal)?
            };
        Ok(Self { schedule_type })
    }

    fn is_continuous(&self) -> bool {
        self.schedule_type == Some(Self::SCHEDULE_TYPE_CONTINUOUS)
    }

    fn is_some(&self) -> bool {
        self.schedule_type.is_some()
    }
}

pub fn manifold_event_log_ttl() -> anyhow::Result<Ttl> {
    manifold_event_log_ttl_impl(ROBOTS, username().ok().flatten(), ScheduleType::new()?)
}

fn manifold_event_log_ttl_impl(
    robots: &[&str],
    username: Option<String>,
    schedule_type: ScheduleType,
) -> anyhow::Result<Ttl> {
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

    // 3. return if it's not continuous
    if schedule_type.is_some() && !schedule_type.is_continuous() {
        return Ok(Ttl::from_days(CI_EXCEPT_CONTINUOUS_TTL_DAYS));
    }

    // 4. use default
    Ok::<Ttl, anyhow::Error>(Ttl::from_days(DEFAULT_TTL_DAYS))
}

#[cfg(test)]
mod tests {
    use super::*;

    impl ScheduleType {
        fn testing_new(schedule_type: &'static str) -> Self {
            Self {
                schedule_type: Some(schedule_type),
            }
        }

        fn testing_empty() -> Self {
            Self {
                schedule_type: None,
            }
        }
    }

    #[test]
    fn test_is_a_user() -> anyhow::Result<()> {
        assert_eq!(
            manifold_event_log_ttl_impl(
                &["twsvcscm"],
                Some("random_person".to_owned()),
                ScheduleType::testing_new("continuous")
            )?
            .as_secs(),
            365 * 24 * 60 * 60,
        );
        Ok(())
    }

    #[test]
    fn test_not_a_user() -> anyhow::Result<()> {
        assert_eq!(
            manifold_event_log_ttl_impl(
                &["twsvcscm"],
                Some("twsvcscm".to_owned()),
                ScheduleType::testing_empty()
            )?
            .as_secs(),
            60 * 24 * 60 * 60,
        );
        Ok(())
    }

    #[test]
    fn test_not_a_user_and_not_continuous() -> anyhow::Result<()> {
        assert_eq!(
            manifold_event_log_ttl_impl(
                &["twsvcscm"],
                Some("twsvcscm".to_owned()),
                ScheduleType::testing_new("foo")
            )?
            .as_secs(),
            28 * 24 * 60 * 60,
        );
        Ok(())
    }

    #[test]
    fn test_not_a_user_and_continuous() -> anyhow::Result<()> {
        assert_eq!(
            manifold_event_log_ttl_impl(
                &["twsvcscm"],
                Some("twsvcscm".to_owned()),
                ScheduleType::testing_new("continuous")
            )?
            .as_secs(),
            60 * 24 * 60 * 60,
        );
        Ok(())
    }
}
