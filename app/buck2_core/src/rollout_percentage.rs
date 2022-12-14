/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;

use gazebo::prelude::*;
use os_str_bytes::OsStrBytes;
use rand::Rng;

/// Returns true or false for percentage-based feature rollouts based on a configuration string.
/// Configurations supported today are random and hostname.
/// - Random: Enabled by directly setting a decimal value.
///     Checks whether to enable feature based on a random roll
/// - Hostname: Set by "hostname=<value>", ex. "hostname=0.5". Checks whether to roll out feature
///     based on hash of hostname. Useful when you want the same host to consistently get the
///     same feature enabled/disabled.
/// It's possible to extend this system to support per-username rollout as well in addition to
/// per-host rollout.
#[derive(Copy, Clone, Dupe, Debug)]
pub struct RolloutPercentage {
    inner: Inner,
}

impl RolloutPercentage {
    pub fn roll(&self) -> bool {
        match self.inner {
            Inner::Hostname(pct) => {
                if let Ok(hostname) = hostname::get() {
                    let hash = blake3::hash(&hostname.to_raw_bytes());
                    // For simplicity, we just divide the value of the first byte by 255
                    // to get a decimal value to compare against our percentage.
                    // TODO(scottcao): Use get_shard internally
                    (hash.as_bytes()[0] as f64 / std::u8::MAX as f64) < pct
                } else {
                    tracing::warn!("Unable to obtain hostname");
                    false
                }
            }
            Inner::Rate(pct) => rand::thread_rng().gen::<f64>() < pct,
            Inner::Bool(b) => b,
        }
    }

    pub fn from_bool(val: bool) -> Self {
        Self {
            inner: Inner::Bool(val),
        }
    }

    pub fn never() -> Self {
        Self::from_bool(false)
    }

    pub fn always() -> Self {
        Self::from_bool(true)
    }
}

impl FromStr for RolloutPercentage {
    type Err = anyhow::Error;

    fn from_str(val: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            inner: val.parse()?,
        })
    }
}

#[derive(Copy, Clone, Dupe, Debug)]
enum Inner {
    Rate(f64),
    Hostname(f64),
    Bool(bool),
}

impl FromStr for Inner {
    type Err = anyhow::Error;

    fn from_str(val: &str) -> Result<Self, Self::Err> {
        if let Some(val) = val.strip_prefix("hostname:") {
            let val = val.parse()?;
            let val = rate(val)?;
            return Ok(Inner::Hostname(val));
        }

        if let Ok(val) = val.parse() {
            let val = rate(val)?;
            return Ok(Inner::Rate(val));
        }

        if let Ok(val) = val.parse() {
            return Ok(Inner::Bool(val));
        }

        Err(anyhow::anyhow!(
            "RolloutPercentage must be either a float or a bool"
        ))
    }
}

fn rate(val: f64) -> anyhow::Result<f64> {
    if (0.0..=1.0).contains(&val) {
        Ok(val)
    } else {
        Err(anyhow::anyhow!(
            "RolloutPercentage floats must be within [0,1] (got: {})",
            val
        ))
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

    use super::*;

    #[test]
    fn test_parse_ok() {
        assert_matches!("true".parse(), Ok(Inner::Bool(true)));

        // NOTE: 0.5 and 1 are exact values for floats
        assert_matches!("0.5".parse(), Ok(Inner::Rate(v)) if v == 0.5);
        assert_matches!("1".parse(), Ok(Inner::Rate(v)) if v == 1.0);
    }

    #[test]
    fn test_parse_err() {
        assert_matches!(Inner::from_str("foo"), Err(..));
        assert_matches!(Inner::from_str("-1"), Err(..));
        assert_matches!(Inner::from_str("1.1"), Err(..));
    }

    #[test]
    fn test_roll() {
        assert!(
            RolloutPercentage {
                inner: Inner::Bool(true)
            }
            .roll()
        );

        for _ in 0..1000 {
            assert!(
                RolloutPercentage {
                    inner: Inner::Rate(1.0)
                }
                .roll()
            );
            assert!(
                !RolloutPercentage {
                    inner: Inner::Rate(0.0)
                }
                .roll()
            );
        }

        for _ in 0..1000 {
            assert!(
                RolloutPercentage {
                    inner: Inner::Hostname(1.0)
                }
                .roll()
            );
            assert!(
                !RolloutPercentage {
                    inner: Inner::Hostname(0.0)
                }
                .roll()
            );
        }
    }
}
