/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::ffi::OsString;
use std::str::FromStr;

use buck2_error::buck2_error;
use dupe::Dupe;
use os_str_bytes::OsStrBytes;
use rand::Rng;

/// Returns true or false for percentage-based feature rollouts based on a configuration string.
/// Configurations supported today are random and hostname.
///
/// - Daemon: Enabled by setting `daemon:<value>`, ex. `daemon:0.5`.
///   Checks whether to enable feature based on a random roll on daemon startup.
///
/// - Hostname: Set by `hostname:<value>`, ex. `hostname:0.5`, or by setting value directly,
///   ex. `0.5`. Checks whether to roll out feature based on hash of hostname. Useful when you
///   want the same host to consistently get the same feature enabled/disabled.
///
/// It's possible to extend this system to support per-username rollout as well in addition to
/// per-host rollout.
#[derive(Copy, Clone, Dupe, Debug)]
pub struct RolloutPercentage {
    inner: Inner,
}

impl RolloutPercentage {
    pub fn roll(&self) -> bool {
        self.roll_inner(|| hostname::get().ok())
    }

    fn roll_inner<F>(&self, get_hostname: F) -> bool
    where
        F: FnOnce() -> Option<OsString>,
    {
        match self.inner {
            Inner::Hostname(pct) => {
                match get_hostname() {
                    Some(h) => {
                        let hash = blake3::hash(&h.to_raw_bytes());
                        // For simplicity, we just divide the value of the first byte by 256 to get a
                        // decimal value to compare against our percentage. Note that the first byte
                        // can never exceed 255, so if we set the percentage to 100% we return true.
                        // TODO(scottcao): Use get_shard internally
                        (hash.as_bytes()[0] as f64 / 256_f64) < pct
                    }
                    None => {
                        tracing::warn!("Unable to obtain hostname");
                        pct == 1f64 // 1.0 is an exact value in floats, we *can* compare that one.
                    }
                }
            }
            Inner::Rate(pct) => rand::rng().random::<f64>() < pct,
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
    type Err = buck2_error::Error;

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
    type Err = buck2_error::Error;

    fn from_str(val: &str) -> Result<Self, Self::Err> {
        if let Some(val) = val.strip_prefix("hostname:") {
            return Ok(Inner::Hostname(rate(val.parse()?)?));
        }

        if let Some(val) = val.strip_prefix("daemon:") {
            return Ok(Inner::Rate(rate(val.parse()?)?));
        }

        if let Ok(val) = val.parse() {
            return Ok(Inner::Hostname(rate(val)?));
        }

        if let Ok(val) = val.parse() {
            return Ok(Inner::Bool(val));
        }

        Err(buck2_error!(
            buck2_error::ErrorTag::Input,
            "RolloutPercentage must be either a float, a bool, a `daemon:<float>` or a `hostname:<float>` (got: {})",
            &val,
        ))
    }
}

fn rate(val: f64) -> buck2_error::Result<f64> {
    if (0.0..=1.0).contains(&val) {
        Ok(val)
    } else {
        Err(buck2_error!(
            buck2_error::ErrorTag::Input,
            "RolloutPercentage floats must be within [0,1] (got: {})",
            val
        ))
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use rand::distr::Alphanumeric;
    use rand::distr::SampleString;

    use super::*;

    #[test]
    fn test_parse_ok() {
        assert_matches!("true".parse(), Ok(Inner::Bool(true)));

        // NOTE: 0.5 and 1 are exact values for floats
        assert_matches!("0.5".parse(), Ok(Inner::Hostname(v)) if v == 0.5);
        assert_matches!("1".parse(), Ok(Inner::Hostname(v)) if v == 1.0);

        assert_matches!("daemon:0.5".parse(), Ok(Inner::Rate(v)) if v == 0.5);
        assert_matches!("hostname:0.5".parse(), Ok(Inner::Hostname(v)) if v == 0.5);
    }

    #[test]
    fn test_parse_err() {
        assert_matches!(Inner::from_str("foo"), Err(..));
        assert_matches!(Inner::from_str("-1"), Err(..));
        assert_matches!(Inner::from_str("1.1"), Err(..));
    }

    #[test]
    fn test_roll() {
        let hostname = || {
            Some(OsString::from(
                Alphanumeric.sample_string(&mut rand::rng(), 16),
            ))
        };

        assert!(
            RolloutPercentage {
                inner: Inner::Bool(true)
            }
            .roll_inner(hostname)
        );

        for _ in 0..1000 {
            assert!(
                RolloutPercentage {
                    inner: Inner::Rate(1.0)
                }
                .roll_inner(hostname)
            );
            assert!(
                !RolloutPercentage {
                    inner: Inner::Rate(0.0)
                }
                .roll_inner(hostname)
            );
        }

        for _ in 0..1000 {
            assert!(
                RolloutPercentage {
                    inner: Inner::Hostname(1.0)
                }
                .roll_inner(hostname)
            );
            assert!(
                !RolloutPercentage {
                    inner: Inner::Hostname(0.0)
                }
                .roll_inner(hostname)
            );
        }
    }

    #[test]
    fn test_roll_no_hostname() {
        let hostname = || None;

        assert!(
            !RolloutPercentage {
                inner: Inner::Hostname(0.99999)
            }
            .roll_inner(hostname)
        );

        assert!(
            RolloutPercentage {
                inner: Inner::Hostname(1.0)
            }
            .roll_inner(hostname)
        );
    }
}
