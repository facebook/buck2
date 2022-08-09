use std::str::FromStr;

use gazebo::prelude::*;
use rand::Rng;

/// This can produce random rolls driven by configuration. This does not (currently) support
/// per-host or per-user seeds: it's just pure random. We could perhaps extend this by allowing the
/// config to specify the seed as well, e.g. `user=0.5` to indicate that the seed should be the
/// username and the rollout should be enabled for 50% of users.
#[derive(Copy, Clone, Dupe, Debug)]
pub struct RolloutPercentage {
    inner: Inner,
}

impl RolloutPercentage {
    pub fn roll(&self) -> bool {
        match self.inner {
            Inner::Rate(pct) => rand::thread_rng().gen::<f64>() < pct,
            Inner::Bool(b) => b,
        }
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
    Bool(bool),
}

impl FromStr for Inner {
    type Err = anyhow::Error;

    fn from_str(val: &str) -> Result<Self, Self::Err> {
        if let Ok(val) = val.parse() {
            if !(0.0..=1.0).contains(&val) {
                return Err(anyhow::anyhow!(
                    "RolloutPercentage floats must be within [0,1] (got: {})",
                    val
                ));
            }
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
    }
}
