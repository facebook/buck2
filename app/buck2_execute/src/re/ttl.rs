/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Computing expiration timestamps from TTLs reported by RE.

use std::fmt::Display;

use buck2_core::soft_error;
use chrono::DateTime;
use chrono::TimeDelta;
use chrono::Utc;

/// Whether an expiration had to be clamped to the representable datetime range.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Clamped {
    No,
    Min,
    Max,
}

/// Never panics: results outside the representable datetime range are clamped to it, in the
/// direction of the TTL's sign.
fn expiration_from_ttl(now: DateTime<Utc>, ttl_seconds: i64) -> (DateTime<Utc>, Clamped) {
    match TimeDelta::try_seconds(ttl_seconds).and_then(|ttl| now.checked_add_signed(ttl)) {
        Some(expiration) => (expiration, Clamped::No),
        None if ttl_seconds >= 0 => (DateTime::<Utc>::MAX_UTC, Clamped::Max),
        None => (DateTime::<Utc>::MIN_UTC, Clamped::Min),
    }
}

/// Computes the expiration timestamp for something that RE reports as living for another
/// `ttl_seconds` seconds.
///
/// TTLs reported by RE are not trusted: values for which the expiration is not representable
/// produce a clamped expiration and a quiet soft error naming `what`, instead of a panic.
pub fn re_expiration_from_ttl(
    now: DateTime<Utc>,
    ttl_seconds: i64,
    what: &dyn Display,
) -> DateTime<Utc> {
    let (expiration, clamped) = expiration_from_ttl(now, ttl_seconds);
    if clamped != Clamped::No {
        let _ignored = soft_error!(
            "re_digest_ttl_out_of_bounds",
            buck2_error::buck2_error!(
                buck2_error::ErrorTag::ReInvalidGetCasResponse,
                "RE returned a TTL outside the supported range; clamping the expiration of `{}`. TTL seconds: `{}`",
                what,
                ttl_seconds
            ),
            quiet: true
        );
    }
    expiration
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A TTL small enough for `TimeDelta` but big enough that the expiration exceeds the
    /// `DateTime` range. This is the S671995 pattern: a TTL in nanoseconds mistakenly
    /// reported in a seconds field.
    const NS_AS_SECONDS_TTL: i64 = 2_600_000_000_000_000;

    #[test]
    fn test_in_range() {
        let now = Utc::now();
        assert_eq!(
            expiration_from_ttl(now, 600),
            (now + TimeDelta::try_seconds(600).unwrap(), Clamped::No)
        );
        assert_eq!(
            expiration_from_ttl(now, -3600),
            (now - TimeDelta::try_seconds(3600).unwrap(), Clamped::No)
        );
        assert_eq!(expiration_from_ttl(now, 0), (now, Clamped::No));
    }

    #[test]
    fn test_expiration_exceeds_datetime_range() {
        let now = Utc::now();
        assert_eq!(
            expiration_from_ttl(now, NS_AS_SECONDS_TTL),
            (DateTime::<Utc>::MAX_UTC, Clamped::Max)
        );
        assert_eq!(
            expiration_from_ttl(now, -NS_AS_SECONDS_TTL),
            (DateTime::<Utc>::MIN_UTC, Clamped::Min)
        );
    }

    #[test]
    fn test_ttl_exceeds_time_delta_range() {
        let now = Utc::now();
        assert_eq!(
            expiration_from_ttl(now, i64::MAX),
            (DateTime::<Utc>::MAX_UTC, Clamped::Max)
        );
        assert_eq!(
            expiration_from_ttl(now, i64::MIN),
            (DateTime::<Utc>::MIN_UTC, Clamped::Min)
        );
    }
}
