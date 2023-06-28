/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::fmt;

pub mod action_key_owner;

mod serialize_timestamp {
    use serde::Deserialize;
    use serde::Deserializer;
    use serde::Serialize;
    use serde::Serializer;

    pub fn serialize<S>(
        value: &Option<::prost_types::Timestamp>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let d = value.as_ref().map(|v| (v.seconds, v.nanos));
        d.serialize(serializer)
    }

    pub fn deserialize<'de, D>(
        deserializer: D,
    ) -> Result<Option<::prost_types::Timestamp>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d = Option::<(i64, i32)>::deserialize(deserializer)?;
        let d = d.map(|(seconds, nanos)| ::prost_types::Timestamp { seconds, nanos });
        Ok(d)
    }
}

mod serialize_bytes {
    use serde::Deserialize;
    use serde::Deserializer;
    use serde::Serialize;
    use serde::Serializer;

    pub fn serialize<S>(value: &[u8], serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let d = hex::encode(value);
        d.serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d = String::deserialize(deserializer)?;
        let d = hex::decode(d).map_err(serde::de::Error::custom)?;
        Ok(d)
    }
}

mod serialize_action_kind {
    use serde::Deserialize;
    use serde::Deserializer;
    use serde::Serialize;
    use serde::Serializer;

    #[allow(clippy::trivially_copy_pass_by_ref)]
    pub fn serialize<S>(value: &i32, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let k = crate::ActionKind::from_i32(*value).ok_or_else(|| {
            serde::ser::Error::custom(format!("Invalid ActionKind enum value: {}", value))
        })?;
        k.serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<i32, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d = crate::ActionKind::deserialize(deserializer)?;
        Ok(d as i32)
    }
}

tonic::include_proto!("buck.data");

/// Trait for things that can be converted into protobuf messages, for ease of emitting events. There are many core Buck
/// types that are represented in the Daemon API that use this trait to ease conversion.
pub trait ToProtoMessage {
    type Message: prost::Message;

    fn as_proto(&self) -> Self::Message;
}

/// Write out a human-readable description of the error tags
/// that is printed out in the context stack when program fails.
impl fmt::Display for ErrorCategory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match &self {
            ErrorCategory::Infra => "This error is an internal Buck2 error",
            ErrorCategory::User => "This error was caused by the end user",
        };

        write!(f, "{}", msg)
    }
}

impl fmt::Display for ErrorCause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match &self {
            ErrorCause::InvalidPackage => "The package is invalid",
            ErrorCause::DaemonIsBusy => "Buck daemon is busy processing another command",
        };

        write!(f, "{}", msg)
    }
}

impl fmt::Display for DaemonShutdown {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}, caller:", self.reason)?;

        for caller in self.callers.iter() {
            let max_len = 70;

            let short_caller = if caller.len() > max_len {
                Cow::Owned(
                    caller
                        .chars()
                        .take(max_len)
                        .chain(std::iter::repeat('.').take(3))
                        .collect(),
                )
            } else {
                Cow::Borrowed(caller)
            };

            writeln!(f)?;
            write!(f, "  * {}", short_caller)?;
        }

        Ok(())
    }
}

pub mod serialize_duration_as_micros {
    use serde::Deserialize;
    use serde::Deserializer;
    use serde::Serialize;
    use serde::Serializer;

    pub fn serialize<S>(
        value: &Option<::prost_types::Duration>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        value.as_ref().map(to_micros).serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<::prost_types::Duration>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = Option::<i64>::deserialize(deserializer)?;
        Ok(value.map(from_micros))
    }

    const MICROS_IN_SECOND: i64 = 1000000;
    const NANOS_IN_MICRO: i32 = 1000;

    /// We saturate here since technically not all ::prost_types::Duration would fit (but all those
    /// we care about would).
    fn to_micros(duration: &::prost_types::Duration) -> i64 {
        let mut micros: i64 = 0;
        micros = micros.saturating_add(duration.seconds.saturating_mul(MICROS_IN_SECOND));
        micros = micros.saturating_add(duration.nanos.saturating_div(NANOS_IN_MICRO).into());
        micros
    }

    fn from_micros(duration: i64) -> ::prost_types::Duration {
        let seconds = duration / MICROS_IN_SECOND;
        let nanos = ((duration % MICROS_IN_SECOND) as i32) * NANOS_IN_MICRO;
        prost_types::Duration {
            seconds,
            nanos: nanos as _,
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_value() {
            assert_eq!(
                to_micros(&::prost_types::Duration {
                    seconds: 1,
                    nanos: 1000
                }),
                1000001
            );
        }

        #[test]
        fn test_roundtrip() {
            for v in [-1, 10, i64::MAX, i64::MIN, 0] {
                assert_eq!(to_micros(&from_micros(v)), v);
            }
        }
    }
}
