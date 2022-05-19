/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod serialize_duration {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn serialize<S>(
        value: &Option<::prost_types::Duration>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let d = value.as_ref().map(|v| (v.seconds, v.nanos));
        d.serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<::prost_types::Duration>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d = Option::<(i64, i32)>::deserialize(deserializer)?;
        let d = d.map(|(seconds, nanos)| ::prost_types::Duration { seconds, nanos });
        Ok(d)
    }
}

mod serialize_timestamp {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

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
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

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
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

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
