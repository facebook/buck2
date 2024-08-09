/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[allow(clippy::doc_lazy_continuation)]
pub mod google {
    pub mod api {
        tonic::include_proto!("google.api");
    }
    pub mod bytestream {
        tonic::include_proto!("google.bytestream");
    }
    pub mod longrunning {
        tonic::include_proto!("google.longrunning");
    }
    pub mod rpc {
        tonic::include_proto!("google.rpc");
    }
}

#[allow(clippy::doc_lazy_continuation)]
pub mod build {
    pub mod bazel {
        pub mod semver {
            tonic::include_proto!("build.bazel.semver");
        }
        pub mod remote {
            pub mod execution {
                pub mod v2 {
                    tonic::include_proto!("build.bazel.remote.execution.v2");
                }
            }
        }
    }
}

pub mod serialize_vec_any {
    use serde::Deserialize;
    use serde::Deserializer;
    use serde::Serialize;
    use serde::Serializer;

    pub fn serialize<S>(value: &[::prost_types::Any], serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let d: Vec<(String, Vec<u8>)> = value
            .iter()
            .map(|v| (v.type_url.clone(), v.value.clone()))
            .collect();
        d.serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<::prost_types::Any>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d: Vec<::prost_types::Any> = Vec::deserialize(deserializer)?
            .into_iter()
            .map(|(type_url, value)| ::prost_types::Any { type_url, value })
            .collect();
        Ok(d)
    }
}

pub mod serialize_option_any {
    use serde::Deserialize;
    use serde::Deserializer;
    use serde::Serialize;
    use serde::Serializer;

    pub fn serialize<S>(
        value: &Option<::prost_types::Any>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let d = value
            .as_ref()
            .map(|v| (v.type_url.clone(), v.value.clone()));
        d.serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<::prost_types::Any>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d = Option::<(String, Vec<u8>)>::deserialize(deserializer)?
            .map(|(type_url, value)| ::prost_types::Any { type_url, value });
        Ok(d)
    }
}

pub mod serialize_any {
    use serde::Deserialize;
    use serde::Deserializer;
    use serde::Serialize;
    use serde::Serializer;

    pub fn serialize<S>(value: &::prost_types::Any, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let d = (value.type_url.clone(), value.value.clone());
        d.serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<::prost_types::Any, D::Error>
    where
        D: Deserializer<'de>,
    {
        let d = <(String, Vec<u8>)>::deserialize(deserializer)?;
        let d = ::prost_types::Any {
            type_url: d.0,
            value: d.1,
        };
        Ok(d)
    }
}
