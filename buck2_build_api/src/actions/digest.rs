/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::file_ops::FileDigest;

pub type ReDigest = remote_execution::TDigest;

pub type GrpcDigest = remote_execution::Digest;

pub trait FileDigestReExt {
    fn to_re(&self) -> ReDigest;

    fn from_re(x: &ReDigest) -> Self;

    fn to_grpc(&self) -> GrpcDigest;

    fn from_grpc(x: &GrpcDigest) -> Self;
}

impl FileDigestReExt for FileDigest {
    fn to_re(&self) -> ReDigest {
        ReDigest {
            hash: hex::encode(self.sha1()),
            size_in_bytes: self.size() as i64,
            ..Default::default()
        }
    }

    fn from_re(x: &ReDigest) -> Self {
        Self::new(
            Self::parse_digest(x.hash.as_bytes())
                .unwrap_or_else(|| panic!("Invalid ReDigest {}:{}", x.hash, x.size_in_bytes)),
            x.size_in_bytes as u64,
        )
    }

    fn to_grpc(&self) -> GrpcDigest {
        GrpcDigest {
            hash: hex::encode(self.sha1()),
            size_bytes: self.size() as i64,
        }
    }

    fn from_grpc(x: &GrpcDigest) -> Self {
        Self::new(
            Self::parse_digest(x.hash.as_bytes())
                .unwrap_or_else(|| panic!("Invalid GrpcDigest {}:{}", x.hash, x.size_bytes)),
            x.size_bytes as u64,
        )
    }
}

pub trait FileDigestProtoExt {
    fn from_proto_message<M: prost::Message>(m: &M) -> Self;
}

impl FileDigestProtoExt for FileDigest {
    fn from_proto_message<M: prost::Message>(m: &M) -> Self {
        let mut m_encoded = Vec::new();
        m.encode(&mut m_encoded)
            .unwrap_or_else(|e| unreachable!("Protobuf messages are always encodeable: {}", e));
        Self::from_bytes(m_encoded.as_slice())
    }
}
