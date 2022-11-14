/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::cas_digest::CasDigest;
use buck2_common::cas_digest::TrackedCasDigest;

pub type ReDigest = remote_execution::TDigest;

pub type GrpcDigest = remote_execution::Digest;

pub trait CasDigestFromReExt {
    fn from_re(x: &ReDigest) -> Self;
    fn from_grpc(x: &GrpcDigest) -> Self;
}

pub trait CasDigestToReExt {
    fn to_re(&self) -> ReDigest;
    fn to_grpc(&self) -> GrpcDigest;
}

impl<Kind> CasDigestFromReExt for CasDigest<Kind> {
    fn from_re(x: &ReDigest) -> Self {
        Self::new_sha1(
            Self::parse_digest_sha1_without_size(x.hash.as_bytes()).unwrap_or_else(|err| {
                panic!(
                    "Invalid ReDigest {}:{}, error {:#}",
                    x.hash, x.size_in_bytes, err
                )
            }),
            x.size_in_bytes as u64,
        )
    }

    fn from_grpc(x: &GrpcDigest) -> Self {
        Self::new_sha1(
            Self::parse_digest_sha1_without_size(x.hash.as_bytes()).unwrap_or_else(|err| {
                panic!(
                    "Invalid GrpcDigest {}:{}, error {:#}",
                    x.hash, x.size_bytes, err
                )
            }),
            x.size_bytes as u64,
        )
    }
}

impl<Kind> CasDigestToReExt for TrackedCasDigest<Kind> {
    fn to_re(&self) -> ReDigest {
        self.data().to_re()
    }

    fn to_grpc(&self) -> GrpcDigest {
        self.data().to_grpc()
    }
}

impl<Kind> CasDigestToReExt for CasDigest<Kind> {
    fn to_re(&self) -> ReDigest {
        ReDigest {
            hash: hex::encode(self.sha1()),
            size_in_bytes: self.size() as i64,
            ..Default::default()
        }
    }

    fn to_grpc(&self) -> GrpcDigest {
        GrpcDigest {
            hash: hex::encode(self.sha1()),
            size_bytes: self.size() as i64,
        }
    }
}

pub trait FileDigestFromProtoExt {
    fn from_proto_message<M: prost::Message>(m: &M) -> Self;
}

impl<Kind> FileDigestFromProtoExt for CasDigest<Kind> {
    fn from_proto_message<M: prost::Message>(m: &M) -> Self {
        let mut m_encoded = Vec::new();
        m.encode(&mut m_encoded)
            .unwrap_or_else(|e| unreachable!("Protobuf messages are always encodeable: {}", e));
        Self::from_bytes_sha1(m_encoded.as_slice())
    }
}
