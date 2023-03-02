/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use buck2_common::cas_digest::CasDigest;
use buck2_common::cas_digest::CasDigestParseError;
use buck2_common::cas_digest::TrackedCasDigest;
use remote_execution::Digest;
use remote_execution::TDigest;
use thiserror::Error;

use crate::digest_config::DigestConfig;

#[derive(Error, Debug)]
pub enum DigestConversionError {
    #[error("Error parsing digest: `{}`", digest)]
    ParseError {
        digest: String,

        #[source]
        error: CasDigestParseError,
    },
}

pub trait CasDigestFromReExt: Sized {
    fn from_re(x: &TDigest, digest_config: DigestConfig) -> Result<Self, DigestConversionError>;
    fn from_grpc(x: &Digest, digest_config: DigestConfig) -> Result<Self, DigestConversionError>;
}

pub trait CasDigestToReExt {
    fn to_re(&self) -> TDigest;
    fn to_grpc(&self) -> Digest;
}

impl<Kind> CasDigestFromReExt for CasDigest<Kind> {
    fn from_re(
        digest: &TDigest,
        digest_config: DigestConfig,
    ) -> Result<Self, DigestConversionError> {
        Ok(Self::new(
            Self::parse_digest_without_size(&digest.hash, digest_config.cas_digest_config())
                .map_err(|error| DigestConversionError::ParseError {
                    digest: digest.to_string(),
                    error,
                })?,
            digest.size_in_bytes as u64,
        ))
    }

    fn from_grpc(
        digest: &Digest,
        digest_config: DigestConfig,
    ) -> Result<Self, DigestConversionError> {
        Ok(Self::new(
            Self::parse_digest_without_size(&digest.hash, digest_config.cas_digest_config())
                .map_err(|error| DigestConversionError::ParseError {
                    digest: format!("{}:{}", digest.hash, digest.size_bytes),
                    error,
                })?,
            digest.size_bytes as u64,
        ))
    }
}

pub trait CasDigestConversionResultExt {
    fn as_display(&self) -> &dyn fmt::Display;
}

impl<Kind> CasDigestConversionResultExt for Result<CasDigest<Kind>, DigestConversionError> {
    fn as_display(&self) -> &dyn fmt::Display {
        match self {
            Self::Ok(ref v) => v as _,
            Self::Err(DigestConversionError::ParseError { ref digest, .. }) => digest as _,
        }
    }
}

impl<Kind> CasDigestToReExt for TrackedCasDigest<Kind> {
    fn to_re(&self) -> TDigest {
        self.data().to_re()
    }

    fn to_grpc(&self) -> Digest {
        self.data().to_grpc()
    }
}

impl<Kind> CasDigestToReExt for CasDigest<Kind> {
    fn to_re(&self) -> TDigest {
        TDigest {
            hash: hex::encode(self.digest().as_bytes()),
            size_in_bytes: self.size() as i64,
            ..Default::default()
        }
    }

    fn to_grpc(&self) -> Digest {
        Digest {
            hash: hex::encode(self.digest().as_bytes()),
            size_bytes: self.size() as i64,
        }
    }
}
