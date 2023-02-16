/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Read;
use std::marker::PhantomData;
use std::sync::atomic::AtomicI64;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use allocative::Allocative;
use chrono::DateTime;
use chrono::TimeZone;
use chrono::Utc;
use derivative::Derivative;
use derive_more::Display;
use digest::Digest;
use dupe::Clone_;
use dupe::Dupe;
use dupe::Dupe_;
use num_enum::TryFromPrimitive;
use once_cell::sync::OnceCell;
use sha1::Sha1;
use sha2::Sha256;
use thiserror::Error;

use crate::file_ops::FileDigestKind;

/// The number of bytes required by a SHA-1 hash
pub const SHA1_SIZE: usize = 20;

/// The number of bytes required by a SHA-256 hash
pub const SHA256_SIZE: usize = 32;

/// The number of bytes required by a Blake3 hash
pub const BLAKE3_SIZE: usize = 32;

/// The bytes that make up a file digest.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Allocative, Clone)]
pub enum RawDigest {
    Sha1([u8; SHA1_SIZE]),
    Sha256([u8; SHA256_SIZE]),
    Blake3([u8; BLAKE3_SIZE]),
}

// We consider copying 20 bytes is cheap enough not to qualify for Dupe
impl Dupe for RawDigest {}

impl RawDigest {
    pub fn as_bytes(&self) -> &[u8] {
        match self {
            Self::Sha1(x) => x,
            Self::Sha256(x) => x,
            Self::Blake3(x) => x,
        }
    }

    pub fn algorithm(&self) -> DigestAlgorithm {
        match self {
            Self::Sha1(..) => DigestAlgorithm::Sha1,
            Self::Sha256(..) => DigestAlgorithm::Sha256,
            Self::Blake3(..) => DigestAlgorithm::Blake3,
        }
    }

    pub fn parse_sha1(data: &[u8]) -> Result<Self, CasDigestParseError> {
        let mut sha1 = [0; SHA1_SIZE];
        hex::decode_to_slice(data, &mut sha1).map_err(CasDigestParseError::InvalidSha1)?;
        Ok(RawDigest::Sha1(sha1))
    }
}

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Copy, Clone, Dupe, Hash)]
#[repr(u8)]
pub enum DigestAlgorithm {
    Sha1,
    Sha256,
    Blake3,
}

#[derive(Copy, Clone, Dupe, Debug, Allocative, Hash, Eq, PartialEq)]
pub struct CasDigestConfig {}

impl CasDigestConfig {
    /// Allow optimizing the empty file digest path, we do that by having the CasDigestConfig hold
    /// a cell for it (later in this stack).
    pub fn empty_file_digest(self) -> crate::file_ops::TrackedFileDigest {
        static EMPTY_DIGEST: OnceCell<TrackedCasDigest<FileDigestKind>> = OnceCell::new();

        EMPTY_DIGEST
            .get_or_init(|| TrackedCasDigest {
                inner: Arc::new(TrackedCasDigestInner {
                    data: CasDigest::empty(self),
                    expires: AtomicI64::new(0),
                }),
            })
            .dupe()
    }

    pub fn compat() -> Self {
        Self {}
    }

    pub fn preferred_algorithm(self) -> DigestAlgorithm {
        // TODO (DigestConfig): actually make this configurable
        DigestAlgorithm::Sha1
    }
}

/// Separate struct to allow us to use  `repr(transparent)` below and guarantee an identical
/// layout.
#[derive(Display, PartialEq, Eq, PartialOrd, Ord, Hash, Allocative, Clone, Dupe)]
#[display(fmt = "{}:{}", "hex::encode(digest.as_bytes())", size)]
struct CasDigestData {
    size: u64,
    digest: RawDigest,
}

#[derive(Display, Derivative, Allocative, Clone_, Dupe_)]
#[allocative(bound = "")]
#[derivative(PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display(fmt = "{}", data)]
#[repr(transparent)]
pub struct CasDigest<Kind> {
    data: CasDigestData,
    #[derivative(Hash = "ignore", PartialEq = "ignore", PartialOrd = "ignore")]
    kind: PhantomData<Kind>,
}

impl<Kind> fmt::Debug for CasDigest<Kind> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl<Kind> CasDigest<Kind> {
    pub fn from_digest_bytes(
        kind: DigestAlgorithm,
        digest: &[u8],
        size: u64,
    ) -> anyhow::Result<Self> {
        Ok(match kind {
            DigestAlgorithm::Sha1 => Self::new_sha1(digest.try_into()?, size),
            DigestAlgorithm::Sha256 => Self::new_sha256(digest.try_into()?, size),
            DigestAlgorithm::Blake3 => Self::new_blake3(digest.try_into()?, size),
        })
    }

    pub fn new(digest: RawDigest, size: u64) -> Self {
        Self {
            data: CasDigestData { size, digest },
            kind: PhantomData,
        }
    }

    pub fn new_sha1(sha1: [u8; SHA1_SIZE], size: u64) -> Self {
        Self {
            data: CasDigestData {
                size,
                digest: RawDigest::Sha1(sha1),
            },
            kind: PhantomData,
        }
    }

    pub fn new_sha256(sha256: [u8; SHA256_SIZE], size: u64) -> Self {
        Self {
            data: CasDigestData {
                size,
                digest: RawDigest::Sha256(sha256),
            },
            kind: PhantomData,
        }
    }

    pub fn new_blake3(blake3: [u8; BLAKE3_SIZE], size: u64) -> Self {
        Self {
            data: CasDigestData {
                size,
                digest: RawDigest::Blake3(blake3),
            },
            kind: PhantomData,
        }
    }

    pub fn digest(&self) -> &RawDigest {
        &self.data.digest
    }

    pub fn size(&self) -> u64 {
        self.data.size
    }

    /// A tiny representation of this digest, useful for logging when the full sha1 presentation is
    /// too expensive.
    pub fn tiny_digest(&self) -> TinyDigest<'_, Kind> {
        TinyDigest { of: self }
    }

    pub fn parse_digest(s: &str, config: CasDigestConfig) -> Result<Self, CasDigestParseError> {
        let (digest, size) = s
            .split_once(':')
            .ok_or(CasDigestParseError::MissingSizeSeparator)?;

        let digest = CasDigest::<Kind>::parse_digest_without_size(digest.as_bytes(), config)?;
        let size = size.parse().map_err(CasDigestParseError::InvalidSize)?;

        Ok(Self::new(digest, size))
    }

    pub fn parse_digest_without_size(
        data: &[u8],
        _config: CasDigestConfig,
    ) -> Result<RawDigest, CasDigestParseError> {
        // TODO (DigestConfig): actually select the preferred variant.
        RawDigest::parse_sha1(data)
    }

    /// Return the digest of an empty string
    pub fn empty(config: CasDigestConfig) -> Self {
        Self::from_content(&[], config)
    }

    pub fn from_content(bytes: &[u8], config: CasDigestConfig) -> Self {
        Self::from_content_for_algorithm(bytes, config.preferred_algorithm())
    }

    /// NOTE: Eventually this probably needs to take something that isn't DigestAlgorithm because
    /// we might need to deal with keyed Blake3.
    pub fn from_content_for_algorithm(bytes: &[u8], algorithm: DigestAlgorithm) -> Self {
        match algorithm {
            DigestAlgorithm::Sha1 => {
                let mut hasher = Sha1::new();
                hasher.update(bytes);
                Self::new_sha1(hasher.finalize().into(), bytes.len() as u64)
            }
            DigestAlgorithm::Sha256 => {
                let mut sha256 = Sha256::new();
                sha256.update(bytes);
                Self::new_sha256(sha256.finalize().into(), bytes.len() as u64)
            }
            DigestAlgorithm::Blake3 => {
                // NOTE: This is where keying would matter. Note that we don't need to actually
                // retain the key in RawDigest or DigestAlgorithm, since we never actually care
                // about which hash we have besides debugging purposes.
                let mut digest = blake3::Hasher::new();
                digest.update(bytes);
                Self::new_blake3(digest.finalize().into(), bytes.len() as u64)
            }
        }
    }

    pub fn from_reader<R: Read>(reader: R, config: CasDigestConfig) -> anyhow::Result<Self> {
        Self::from_reader_for_algorithm(reader, config.preferred_algorithm())
    }

    /// NOTE: Eventually this probably needs to take something that isn't DigestAlgorithm because
    /// we might need to deal with keyed Blake3.
    pub fn from_reader_for_algorithm<R: Read>(
        reader: R,
        algorithm: DigestAlgorithm,
    ) -> anyhow::Result<Self> {
        fn hash_reader<R: Read, H: Digest>(mut reader: R, hasher: &mut H) -> anyhow::Result<u64> {
            // Buffer size chosen based on benchmarks at D26176645
            // Also optimal for Blake3's SIMD implementation.
            let mut size = 0;
            let mut buffer = [0; 16 * 1024];
            loop {
                let count = reader.read(&mut buffer)?;
                if count == 0 {
                    break;
                }
                size += count as u64;
                hasher.update(&buffer[..count]);
            }

            Ok(size)
        }

        Ok(match algorithm {
            DigestAlgorithm::Sha1 => {
                let mut hasher = Sha1::new();
                let len = hash_reader(reader, &mut hasher)?;
                Self::new_sha1(hasher.finalize().into(), len)
            }
            DigestAlgorithm::Sha256 => {
                let mut hasher = Sha256::new();
                let len = hash_reader(reader, &mut hasher)?;
                Self::new_sha256(hasher.finalize().into(), len)
            }
            DigestAlgorithm::Blake3 => {
                // NOTE: This is where keying would matter. Note that we don't need to actually
                // retain the key in RawDigest or DigestAlgorithm, since we never actually care
                // about which hash we have besides debugging purposes.
                let mut hasher = blake3::Hasher::new();
                let len = hash_reader(reader, &mut hasher)?;
                Self::new_blake3(hasher.finalize().into(), len)
            }
        })
    }

    pub fn coerce<NewKind>(self) -> CasDigest<NewKind> {
        CasDigest {
            data: self.data,
            kind: PhantomData,
        }
    }
}

pub trait TrackedCasDigestKind: Sized + 'static {
    /// This needs to be a concrete implementation since we share the empty instance in a static
    /// but we can't have static generics.
    fn empty_digest(config: CasDigestConfig) -> Option<TrackedCasDigest<Self>>;
}

#[derive(Display)]
#[display(fmt = "{}", "hex::encode(&of.digest().as_bytes()[0..4])")]
pub struct TinyDigest<'a, Kind> {
    of: &'a CasDigest<Kind>,
}

#[derive(Error, Debug)]
pub enum CasDigestParseError {
    #[error("The digest is missing a size separator, it should look like `HASH:SIZE`")]
    MissingSizeSeparator,

    #[error("The SHA1 part of the digest is invalid")]
    InvalidSha1(#[source] hex::FromHexError),

    #[error("The size part of the digest is invalid")]
    InvalidSize(#[source] std::num::ParseIntError),
}

/// A digest to interact with RE. This, despite the name, can be a file or a directory. We track
/// the sha1 and the size of the underlying blob. We *also* keep track of its expiry in the CAS.
/// Note that for directory, the expiry represents that of the directory's blob, not its underlying
/// contents.
#[derive(Allocative)]
#[allocative(bound = "")]
struct TrackedCasDigestInner<Kind> {
    data: CasDigest<Kind>,
    expires: AtomicI64,
}

#[derive(Display, Dupe_, Allocative)]
#[allocative(bound = "")]
#[display(fmt = "{}", "self.data()")]
pub struct TrackedCasDigest<Kind> {
    inner: Arc<TrackedCasDigestInner<Kind>>,
}

impl<Kind> Clone for TrackedCasDigest<Kind> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.dupe(),
        }
    }
}

impl<Kind> Borrow<CasDigest<Kind>> for TrackedCasDigest<Kind> {
    fn borrow(&self) -> &CasDigest<Kind> {
        self.data()
    }
}

impl<'a, Kind> Borrow<CasDigest<Kind>> for &'a TrackedCasDigest<Kind> {
    fn borrow(&self) -> &CasDigest<Kind> {
        self.data()
    }
}

impl<Kind> PartialOrd for TrackedCasDigest<Kind> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.data().partial_cmp(other.data())
    }
}

impl<Kind> Ord for TrackedCasDigest<Kind> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.data().cmp(other.data())
    }
}

impl<Kind> PartialEq for TrackedCasDigest<Kind> {
    fn eq(&self, other: &Self) -> bool {
        self.data().eq(other.data())
    }
}

impl<Kind> Eq for TrackedCasDigest<Kind> {}

impl<Kind> Hash for TrackedCasDigest<Kind> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data().hash(state)
    }
}

impl<Kind> fmt::Debug for TrackedCasDigest<Kind> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{} expires at {}]",
            self,
            self.inner.expires.load(Ordering::Relaxed)
        )
    }
}

impl<Kind> buck2_core::directory::DirectoryDigest for TrackedCasDigest<Kind> {}

impl<Kind> TrackedCasDigest<Kind> {
    pub fn new(data: CasDigest<Kind>, config: CasDigestConfig) -> Self
    where
        Kind: TrackedCasDigestKind,
    {
        if data.size() == 0 {
            return Self::empty(config);
        }

        Self {
            inner: Arc::new(TrackedCasDigestInner {
                data,
                expires: AtomicI64::new(0),
            }),
        }
    }

    pub fn new_expires(
        data: CasDigest<Kind>,
        expiry: DateTime<Utc>,
        config: CasDigestConfig,
    ) -> Self
    where
        Kind: TrackedCasDigestKind,
    {
        let res = Self::new(data, config);
        res.update_expires(expiry);
        res
    }

    pub fn empty(config: CasDigestConfig) -> Self
    where
        Kind: TrackedCasDigestKind,
    {
        match Kind::empty_digest(config) {
            Some(o) => o,
            None => Self {
                inner: Arc::new(TrackedCasDigestInner {
                    data: CasDigest::empty(config),
                    expires: AtomicI64::new(0),
                }),
            },
        }
    }

    pub fn from_content(bytes: &[u8], config: CasDigestConfig) -> Self
    where
        Kind: TrackedCasDigestKind,
    {
        if bytes.is_empty() {
            return Self::empty(config);
        }

        Self {
            inner: Arc::new(TrackedCasDigestInner {
                data: CasDigest::from_content(bytes, config),
                expires: AtomicI64::new(0),
            }),
        }
    }

    pub fn data(&self) -> &CasDigest<Kind> {
        &self.inner.data
    }

    pub fn digest(&self) -> &RawDigest {
        self.inner.data.digest()
    }

    pub fn size(&self) -> u64 {
        self.inner.data.size()
    }

    pub fn expires(&self) -> DateTime<Utc> {
        Utc.timestamp_opt(self.inner.expires.load(Ordering::Relaxed), 0)
            .unwrap()
    }

    pub fn update_expires(&self, time: DateTime<Utc>) {
        self.inner
            .expires
            .store(time.timestamp(), Ordering::Relaxed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_digest_from_str() {
        let s = "0000000000000000000000000000000000000000:123";
        let config = CasDigestConfig::compat();
        assert_eq!(
            CasDigest::<()>::parse_digest(s, config)
                .unwrap()
                .to_string(),
            s
        );
    }
}
