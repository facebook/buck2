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
use gazebo::coerce::Coerce;
use gazebo::prelude::*;
use once_cell::sync::OnceCell;
use sha1::Digest;
use sha1::Sha1;
use thiserror::Error;

/// The number of bytes required by a SHA-1 hash
pub const SHA1_SIZE: usize = 20;

/// The number of bytes required by a SHA-256 hash
pub const SHA256_SIZE: usize = 32;

/// The number of bytes required by a Blake3 hash
pub const BLAKE3_SIZE: usize = 32;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Allocative, Clone)]
enum DigestHash {
    Sha1([u8; SHA1_SIZE]),
    Sha256([u8; SHA256_SIZE]),
    Blake3([u8; BLAKE3_SIZE]),
}

// We consider copying 20 bytes is cheap enough not to qualify for Dupe
impl Dupe for DigestHash {}

impl DigestHash {
    fn as_bytes(&self) -> &[u8] {
        match self {
            Self::Sha1(x) => x,
            Self::Sha256(x) => x,
            Self::Blake3(x) => x,
        }
    }
}

/// Separate struct to allow us to use  `repr(transparent)` below and guarantee an identical
/// layout.
#[derive(Display, PartialEq, Eq, PartialOrd, Ord, Hash, Allocative, Clone, Dupe)]
#[display(fmt = "{}:{}", "hex::encode(digest.as_bytes())", size)]
struct CasDigestData {
    size: u64,
    digest: DigestHash,
}

/// The bytes that make up a file digest.
#[derive(Display, Derivative, Coerce, Allocative, Clone_, Dupe_)]
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
    pub fn new_sha1(sha1: [u8; SHA1_SIZE], size: u64) -> Self {
        Self {
            data: CasDigestData {
                size,
                digest: DigestHash::Sha1(sha1),
            },
            kind: PhantomData,
        }
    }

    pub fn new_sha256(sha1: [u8; SHA256_SIZE], size: u64) -> Self {
        Self {
            data: CasDigestData {
                size,
                digest: DigestHash::Sha256(sha1),
            },
            kind: PhantomData,
        }
    }

    pub fn new_blake3(sha1: [u8; BLAKE3_SIZE], size: u64) -> Self {
        Self {
            data: CasDigestData {
                size,
                digest: DigestHash::Blake3(sha1),
            },
            kind: PhantomData,
        }
    }

    pub fn digest(&self) -> &[u8] {
        self.data.digest.as_bytes()
    }

    pub fn sha1(&self) -> &[u8; SHA1_SIZE] {
        match &self.data.digest {
            DigestHash::Sha1(x) => x,
            _ => panic!("Can;'t get the SHA1 for something that doesn't have it"),
        }
    }

    pub fn size(&self) -> u64 {
        self.data.size
    }

    /// A tiny representation of this digest, useful for logging when the full sha1 presentation is
    /// too expensive.
    pub fn tiny_digest(&self) -> TinyDigest<'_, Kind> {
        TinyDigest { of: self }
    }

    pub fn parse_digest_sha1(s: &str) -> Result<Self, CasDigestParseError> {
        let (sha1, size) = s
            .split_once(':')
            .ok_or(CasDigestParseError::MissingSizeSeparator)?;

        let sha1 = CasDigest::<Kind>::parse_digest_sha1_without_size(sha1.as_bytes())?;
        let size = size.parse().map_err(CasDigestParseError::InvalidSize)?;

        Ok(Self::new_sha1(sha1, size))
    }

    pub fn parse_digest_sha1_without_size(
        data: &[u8],
    ) -> Result<[u8; SHA1_SIZE], CasDigestParseError> {
        let mut sha1 = [0; SHA1_SIZE];
        hex::decode_to_slice(data, &mut sha1).map_err(CasDigestParseError::InvalidSha1)?;
        Ok(sha1)
    }

    /// Return the digest of an empty string
    pub fn empty_sha1() -> Self {
        Self::from_bytes_sha1(&[])
    }

    pub fn from_bytes_sha1(bytes: &[u8]) -> Self {
        let sha1 = Sha1::digest(bytes).into();
        Self::new_sha1(sha1, bytes.len() as u64)
    }
}

pub trait TrackedCasDigestKind: Sized + 'static {
    /// This needs to be a concrete implementation since we share the empty instance in a static
    /// but we can't have static generics.
    fn cell_for_empty_digest() -> Option<&'static OnceCell<TrackedCasDigest<Self>>>;
}

#[derive(Display)]
#[display(fmt = "{}", "hex::encode(&of.digest()[0..4])")]
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

impl<Kind> TrackedCasDigest<Kind> {
    pub fn new(data: CasDigest<Kind>) -> Self
    where
        Kind: TrackedCasDigestKind,
    {
        if data.size() == 0 {
            return Self::empty();
        }

        Self {
            inner: Arc::new(TrackedCasDigestInner {
                data,
                expires: AtomicI64::new(0),
            }),
        }
    }

    pub fn new_expires(data: CasDigest<Kind>, expiry: DateTime<Utc>) -> Self
    where
        Kind: TrackedCasDigestKind,
    {
        let res = Self::new(data);
        res.update_expires(expiry);
        res
    }

    pub fn empty() -> Self
    where
        Kind: TrackedCasDigestKind,
    {
        let make = || Self {
            inner: Arc::new(TrackedCasDigestInner {
                data: CasDigest::empty_sha1(),
                expires: AtomicI64::new(0),
            }),
        };

        if let Some(cell) = Kind::cell_for_empty_digest() {
            return cell.get_or_init(make).dupe();
        }

        make()
    }

    pub fn data(&self) -> &CasDigest<Kind> {
        &self.inner.data
    }

    pub fn digest(&self) -> &[u8] {
        self.inner.data.digest()
    }

    pub fn sha1(&self) -> &[u8; SHA1_SIZE] {
        self.inner.data.sha1()
    }

    pub fn size(&self) -> u64 {
        self.inner.data.size()
    }

    pub fn expires(&self) -> DateTime<Utc> {
        Utc.timestamp(self.inner.expires.load(Ordering::Relaxed), 0)
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
        assert_eq!(
            CasDigest::<()>::parse_digest_sha1(s).unwrap().to_string(),
            s
        );
    }
}
