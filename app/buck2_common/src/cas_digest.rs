/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Borrow;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Read;
use std::marker::PhantomData;
use std::sync::Arc;
use std::sync::atomic::AtomicI64;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use chrono::DateTime;
use chrono::TimeZone;
use chrono::Utc;
use derivative::Derivative;
use derive_more::Display;
use digest::Digest;
use dupe::Clone_;
use dupe::Copy_;
use dupe::Dupe;
use dupe::Dupe_;
use num_enum::TryFromPrimitive;
use once_cell::sync::Lazy;
use pagable::Pagable;
use sha1::Sha1;
use sha2::Sha256;

/// The number of bytes required by a SHA-1 hash
pub const SHA1_SIZE: usize = 20;

/// The number of bytes required by a SHA-256 hash
pub const SHA256_SIZE: usize = 32;

/// The number of bytes required by a Blake3 hash
pub const BLAKE3_SIZE: usize = 32;

/// The bytes that make up a file digest.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Allocative, Clone, Copy, Pagable)]
pub enum RawDigest {
    // TODO: Perhaps this should be represented as a (DigestAlgorithmKind, [0;32])
    Sha1([u8; SHA1_SIZE]),
    Sha256([u8; SHA256_SIZE]),
    Blake3([u8; BLAKE3_SIZE]),
    Blake3Keyed([u8; BLAKE3_SIZE]),
}

// We consider copying 20 bytes is cheap enough not to qualify for Dupe
impl Dupe for RawDigest {}

impl RawDigest {
    pub fn as_bytes(&self) -> &[u8] {
        match self {
            Self::Sha1(x) => x,
            Self::Sha256(x) => x,
            Self::Blake3(x) => x,
            Self::Blake3Keyed(x) => x,
        }
    }

    pub fn algorithm(&self) -> DigestAlgorithmFamily {
        match self {
            Self::Sha1(..) => DigestAlgorithmFamily::Sha1,
            Self::Sha256(..) => DigestAlgorithmFamily::Sha256,
            Self::Blake3(..) => DigestAlgorithmFamily::Blake3,
            Self::Blake3Keyed(..) => DigestAlgorithmFamily::Blake3Keyed,
        }
    }

    pub fn parse_sha1(data: &[u8]) -> Result<Self, CasDigestParseError> {
        let mut sha1 = [0; SHA1_SIZE];
        hex::decode_to_slice(data, &mut sha1).map_err(CasDigestParseError::InvalidSha1)?;
        Ok(RawDigest::Sha1(sha1))
    }

    pub fn parse_sha256(data: &[u8]) -> Result<Self, CasDigestParseError> {
        let mut sha256 = [0; SHA256_SIZE];
        hex::decode_to_slice(data, &mut sha256).map_err(CasDigestParseError::InvalidSha256)?;
        Ok(RawDigest::Sha256(sha256))
    }

    pub fn parse_blake3(data: &[u8]) -> Result<Self, CasDigestParseError> {
        let mut blake3 = [0; BLAKE3_SIZE];
        hex::decode_to_slice(data, &mut blake3).map_err(CasDigestParseError::InvalidBlake3)?;
        Ok(RawDigest::Blake3(blake3))
    }

    pub fn parse_blake3_keyed(data: &[u8]) -> Result<Self, CasDigestParseError> {
        let mut blake3 = [0; BLAKE3_SIZE];
        hex::decode_to_slice(data, &mut blake3).map_err(CasDigestParseError::InvalidBlake3)?;
        Ok(RawDigest::Blake3Keyed(blake3))
    }
}

impl fmt::Display for RawDigest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", hex::encode(self.as_bytes()))
    }
}

/// The family of digest algorithm associated with a digest. This tells you what kind of digest it
/// is, but it might not be sufficient in order to actually recreate the digest. For example, this
/// could contain keyed digests, but then you wouldn't have the key. We use this to store our
/// digest kind when it's informative-only, like in our materializer state on disk.
#[derive(
    Debug,
    Display,
    Eq,
    PartialEq,
    TryFromPrimitive,
    Copy,
    Clone,
    Dupe,
    Hash,
    Allocative
)]
#[repr(u8)]
pub enum DigestAlgorithmFamily {
    #[display("SHA1")]
    Sha1,
    #[display("SHA256")]
    Sha256,
    #[display("BLAKE3")]
    Blake3,
    #[display("BLAKE3-KEYED")]
    Blake3Keyed,
}

#[derive(buck2_error::Error, Debug)]
#[error("Invalid Digest algorithm: `{0}`")]
#[buck2(tag = Input)]
pub struct InvalidDigestAlgorithmFamily(String);

impl std::str::FromStr for DigestAlgorithmFamily {
    type Err = InvalidDigestAlgorithmFamily;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "SHA1" {
            return Ok(Self::Sha1);
        }

        if s == "SHA256" {
            return Ok(Self::Sha256);
        }

        if s == "BLAKE3" {
            return Ok(Self::Blake3);
        }

        if s == "BLAKE3-KEYED" {
            return Ok(Self::Blake3Keyed);
        }

        Err(InvalidDigestAlgorithmFamily(s.to_owned()))
    }
}

/// An actual digest algorithm you can use to hash data.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Dupe, Hash, Allocative)]
pub enum DigestAlgorithm {
    Sha1,
    Sha256,
    Blake3,
    Blake3Keyed { key: &'static [u8; 32] },
}

impl DigestAlgorithm {
    fn family(self) -> DigestAlgorithmFamily {
        match self {
            Self::Sha1 => DigestAlgorithmFamily::Sha1,
            Self::Sha256 => DigestAlgorithmFamily::Sha256,
            Self::Blake3 => DigestAlgorithmFamily::Blake3,
            Self::Blake3Keyed { .. } => DigestAlgorithmFamily::Blake3Keyed,
        }
    }
}

#[derive(Copy, Clone, Dupe, Debug, Allocative, Hash, Eq, PartialEq)]
pub struct CasDigestConfig {
    inner: &'static CasDigestConfigInner,
}

impl CasDigestConfig {
    pub fn testing_default() -> Self {
        static COMPAT: Lazy<CasDigestConfigInner> =
            Lazy::new(|| CasDigestConfigInner::new(vec![DigestAlgorithm::Sha1], None).unwrap());

        Self { inner: &COMPAT }
    }

    /// We just Box::leak this since we create one per daemon and as a result just use
    /// CasDigestConfig as a pointer.
    pub fn leak_new(
        algorithms: Vec<DigestAlgorithm>,
        preferred_source_algorithm: Option<DigestAlgorithm>,
    ) -> Result<Self, CasDigestConfigError> {
        let inner = Box::leak(Box::new(CasDigestConfigInner::new(
            algorithms,
            preferred_source_algorithm,
        )?));
        Ok(Self { inner })
    }

    /// Allow optimizing the empty file digest path, we do that by having the CasDigestConfig hold
    /// a cell for it (later in this stack).
    pub fn empty_file_digest(self) -> crate::file_ops::metadata::TrackedFileDigest {
        self.inner.empty_file_digest.dupe()
    }

    pub fn preferred_algorithm(self) -> DigestAlgorithm {
        self.inner.preferred_algorithm
    }

    pub fn digest160(self) -> Option<DigestAlgorithm> {
        self.inner.digest160
    }

    pub fn digest256(self) -> Option<DigestAlgorithm> {
        self.inner.digest256
    }

    pub fn allows_sha1(self) -> bool {
        self.inner.digest160 == Some(DigestAlgorithm::Sha1)
    }

    pub fn allows_sha256(self) -> bool {
        self.inner.digest256 == Some(DigestAlgorithm::Sha256)
    }

    pub fn allows_blake3(self) -> bool {
        self.inner.digest256 == Some(DigestAlgorithm::Blake3)
    }

    pub fn allows_blake3_keyed(self) -> bool {
        matches!(
            self.inner.digest256,
            Some(DigestAlgorithm::Blake3Keyed { .. })
        )
    }

    /// Access the config for source files. Note that there is no method to go back to the
    /// non-source config.
    pub fn source_files_config(self) -> Self {
        match &self.inner.source {
            SourceFilesConfig::UseSelf => self,
            SourceFilesConfig::UseThis(other) => Self { inner: other },
        }
    }
}

impl fmt::Display for CasDigestConfig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "CasDigestConfig(preferred = {}, source preferred = {})",
            self.preferred_algorithm().family(),
            self.source_files_config().preferred_algorithm().family()
        )
    }
}

#[derive(Debug, Allocative, Hash, Eq, PartialEq)]
struct CasDigestConfigInner {
    /// The algorithm we use for non-source files, action digests, etc.
    preferred_algorithm: DigestAlgorithm,
    digest160: Option<DigestAlgorithm>,
    digest256: Option<DigestAlgorithm>,
    empty_file_digest: crate::file_ops::metadata::TrackedFileDigest,
    /// A potentially different configuration to use when digesting source files.
    source: SourceFilesConfig,
}

#[derive(Debug, Allocative, Hash, Eq, PartialEq)]
enum SourceFilesConfig {
    UseSelf,
    UseThis(Box<CasDigestConfigInner>),
}

impl CasDigestConfigInner {
    /// Initialize a CasDigestConfigInner. The algorithms should be listed in decreasing order of
    /// preference.
    fn new(
        algorithms: Vec<DigestAlgorithm>,
        preferred_source_algorithm: Option<DigestAlgorithm>,
    ) -> Result<Self, CasDigestConfigError> {
        let preferred_algorithm = *algorithms
            .first()
            .ok_or(CasDigestConfigError::NotConfigured)?;

        let preferred_source_algorithm = preferred_source_algorithm
            .map(|a| {
                if algorithms.contains(&a) {
                    Ok(a)
                } else {
                    Err(CasDigestConfigError::InvalidPreferredSourceAlgorithm)
                }
            })
            .transpose()?;

        let mut digest160 = None;
        let mut digest256 = None;

        for algo in algorithms {
            let slot = match algo {
                DigestAlgorithm::Sha1 => &mut digest160,
                DigestAlgorithm::Sha256 => &mut digest256,
                DigestAlgorithm::Blake3 => &mut digest256,
                DigestAlgorithm::Blake3Keyed { .. } => &mut digest256,
            };

            if let Some(slot) = &slot {
                return Err(CasDigestConfigError::Conflict(*slot, algo));
            }

            *slot = Some(algo);
        }

        let empty_file_digest = TrackedCasDigest {
            inner: Arc::new(TrackedCasDigestInner {
                data: CasDigest::from_content_for_algorithm(&[], preferred_algorithm),
                expires: AtomicI64::new(0),
            }),
        };

        let source = match preferred_source_algorithm {
            Some(algo) => SourceFilesConfig::UseThis(Box::new(Self::new(vec![algo], None)?)),
            None => SourceFilesConfig::UseSelf,
        };

        Ok(Self {
            preferred_algorithm,
            digest160,
            digest256,
            empty_file_digest,
            source,
        })
    }
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
pub enum CasDigestConfigError {
    #[error("At least one algorithm must be enabled")]
    NotConfigured,
    #[error("The preferred source algorithm must be in the algorithms list")]
    InvalidPreferredSourceAlgorithm,
    #[error("Two algorithms were enabled for the same size: `{}` and `{}`", .0.family(), .1.family())]
    Conflict(DigestAlgorithm, DigestAlgorithm),
}

pub struct DataDigester {
    variant: DigesterVariant,
    size: u64,
}

pub struct Digester<Kind: CasDigestKind> {
    data: DataDigester,
    kind: PhantomData<Kind>,
}

enum DigesterVariant {
    Sha1(Sha1),
    Sha256(Sha256),
    Blake3(Box<blake3::Hasher>),      // This is unusually large.
    Blake3Keyed(Box<blake3::Hasher>), // Same as above
}

impl DataDigester {
    pub fn update(&mut self, data: &[u8]) {
        // Explicit dynamic dispatch because we need to match on which variant it was.
        match &mut self.variant {
            DigesterVariant::Sha1(h) => {
                h.update(data);
            }
            DigesterVariant::Sha256(h) => {
                h.update(data);
            }
            DigesterVariant::Blake3(h) => {
                h.update(data);
            }
            DigesterVariant::Blake3Keyed(h) => {
                h.update(data);
            }
        };

        self.size += data.len() as u64;
    }

    pub fn finalize(self) -> CasDigestData {
        match self.variant {
            DigesterVariant::Sha1(h) => CasDigestData::new_sha1(h.finalize().into(), self.size),
            DigesterVariant::Sha256(h) => CasDigestData::new_sha256(h.finalize().into(), self.size),
            DigesterVariant::Blake3(h) => CasDigestData::new_blake3(h.finalize().into(), self.size),
            DigesterVariant::Blake3Keyed(h) => {
                CasDigestData::new_blake3_keyed(h.finalize().into(), self.size)
            }
        }
    }

    pub fn algorithm(&self) -> DigestAlgorithmFamily {
        match &self.variant {
            DigesterVariant::Sha1(..) => DigestAlgorithmFamily::Sha1,
            DigesterVariant::Sha256(..) => DigestAlgorithmFamily::Sha256,
            DigesterVariant::Blake3(..) => DigestAlgorithmFamily::Blake3,
            DigesterVariant::Blake3Keyed(..) => DigestAlgorithmFamily::Blake3Keyed,
        }
    }

    pub fn bytes_read(&self) -> u64 {
        self.size
    }
}

impl<Kind: CasDigestKind> Digester<Kind> {
    pub fn update(&mut self, data: &[u8]) {
        self.data.update(data);
    }

    pub fn finalize(self) -> CasDigest<Kind> {
        CasDigest {
            data: self.data.finalize(),
            kind: PhantomData,
        }
    }

    pub fn algorithm(&self) -> DigestAlgorithmFamily {
        self.data.algorithm()
    }

    pub fn bytes_read(&self) -> u64 {
        self.data.bytes_read()
    }
}

/// Separate struct to allow us to use  `repr(transparent)` below and guarantee an identical
/// layout.
#[derive(
    Display, PartialEq, Eq, PartialOrd, Ord, Hash, Allocative, Clone, Dupe, Copy, Pagable
)]
#[display("{}:{}", digest, size)]
pub struct CasDigestData {
    size: u64,
    digest: RawDigest,
}

impl CasDigestData {
    fn new(digest: RawDigest, size: u64) -> Self {
        CasDigestData { size, digest }
    }

    pub fn new_sha1(sha1: [u8; SHA1_SIZE], size: u64) -> Self {
        Self::new(RawDigest::Sha1(sha1), size)
    }

    pub fn new_sha256(sha256: [u8; SHA256_SIZE], size: u64) -> Self {
        Self::new(RawDigest::Sha256(sha256), size)
    }

    pub fn new_blake3(blake3: [u8; BLAKE3_SIZE], size: u64) -> Self {
        Self::new(RawDigest::Blake3(blake3), size)
    }

    pub fn new_blake3_keyed(blake3: [u8; BLAKE3_SIZE], size: u64) -> Self {
        Self::new(RawDigest::Blake3Keyed(blake3), size)
    }

    pub fn digester(config: CasDigestConfig) -> DataDigester {
        Self::digester_for_algorithm(config.preferred_algorithm())
    }

    pub fn digester_for_algorithm(algorithm: DigestAlgorithm) -> DataDigester {
        let variant = match algorithm {
            DigestAlgorithm::Sha1 => DigesterVariant::Sha1(Sha1::new()),
            DigestAlgorithm::Sha256 => DigesterVariant::Sha256(Sha256::new()),
            DigestAlgorithm::Blake3 => DigesterVariant::Blake3(Box::new(blake3::Hasher::new())),
            DigestAlgorithm::Blake3Keyed { key } => {
                DigesterVariant::Blake3Keyed(Box::new(blake3::Hasher::new_keyed(key)))
            }
        };

        DataDigester { variant, size: 0 }
    }

    pub fn raw_digest(&self) -> &RawDigest {
        &self.digest
    }
}

#[derive(Display, Derivative, Allocative, Clone_, Dupe_, Copy_, Pagable)]
#[allocative(bound = "")]
#[derivative(PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display("{}", data)]
#[repr(transparent)]
pub struct CasDigest<Kind: CasDigestKind> {
    data: CasDigestData,
    #[derivative(
        Hash = "ignore",
        PartialEq = "ignore",
        PartialOrd = "ignore",
        Ord = "ignore"
    )]
    kind: PhantomData<Kind>,
}

impl<Kind: CasDigestKind> fmt::Debug for CasDigest<Kind> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl<Kind: CasDigestKind> CasDigest<Kind> {
    pub fn from_digest_bytes(
        kind: DigestAlgorithmFamily,
        digest: &[u8],
        size: u64,
    ) -> buck2_error::Result<Self> {
        Ok(match kind {
            DigestAlgorithmFamily::Sha1 => Self::new_sha1(digest.try_into()?, size),
            DigestAlgorithmFamily::Sha256 => Self::new_sha256(digest.try_into()?, size),
            DigestAlgorithmFamily::Blake3 => Self::new_blake3(digest.try_into()?, size),
            DigestAlgorithmFamily::Blake3Keyed => Self::new_blake3_keyed(digest.try_into()?, size),
        })
    }

    pub fn new(digest: RawDigest, size: u64) -> Self {
        Self {
            data: CasDigestData { size, digest },
            kind: PhantomData,
        }
    }

    pub fn new_sha1(sha1: [u8; SHA1_SIZE], size: u64) -> Self {
        Self::new(RawDigest::Sha1(sha1), size)
    }

    pub fn new_sha256(sha256: [u8; SHA256_SIZE], size: u64) -> Self {
        Self::new(RawDigest::Sha256(sha256), size)
    }

    pub fn new_blake3(blake3: [u8; BLAKE3_SIZE], size: u64) -> Self {
        Self::new(RawDigest::Blake3(blake3), size)
    }

    pub fn new_blake3_keyed(blake3: [u8; BLAKE3_SIZE], size: u64) -> Self {
        Self::new(RawDigest::Blake3Keyed(blake3), size)
    }

    pub fn raw_digest(&self) -> &RawDigest {
        self.data.raw_digest()
    }

    pub fn size(&self) -> u64 {
        self.data.size
    }

    /// A tiny representation of this digest, useful for logging when the full sha1 presentation is
    /// too expensive.
    pub fn tiny_digest(&self) -> TinyDigest<'_, Kind> {
        TinyDigest { of: self }
    }

    pub fn parse_digest(
        s: &str,
        config: CasDigestConfig,
    ) -> Result<(Self, DigestAlgorithm), CasDigestParseError> {
        let (digest, size) = s
            .split_once(':')
            .ok_or(CasDigestParseError::MissingSizeSeparator)?;

        let (digest, algo) = CasDigest::<Kind>::parse_digest_without_size(digest, config)?;
        let size = size.parse().map_err(CasDigestParseError::InvalidSize)?;

        Ok((Self::new(digest, size), algo))
    }

    pub fn parse_digest_without_size(
        data: &str,
        config: CasDigestConfig,
    ) -> Result<(RawDigest, DigestAlgorithm), CasDigestParseError> {
        let algo = if data.len() == 40 {
            config.digest160()
        } else if data.len() == 64 {
            config.digest256()
        } else {
            None
        };

        let algo = algo.ok_or(CasDigestParseError::UnsupportedDigest(data.len()))?;

        let digest = match algo {
            DigestAlgorithm::Sha1 => RawDigest::parse_sha1(data.as_bytes()),
            DigestAlgorithm::Sha256 => RawDigest::parse_sha256(data.as_bytes()),
            DigestAlgorithm::Blake3 => RawDigest::parse_blake3(data.as_bytes()),
            DigestAlgorithm::Blake3Keyed { .. } => RawDigest::parse_blake3_keyed(data.as_bytes()),
        }?;

        Ok((digest, algo))
    }

    /// Return the digest of an empty string
    pub fn empty(config: CasDigestConfig) -> Self {
        Self::from_content(&[], config)
    }

    pub fn digester(config: CasDigestConfig) -> Digester<Kind> {
        Self::digester_for_algorithm(config.preferred_algorithm())
    }

    pub fn digester_for_algorithm(algorithm: DigestAlgorithm) -> Digester<Kind> {
        Digester {
            data: CasDigestData::digester_for_algorithm(algorithm),
            kind: PhantomData,
        }
    }

    pub fn from_content(bytes: &[u8], config: CasDigestConfig) -> Self {
        Self::from_content_for_algorithm(bytes, config.preferred_algorithm())
    }

    pub fn from_content_for_algorithm(bytes: &[u8], algorithm: DigestAlgorithm) -> Self {
        let mut digester = Self::digester_for_algorithm(algorithm);
        digester.update(bytes);
        digester.finalize()
    }

    pub fn from_reader<R: Read>(reader: R, config: CasDigestConfig) -> buck2_error::Result<Self> {
        Self::from_reader_for_algorithm(reader, config.preferred_algorithm())
    }

    pub fn from_reader_for_algorithm<R: Read>(
        mut reader: R,
        algorithm: DigestAlgorithm,
    ) -> buck2_error::Result<Self> {
        let mut digester = Self::digester_for_algorithm(algorithm);

        // Buffer size chosen based on benchmarks at D26176645
        // Also optimal for Blake3's SIMD implementation.
        let mut buffer = [0; 16 * 1024];
        loop {
            let count = reader.read(&mut buffer)?;
            if count == 0 {
                break;
            }
            digester.update(&buffer[..count]);
        }

        Ok(digester.finalize())
    }

    pub fn coerce<NewKind: CasDigestKind>(self) -> CasDigest<NewKind> {
        CasDigest {
            data: self.data,
            kind: PhantomData,
        }
    }
}

pub trait CasDigestKind: Sized + 'static {
    /// This needs to be a concrete implementation since we share the empty instance in a static
    /// but we can't have static generics.
    fn empty_digest(config: CasDigestConfig) -> Option<TrackedCasDigest<Self>>;
}

#[derive(Display)]
#[display("{}", hex::encode(&of.raw_digest().as_bytes()[0..4]))]
pub struct TinyDigest<'a, Kind: CasDigestKind> {
    of: &'a CasDigest<Kind>,
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = InvalidDigest)]
pub enum CasDigestParseError {
    #[error("The digest is missing a size separator, it should look like `HASH:SIZE`")]
    MissingSizeSeparator,

    #[error("The digest part of the CAS digest is not a valid SHA1 digest")]
    InvalidSha1(#[source] hex::FromHexError),

    #[error("The digest part of the CAS digest is not a valid SHA256 digest")]
    InvalidSha256(#[source] hex::FromHexError),

    #[error("The digest part of the CAS digest is not a valid BLAKE3 hash")]
    InvalidBlake3(#[source] hex::FromHexError),

    #[error("The digest size ({} chars) does not correspond to a supported digest size", .0)]
    UnsupportedDigest(usize),

    #[error("The size part of the CAS digest is invalid")]
    InvalidSize(#[source] std::num::ParseIntError),
}

/// A digest to interact with RE. This, despite the name, can be a file or a directory. We track
/// the sha1 and the size of the underlying blob. We *also* keep track of its expiry in the CAS.
/// Note that for directory, the expiry represents that of the directory's blob, not its underlying
/// contents.
#[derive(Allocative, Debug, Pagable)]
#[allocative(bound = "")]
struct TrackedCasDigestInner<Kind: CasDigestKind> {
    data: CasDigest<Kind>,
    expires: AtomicI64,
}

#[derive(Display, Dupe_, Allocative, Pagable)]
#[allocative(bound = "")]
#[display("{}", self.data())]
pub struct TrackedCasDigest<Kind: CasDigestKind> {
    inner: Arc<TrackedCasDigestInner<Kind>>,
}

impl<Kind: CasDigestKind> Clone for TrackedCasDigest<Kind> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.dupe(),
        }
    }
}

impl<Kind: CasDigestKind> Borrow<CasDigest<Kind>> for TrackedCasDigest<Kind> {
    fn borrow(&self) -> &CasDigest<Kind> {
        self.data()
    }
}

impl<Kind: CasDigestKind> Borrow<CasDigest<Kind>> for &TrackedCasDigest<Kind> {
    fn borrow(&self) -> &CasDigest<Kind> {
        self.data()
    }
}

impl<Kind: CasDigestKind> PartialOrd for TrackedCasDigest<Kind> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.data().partial_cmp(other.data())
    }
}

impl<Kind: CasDigestKind> Ord for TrackedCasDigest<Kind> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.data().cmp(other.data())
    }
}

impl<Kind: CasDigestKind> PartialEq for TrackedCasDigest<Kind> {
    fn eq(&self, other: &Self) -> bool {
        self.data().eq(other.data())
    }
}

impl<Kind: CasDigestKind> Eq for TrackedCasDigest<Kind> {}

impl<Kind: CasDigestKind> Hash for TrackedCasDigest<Kind> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data().hash(state)
    }
}

impl<Kind: CasDigestKind> fmt::Debug for TrackedCasDigest<Kind> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{} expires at {}]",
            self,
            self.inner.expires.load(Ordering::Relaxed)
        )
    }
}

impl<Kind: CasDigestKind> buck2_core::directory_digest::DirectoryDigest for TrackedCasDigest<Kind> {}

impl<Kind: CasDigestKind> TrackedCasDigest<Kind> {
    pub fn new(data: CasDigest<Kind>, config: CasDigestConfig) -> Self
    where
        Kind: CasDigestKind,
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
        Kind: CasDigestKind,
    {
        let res = Self::new(data, config);
        res.update_expires(expiry);
        res
    }

    pub fn empty(config: CasDigestConfig) -> Self
    where
        Kind: CasDigestKind,
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
        Kind: CasDigestKind,
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

    pub fn raw_digest(&self) -> &RawDigest {
        self.inner.data.raw_digest()
    }

    pub fn size(&self) -> u64 {
        self.inner.data.size()
    }

    pub fn expires(&self) -> buck2_error::Result<DateTime<Utc>> {
        match Utc.timestamp_opt(self.inner.expires.load(Ordering::Relaxed), 0) {
            chrono::MappedLocalTime::Single(t) => Ok(t),
            chrono::MappedLocalTime::None => Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Environment,
                "CAS Digest expiration is an invalid local time"
            )),
            chrono::MappedLocalTime::Ambiguous(t1, t2) => Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Environment,
                "Cas Digest expiration is ambiguous, ranging from {:?} to {:?}",
                t1,
                t2
            )),
        }
    }

    pub fn update_expires(&self, time: DateTime<Utc>) {
        self.inner
            .expires
            .store(time.timestamp(), Ordering::Relaxed)
    }
}

pub mod testing {
    use super::*;

    pub fn sha1_sha256() -> CasDigestConfig {
        static CONFIG: Lazy<CasDigestConfigInner> = Lazy::new(|| {
            CasDigestConfigInner::new(vec![DigestAlgorithm::Sha1, DigestAlgorithm::Sha256], None)
                .unwrap()
        });
        CasDigestConfig { inner: &CONFIG }
    }

    pub fn sha1_blake3() -> CasDigestConfig {
        static CONFIG: Lazy<CasDigestConfigInner> = Lazy::new(|| {
            CasDigestConfigInner::new(vec![DigestAlgorithm::Sha1, DigestAlgorithm::Blake3], None)
                .unwrap()
        });
        CasDigestConfig { inner: &CONFIG }
    }

    pub fn sha256_sha1() -> CasDigestConfig {
        static CONFIG: Lazy<CasDigestConfigInner> = Lazy::new(|| {
            CasDigestConfigInner::new(vec![DigestAlgorithm::Sha256, DigestAlgorithm::Sha1], None)
                .unwrap()
        });
        CasDigestConfig { inner: &CONFIG }
    }

    pub fn sha1() -> CasDigestConfig {
        static CONFIG: Lazy<CasDigestConfigInner> =
            Lazy::new(|| CasDigestConfigInner::new(vec![DigestAlgorithm::Sha1], None).unwrap());
        CasDigestConfig { inner: &CONFIG }
    }

    pub fn sha256() -> CasDigestConfig {
        static CONFIG: Lazy<CasDigestConfigInner> =
            Lazy::new(|| CasDigestConfigInner::new(vec![DigestAlgorithm::Sha256], None).unwrap());
        CasDigestConfig { inner: &CONFIG }
    }

    pub fn blake3() -> CasDigestConfig {
        static CONFIG: Lazy<CasDigestConfigInner> =
            Lazy::new(|| CasDigestConfigInner::new(vec![DigestAlgorithm::Blake3], None).unwrap());
        CasDigestConfig { inner: &CONFIG }
    }

    pub fn blake3_keyed() -> CasDigestConfig {
        static CONFIG: Lazy<CasDigestConfigInner> = Lazy::new(|| {
            CasDigestConfigInner::new(vec![DigestAlgorithm::Blake3Keyed { key: &[0; 32] }], None)
                .unwrap()
        });
        CasDigestConfig { inner: &CONFIG }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_ops::metadata::FileDigestKind;

    #[test]
    fn test_digest_from_str() {
        let s = "0000000000000000000000000000000000000000:123";
        let config = CasDigestConfig::testing_default();
        assert_eq!(
            CasDigest::<FileDigestKind>::parse_digest(s, config)
                .unwrap()
                .0
                .to_string(),
            s
        );
    }

    #[test]
    fn test_digest_from_reader() {
        let content = &b"foo"[..];

        assert_eq!(
            CasDigest::<FileDigestKind>::from_reader_for_algorithm(content, DigestAlgorithm::Sha1)
                .unwrap()
                .to_string(),
            "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33:3"
        );
        assert_eq!(
            CasDigest::<FileDigestKind>::from_reader_for_algorithm(
                content,
                DigestAlgorithm::Sha256
            )
            .unwrap()
            .to_string(),
            "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae:3"
        );
        assert_eq!(
            CasDigest::<FileDigestKind>::from_reader_for_algorithm(
                content,
                DigestAlgorithm::Blake3
            )
            .unwrap()
            .to_string(),
            "04e0bb39f30b1a3feb89f536c93be15055482df748674b00d26e5a75777702e9:3"
        );
    }

    #[test]
    fn test_digest_from_content() {
        let content = &b"foo"[..];

        assert_eq!(
            CasDigest::<FileDigestKind>::from_content_for_algorithm(content, DigestAlgorithm::Sha1)
                .to_string(),
            "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33:3"
        );
        assert_eq!(
            CasDigest::<FileDigestKind>::from_content_for_algorithm(
                content,
                DigestAlgorithm::Sha256
            )
            .to_string(),
            "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae:3"
        );
        assert_eq!(
            CasDigest::<FileDigestKind>::from_content_for_algorithm(
                content,
                DigestAlgorithm::Blake3
            )
            .to_string(),
            "04e0bb39f30b1a3feb89f536c93be15055482df748674b00d26e5a75777702e9:3"
        );
    }

    #[test]
    fn test_preferred_algorithm() {
        let content = &b"foo"[..];

        assert_eq!(
            CasDigest::<FileDigestKind>::from_content(content, testing::sha1(),).to_string(),
            "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33:3"
        );
        assert_eq!(
            CasDigest::<FileDigestKind>::from_content(content, testing::sha256(),).to_string(),
            "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae:3"
        );
        assert_eq!(
            CasDigest::<FileDigestKind>::from_content(content, testing::blake3(),).to_string(),
            "04e0bb39f30b1a3feb89f536c93be15055482df748674b00d26e5a75777702e9:3"
        );
    }

    #[test]
    fn test_parse_digest() {
        let sha1 = CasDigest::<FileDigestKind>::parse_digest(
            "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33:3",
            testing::sha256_sha1(),
        )
        .unwrap()
        .0;
        assert_eq!(sha1.raw_digest().algorithm(), DigestAlgorithmFamily::Sha1);
        assert_eq!(
            sha1.to_string(),
            "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33:3"
        );

        let sha256 = CasDigest::<FileDigestKind>::parse_digest(
            "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae:3",
            testing::sha1_sha256(),
        )
        .unwrap()
        .0;
        assert_eq!(
            sha256.raw_digest().algorithm(),
            DigestAlgorithmFamily::Sha256
        );
        assert_eq!(
            sha256.to_string(),
            "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae:3"
        );

        let blake3 = CasDigest::<FileDigestKind>::parse_digest(
            "04e0bb39f30b1a3feb89f536c93be15055482df748674b00d26e5a75777702e9:3",
            testing::sha1_blake3(),
        )
        .unwrap()
        .0;
        assert_eq!(
            blake3.raw_digest().algorithm(),
            DigestAlgorithmFamily::Blake3
        );
        assert_eq!(
            blake3.to_string(),
            "04e0bb39f30b1a3feb89f536c93be15055482df748674b00d26e5a75777702e9:3"
        );

        let blake3_keyed = CasDigest::<FileDigestKind>::parse_digest(
            "04e0bb39f30b1a3feb89f536c93be15055482df748674b00d26e5a75777702e9:3",
            testing::blake3_keyed(),
        )
        .unwrap()
        .0;
        assert_eq!(
            blake3_keyed.raw_digest().algorithm(),
            DigestAlgorithmFamily::Blake3Keyed
        );
        assert_eq!(
            blake3_keyed.to_string(),
            "04e0bb39f30b1a3feb89f536c93be15055482df748674b00d26e5a75777702e9:3"
        );
    }

    #[test]
    fn test_digest_algorithm_kind_roundtrip() {
        for v in [
            DigestAlgorithmFamily::Sha1,
            DigestAlgorithmFamily::Sha256,
            DigestAlgorithmFamily::Blake3,
            DigestAlgorithmFamily::Blake3Keyed,
        ] {
            assert_eq!(v, v.to_string().parse().unwrap());
        }
    }
}
