/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Read and write Clang's version 1 header-map format.
//!
//! Paths are stored as UTF-8, without normalization or Unicode case folding.
//! Callers choose mapping order and resolve duplicate keys before encoding.
//! Compatibility formats preserve the output of the existing clangd converter
//! and LLVM's Python hmaptool, including their non-ASCII hashing behavior.

use std::collections::HashMap;
use std::io::Write;
use std::str::Utf8Error;

use thiserror::Error;

mod compat;

const MAGIC: u32 = u32::from_be_bytes(*b"hmap");
const HEADER_SIZE: usize = 24;
const BUCKET_SIZE: usize = 12;

/// The byte order of the file. Clang can read either order.
#[derive(Clone, Copy, Debug)]
pub enum ByteOrder {
    Little,
    Big,
}

impl ByteOrder {
    /// Returns the byte order of the machine running this code.
    pub const fn native() -> Self {
        if cfg!(target_endian = "little") {
            Self::Little
        } else {
            Self::Big
        }
    }

    fn u16(self, value: u16) -> [u8; 2] {
        match self {
            Self::Little => value.to_le_bytes(),
            Self::Big => value.to_be_bytes(),
        }
    }

    fn u32(self, value: u32) -> [u8; 4] {
        match self {
            Self::Little => value.to_le_bytes(),
            Self::Big => value.to_be_bytes(),
        }
    }

    fn read_u16(self, bytes: [u8; 2]) -> u16 {
        match self {
            Self::Little => u16::from_le_bytes(bytes),
            Self::Big => u16::from_be_bytes(bytes),
        }
    }

    fn read_u32(self, bytes: [u8; 4]) -> u32 {
        match self {
            Self::Little => u32::from_le_bytes(bytes),
            Self::Big => u32::from_be_bytes(bytes),
        }
    }
}

/// The signedness of plain C char in the compiler that reads the header map.
/// This is a property of the Clang executable, not its compilation target.
#[derive(Clone, Copy, Debug)]
pub enum CharSignedness {
    Signed,
    Unsigned,
}

impl CharSignedness {
    /// Matches C char on the machine running this code.
    pub const fn native() -> Self {
        if std::ffi::c_char::MIN == 0 {
            Self::Unsigned
        } else {
            Self::Signed
        }
    }
}

/// Selects hashing, string-table layout, and path splitting.
#[derive(Clone, Copy, Debug)]
pub enum WriteFormat {
    /// Uses byte offsets and Clang's ASCII-only byte hashing.
    Clang(CharSignedness),
    /// Preserves the clangd converter's character hash and undeduplicated strings.
    LegacyClangd,
    /// Preserves Python hmaptool's Unicode hash and character-counted offsets.
    /// Non-ASCII output can be invalid for Clang and may not round trip.
    PythonHmaptool,
}

impl Default for WriteFormat {
    fn default() -> Self {
        Self::Clang(CharSignedness::native())
    }
}

/// A header map could not be encoded or decoded.
#[derive(Debug, Error, PartialEq, Eq)]
pub enum HeaderMapError {
    #[error("Invalid header map file")]
    InvalidFile,
    #[error("Unexpected magic value found {0}")]
    UnexpectedMagic(u32),
    #[error("Unsupported version {0}")]
    UnsupportedVersion(u16),
    #[error("Unexpected reserved value found {0}")]
    UnexpectedReserved(u16),
    #[error("Invalid number of buckets found {0}")]
    InvalidNumberOfBuckets(u32),
    #[error("Header map entry count does not match its buckets")]
    InvalidEntryCount,
    #[error("Invalid header map string offset {0}")]
    InvalidStringOffset(u32),
    #[error("Header map string at offset {0} is not NUL-terminated")]
    UnterminatedString(u32),
    #[error("Header map string is not UTF-8: {0}")]
    InvalidUtf8(#[from] Utf8Error),
    #[error("Header map exceeds the 32-bit format limit")]
    TooLarge,
    #[error("Header map strings must not contain NUL bytes")]
    NulByte,
    #[error("Lowercasing {0:?} produced multiple characters")]
    InvalidLowercase(char),
}

/// A header map with owned buckets and strings, independent of file I/O.
#[derive(Debug, PartialEq, Eq)]
pub struct HeaderMap {
    buckets: Vec<[u32; 3]>,
    strings: Vec<u8>,
    num_entries: u32,
    max_value_length: u32,
}

impl HeaderMap {
    /// Encodes owned or borrowed mappings in iteration order. Owned entries
    /// are released as they are encoded. Does not add destination self-maps.
    ///
    /// Duplicate keys remain separate buckets; Clang finds the first matching
    /// bucket. Use a sorted map for deterministic output and resolve conflicts
    /// in the caller when a different policy is needed.
    pub fn from_mappings<K: AsRef<str>, V: AsRef<str>>(
        mappings: impl ExactSizeIterator<Item = (K, V)>,
        format: WriteFormat,
    ) -> Result<Self, HeaderMapError> {
        let num_entries = u32::try_from(mappings.len()).map_err(|_| HeaderMapError::TooLarge)?;
        let num_buckets = num_entries
            .checked_mul(3)
            .and_then(u32::checked_next_power_of_two)
            .ok_or(HeaderMapError::TooLarge)?;
        table_end(num_buckets)?;
        let mut buckets = vec![[0; 3]; num_buckets as usize];
        let mut strings = StringTable::new(format);
        let mut max_value_length = 0;
        let mut inserted = 0;
        for (key, value) in mappings {
            let key = key.as_ref();
            let value = value.as_ref();
            if inserted == num_entries {
                return Err(HeaderMapError::InvalidEntryCount);
            }
            let value_length = match format {
                WriteFormat::PythonHmaptool => value.chars().count(),
                _ => value.len(),
            };
            max_value_length = max_value_length
                .max(u32::try_from(value_length).map_err(|_| HeaderMapError::TooLarge)?);
            let (prefix, suffix) = match format {
                WriteFormat::Clang(_) => value.split_at(value.rfind('/').map_or(0, |i| i + 1)),
                WriteFormat::LegacyClangd => compat::split_legacy_path(value),
                WriteFormat::PythonHmaptool => compat::split_hmaptool_path(value),
            };
            let bucket = [
                strings.add(key)?,
                strings.add(prefix)?,
                strings.add(suffix)?,
            ];
            let mut index = hash_key(key, format)? & (num_buckets - 1);
            while buckets[index as usize][0] != 0 {
                index = (index + 1) & (num_buckets - 1);
            }
            buckets[index as usize] = bucket;
            inserted += 1;
        }
        if inserted != num_entries {
            return Err(HeaderMapError::InvalidEntryCount);
        }
        u32::try_from(strings.bytes.len()).map_err(|_| HeaderMapError::TooLarge)?;
        Ok(Self {
            buckets,
            strings: strings.bytes,
            num_entries,
            max_value_length,
        })
    }

    /// Reads either byte order and checks bucket and string bounds.
    /// Malformed files return errors instead of panicking.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, HeaderMapError> {
        if bytes.len() < HEADER_SIZE {
            return Err(HeaderMapError::InvalidFile);
        }
        let byte_order = match &bytes[..4] {
            b"pamh" => ByteOrder::Little,
            b"hmap" => ByteOrder::Big,
            _ => {
                return Err(HeaderMapError::UnexpectedMagic(u32::from_be_bytes(
                    bytes[..4].try_into().expect("magic has four bytes"),
                )));
            }
        };
        let u16_at = |offset| {
            byte_order.read_u16(
                bytes[offset..offset + 2]
                    .try_into()
                    .expect("header is complete"),
            )
        };
        let u32_at = |offset| {
            byte_order.read_u32(
                bytes[offset..offset + 4]
                    .try_into()
                    .expect("header is complete"),
            )
        };
        let version = u16_at(4);
        if version != 1 {
            return Err(HeaderMapError::UnsupportedVersion(version));
        }
        let reserved = u16_at(6);
        if reserved != 0 {
            return Err(HeaderMapError::UnexpectedReserved(reserved));
        }
        let strings_offset = u32_at(8) as usize;
        let num_entries = u32_at(12);
        let num_buckets = u32_at(16);
        if !num_buckets.is_power_of_two() {
            return Err(HeaderMapError::InvalidNumberOfBuckets(num_buckets));
        }
        let buckets_end = table_end(num_buckets)? as usize;
        if buckets_end > bytes.len()
            || strings_offset < buckets_end
            || strings_offset >= bytes.len()
        {
            return Err(HeaderMapError::InvalidFile);
        }
        let buckets = bytes[HEADER_SIZE..buckets_end]
            .as_chunks::<BUCKET_SIZE>()
            .0
            .iter()
            .map(|bucket| {
                std::array::from_fn(|i| {
                    byte_order.read_u32(
                        bucket[i * 4..i * 4 + 4]
                            .try_into()
                            .expect("bucket is complete"),
                    )
                })
            })
            .collect::<Vec<[u32; 3]>>();
        let occupied = buckets.iter().filter(|bucket| bucket[0] != 0).count();
        // Clang stops probing at an empty bucket, so a full table can hang it.
        if occupied != num_entries as usize || occupied == buckets.len() {
            return Err(HeaderMapError::InvalidEntryCount);
        }
        let result = Self {
            buckets,
            strings: bytes[strings_offset..].to_vec(),
            num_entries,
            max_value_length: u32_at(20),
        };
        for bucket in result.buckets.iter().filter(|bucket| bucket[0] != 0) {
            for offset in bucket {
                result.string_at(*offset)?;
            }
        }
        Ok(result)
    }

    /// Returns the mappings in bucket order. Compatibility output with invalid
    /// UTF-8 offsets returns an error.
    pub fn mappings(&self) -> Result<Vec<(String, String)>, HeaderMapError> {
        self.buckets
            .iter()
            .filter(|bucket| bucket[0] != 0)
            .map(|bucket| {
                Ok((
                    self.string_at(bucket[0])?.to_owned(),
                    [self.string_at(bucket[1])?, self.string_at(bucket[2])?].concat(),
                ))
            })
            .collect()
    }

    /// Writes the header map in the selected byte order. Does not flush the writer.
    pub fn write_to(&self, writer: &mut impl Write, byte_order: ByteOrder) -> std::io::Result<()> {
        writer.write_all(&byte_order.u32(MAGIC))?;
        writer.write_all(&byte_order.u16(1))?;
        writer.write_all(&byte_order.u16(0))?;
        let strings_offset = HEADER_SIZE as u32 + self.buckets.len() as u32 * BUCKET_SIZE as u32;
        for value in [
            strings_offset,
            self.num_entries,
            self.buckets.len() as u32,
            self.max_value_length,
        ] {
            writer.write_all(&byte_order.u32(value))?;
        }
        for bucket in &self.buckets {
            for value in bucket {
                writer.write_all(&byte_order.u32(*value))?;
            }
        }
        writer.write_all(&self.strings)
    }

    /// Returns the encoded file without using the filesystem.
    pub fn to_bytes(&self, byte_order: ByteOrder) -> Vec<u8> {
        let mut bytes = Vec::new();
        self.write_to(&mut bytes, byte_order)
            .expect("writing to a Vec cannot fail");
        bytes
    }

    fn string_at(&self, offset: u32) -> Result<&str, HeaderMapError> {
        let bytes = self
            .strings
            .get(offset as usize..)
            .ok_or(HeaderMapError::InvalidStringOffset(offset))?;
        let length = bytes
            .iter()
            .position(|byte| *byte == 0)
            .ok_or(HeaderMapError::UnterminatedString(offset))?;
        Ok(std::str::from_utf8(&bytes[..length])?)
    }
}

fn table_end(num_buckets: u32) -> Result<u32, HeaderMapError> {
    num_buckets
        .checked_mul(BUCKET_SIZE as u32)
        .and_then(|size| size.checked_add(HEADER_SIZE as u32))
        .ok_or(HeaderMapError::TooLarge)
}

fn hash_key(key: &str, format: WriteFormat) -> Result<u32, HeaderMapError> {
    match format {
        WriteFormat::Clang(signedness) => Ok(key.bytes().fold(0u32, |hash, byte| {
            let byte = byte.to_ascii_lowercase();
            let value = match signedness {
                CharSignedness::Signed => i32::from(byte as i8) as u32,
                CharSignedness::Unsigned => u32::from(byte),
            };
            hash.wrapping_add(value.wrapping_mul(13))
        })),
        WriteFormat::LegacyClangd => Ok(key.chars().fold(0u32, |hash, ch| {
            hash.wrapping_add((ch.to_ascii_lowercase() as u32).wrapping_mul(13))
        })),
        WriteFormat::PythonHmaptool => key.chars().try_fold(0u32, |hash, ch| {
            let mut lowercase = ch.to_lowercase();
            let lowered = lowercase
                .next()
                .expect("lowercase has at least one character");
            if lowercase.next().is_some() {
                return Err(HeaderMapError::InvalidLowercase(ch));
            }
            Ok(hash.wrapping_add((lowered as u32).wrapping_mul(13)))
        }),
    }
}

struct StringTable {
    bytes: Vec<u8>,
    offsets: HashMap<String, u32>,
    character_length: usize,
    format: WriteFormat,
}

impl StringTable {
    fn new(format: WriteFormat) -> Self {
        Self {
            bytes: vec![0],
            offsets: HashMap::new(),
            character_length: 1,
            format,
        }
    }

    fn add(&mut self, value: &str) -> Result<u32, HeaderMapError> {
        if value.contains('\0') {
            return Err(HeaderMapError::NulByte);
        }
        if !matches!(self.format, WriteFormat::LegacyClangd)
            && let Some(offset) = self.offsets.get(value)
        {
            return Ok(*offset);
        }
        let length = match self.format {
            WriteFormat::PythonHmaptool => self.character_length,
            _ => self.bytes.len(),
        };
        let offset = u32::try_from(length).map_err(|_| HeaderMapError::TooLarge)?;
        self.bytes.extend_from_slice(value.as_bytes());
        self.bytes.push(0);
        if !matches!(self.format, WriteFormat::LegacyClangd) {
            if matches!(self.format, WriteFormat::PythonHmaptool) {
                self.character_length = self
                    .character_length
                    .checked_add(value.chars().count())
                    .and_then(|length| length.checked_add(1))
                    .ok_or(HeaderMapError::TooLarge)?;
            }
            self.offsets.insert(value.to_owned(), offset);
        }
        Ok(offset)
    }
}

#[cfg(test)]
mod tests;
