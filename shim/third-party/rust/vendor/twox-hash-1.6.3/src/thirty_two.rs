use crate::UnalignedBuffer;
use core::{cmp, hash::Hasher};

#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};

const CHUNK_SIZE: usize = 16;

pub const PRIME_1: u32 = 2_654_435_761;
pub const PRIME_2: u32 = 2_246_822_519;
pub const PRIME_3: u32 = 3_266_489_917;
pub const PRIME_4: u32 = 668_265_263;
pub const PRIME_5: u32 = 374_761_393;

#[cfg_attr(feature = "serialize", derive(Deserialize, Serialize))]
#[derive(Copy, Clone, PartialEq)]
struct XxCore {
    v1: u32,
    v2: u32,
    v3: u32,
    v4: u32,
}

/// Calculates the 32-bit hash. Care should be taken when using this
/// hash.
///
/// Although this struct implements `Hasher`, it only calculates a
/// 32-bit number, leaving the upper bits as 0. This means it is
/// unlikely to be correct to use this in places like a `HashMap`.
#[cfg_attr(feature = "serialize", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct XxHash32 {
    total_len: u64,
    seed: u32,
    core: XxCore,
    #[cfg_attr(feature = "serialize", serde(flatten))]
    buffer: Buffer,
}

impl XxCore {
    fn with_seed(seed: u32) -> XxCore {
        XxCore {
            v1: seed.wrapping_add(PRIME_1).wrapping_add(PRIME_2),
            v2: seed.wrapping_add(PRIME_2),
            v3: seed,
            v4: seed.wrapping_sub(PRIME_1),
        }
    }

    #[inline(always)]
    fn ingest_chunks<I>(&mut self, values: I)
    where
        I: IntoIterator<Item = [u32; 4]>,
    {
        #[inline(always)]
        fn ingest_one_number(mut current_value: u32, mut value: u32) -> u32 {
            value = value.wrapping_mul(PRIME_2);
            current_value = current_value.wrapping_add(value);
            current_value = current_value.rotate_left(13);
            current_value.wrapping_mul(PRIME_1)
        }

        // By drawing these out, we can avoid going back and forth to
        // memory. It only really helps for large files, when we need
        // to iterate multiple times here.

        let mut v1 = self.v1;
        let mut v2 = self.v2;
        let mut v3 = self.v3;
        let mut v4 = self.v4;

        for [n1, n2, n3, n4] in values {
            v1 = ingest_one_number(v1, n1.to_le());
            v2 = ingest_one_number(v2, n2.to_le());
            v3 = ingest_one_number(v3, n3.to_le());
            v4 = ingest_one_number(v4, n4.to_le());
        }

        self.v1 = v1;
        self.v2 = v2;
        self.v3 = v3;
        self.v4 = v4;
    }

    #[inline(always)]
    fn finish(&self) -> u32 {
        // The original code pulls out local vars for v[1234]
        // here. Performance tests did not show that to be effective
        // here, presumably because this method is not called in a
        // tight loop.

        #[allow(unknown_lints, clippy::needless_late_init)] // keeping things parallel
        let mut hash;

        hash = self.v1.rotate_left(1);
        hash = hash.wrapping_add(self.v2.rotate_left(7));
        hash = hash.wrapping_add(self.v3.rotate_left(12));
        hash = hash.wrapping_add(self.v4.rotate_left(18));

        hash
    }
}

impl core::fmt::Debug for XxCore {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
        write!(
            f,
            "XxCore {{ {:016x} {:016x} {:016x} {:016x} }}",
            self.v1, self.v2, self.v3, self.v4
        )
    }
}

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Debug, Copy, Clone, Default, PartialEq)]
#[repr(align(4))]
#[cfg_attr(feature = "serialize", serde(transparent))]
struct AlignToU32<T>(T);

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Debug, Copy, Clone, Default, PartialEq)]
struct Buffer {
    #[cfg_attr(feature = "serialize", serde(rename = "buffer"))]
    data: AlignToU32<[u8; CHUNK_SIZE]>,
    #[cfg_attr(feature = "serialize", serde(rename = "buffer_usage"))]
    len: usize,
}

impl Buffer {
    fn data(&self) -> &[u8] {
        &self.data.0[..self.len]
    }

    /// Consumes as much of the parameter as it can, returning the unused part.
    fn consume<'a>(&mut self, data: &'a [u8]) -> &'a [u8] {
        let to_use = cmp::min(self.available(), data.len());
        let (data, remaining) = data.split_at(to_use);
        self.data.0[self.len..][..to_use].copy_from_slice(data);
        self.len += to_use;
        remaining
    }

    fn set_data(&mut self, data: &[u8]) {
        debug_assert!(self.is_empty());
        debug_assert!(data.len() < CHUNK_SIZE);
        self.data.0[..data.len()].copy_from_slice(data);
        self.len = data.len();
    }

    fn available(&self) -> usize {
        CHUNK_SIZE - self.len
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn is_full(&self) -> bool {
        self.len == CHUNK_SIZE
    }
}

impl XxHash32 {
    /// Constructs the hash with an initial seed
    pub fn with_seed(seed: u32) -> XxHash32 {
        XxHash32 {
            total_len: 0,
            seed,
            core: XxCore::with_seed(seed),
            buffer: Buffer::default(),
        }
    }

    pub(crate) fn write(&mut self, bytes: &[u8]) {
        let remaining = self.maybe_consume_bytes(bytes);
        if !remaining.is_empty() {
            let mut remaining = UnalignedBuffer::new(remaining);
            self.core.ingest_chunks(&mut remaining);
            self.buffer.set_data(remaining.remaining());
        }
        self.total_len += bytes.len() as u64;
    }

    // Consume bytes and try to make `self.buffer` empty.
    // If there are not enough bytes, `self.buffer` can be non-empty, and this
    // function returns an empty slice.
    fn maybe_consume_bytes<'a>(&mut self, data: &'a [u8]) -> &'a [u8] {
        if self.buffer.is_empty() {
            data
        } else {
            let data = self.buffer.consume(data);
            if self.buffer.is_full() {
                let mut u32s = UnalignedBuffer::new(self.buffer.data());
                self.core.ingest_chunks(&mut u32s);
                debug_assert!(u32s.remaining().is_empty());
                self.buffer.len = 0;
            }
            data
        }
    }

    pub(crate) fn finish(&self) -> u32 {
        let mut hash = if self.total_len >= CHUNK_SIZE as u64 {
            // We have processed at least one full chunk
            self.core.finish()
        } else {
            self.seed.wrapping_add(PRIME_5)
        };

        hash = hash.wrapping_add(self.total_len as u32);

        let mut buffered_u32s = UnalignedBuffer::<u32>::new(self.buffer.data());
        for buffered_u32 in &mut buffered_u32s {
            let k1 = buffered_u32.to_le().wrapping_mul(PRIME_3);
            hash = hash.wrapping_add(k1);
            hash = hash.rotate_left(17);
            hash = hash.wrapping_mul(PRIME_4);
        }

        let buffered_u8s = buffered_u32s.remaining();
        for &buffered_u8 in buffered_u8s {
            let k1 = u32::from(buffered_u8).wrapping_mul(PRIME_5);
            hash = hash.wrapping_add(k1);
            hash = hash.rotate_left(11);
            hash = hash.wrapping_mul(PRIME_1);
        }

        // The final intermixing
        hash ^= hash >> 15;
        hash = hash.wrapping_mul(PRIME_2);
        hash ^= hash >> 13;
        hash = hash.wrapping_mul(PRIME_3);
        hash ^= hash >> 16;

        hash
    }

    pub fn seed(&self) -> u32 {
        self.seed
    }

    /// Get the total number of bytes hashed, truncated to 32 bits.
    /// For the full 64-bit byte count, use `total_len_64`
    pub fn total_len(&self) -> u32 {
        self.total_len as u32
    }

    /// Get the total number of bytes hashed.
    pub fn total_len_64(&self) -> u64 {
        self.total_len
    }
}

impl Default for XxHash32 {
    fn default() -> XxHash32 {
        XxHash32::with_seed(0)
    }
}

impl Hasher for XxHash32 {
    fn finish(&self) -> u64 {
        u64::from(XxHash32::finish(self))
    }

    fn write(&mut self, bytes: &[u8]) {
        XxHash32::write(self, bytes)
    }
}

#[cfg(feature = "std")]
pub use crate::std_support::thirty_two::RandomXxHashBuilder32;

#[cfg(test)]
mod test {
    use super::{RandomXxHashBuilder32, XxHash32};
    use std::collections::HashMap;
    use std::hash::BuildHasherDefault;
    use std::prelude::v1::*;

    #[test]
    fn ingesting_byte_by_byte_is_equivalent_to_large_chunks() {
        let bytes: Vec<_> = (0..32).map(|_| 0).collect();

        let mut byte_by_byte = XxHash32::with_seed(0);
        for byte in bytes.chunks(1) {
            byte_by_byte.write(byte);
        }

        let mut one_chunk = XxHash32::with_seed(0);
        one_chunk.write(&bytes);

        assert_eq!(byte_by_byte.core, one_chunk.core);
    }

    #[test]
    fn hash_of_nothing_matches_c_implementation() {
        let mut hasher = XxHash32::with_seed(0);
        hasher.write(&[]);
        assert_eq!(hasher.finish(), 0x02cc_5d05);
    }

    #[test]
    fn hash_of_single_byte_matches_c_implementation() {
        let mut hasher = XxHash32::with_seed(0);
        hasher.write(&[42]);
        assert_eq!(hasher.finish(), 0xe0fe_705f);
    }

    #[test]
    fn hash_of_multiple_bytes_matches_c_implementation() {
        let mut hasher = XxHash32::with_seed(0);
        hasher.write(b"Hello, world!\0");
        assert_eq!(hasher.finish(), 0x9e5e_7e93);
    }

    #[test]
    fn hash_of_multiple_chunks_matches_c_implementation() {
        let bytes: Vec<_> = (0..100).collect();
        let mut hasher = XxHash32::with_seed(0);
        hasher.write(&bytes);
        assert_eq!(hasher.finish(), 0x7f89_ba44);
    }

    #[test]
    fn hash_with_different_seed_matches_c_implementation() {
        let mut hasher = XxHash32::with_seed(0x42c9_1977);
        hasher.write(&[]);
        assert_eq!(hasher.finish(), 0xd6bf_8459);
    }

    #[test]
    fn hash_with_different_seed_and_multiple_chunks_matches_c_implementation() {
        let bytes: Vec<_> = (0..100).collect();
        let mut hasher = XxHash32::with_seed(0x42c9_1977);
        hasher.write(&bytes);
        assert_eq!(hasher.finish(), 0x6d2f_6c17);
    }

    #[test]
    fn can_be_used_in_a_hashmap_with_a_default_seed() {
        let mut hash: HashMap<_, _, BuildHasherDefault<XxHash32>> = Default::default();
        hash.insert(42, "the answer");
        assert_eq!(hash.get(&42), Some(&"the answer"));
    }

    #[test]
    fn can_be_used_in_a_hashmap_with_a_random_seed() {
        let mut hash: HashMap<_, _, RandomXxHashBuilder32> = Default::default();
        hash.insert(42, "the answer");
        assert_eq!(hash.get(&42), Some(&"the answer"));
    }

    #[cfg(feature = "serialize")]
    type TestResult<T = ()> = Result<T, Box<dyn std::error::Error>>;

    #[cfg(feature = "serialize")]
    #[test]
    fn test_serialization_cycle() -> TestResult {
        let mut hasher = XxHash32::with_seed(0);
        hasher.write(b"Hello, world!\0");
        hasher.finish();

        let serialized = serde_json::to_string(&hasher)?;
        let unserialized: XxHash32 = serde_json::from_str(&serialized)?;
        assert_eq!(hasher, unserialized);
        Ok(())
    }

    #[cfg(feature = "serialize")]
    #[test]
    fn test_serialization_stability() -> TestResult {
        let mut hasher = XxHash32::with_seed(0);
        hasher.write(b"Hello, world!\0");
        hasher.finish();

        let serialized = r#"{
            "total_len": 14,
            "seed": 0,
            "core": {
              "v1": 606290984,
              "v2": 2246822519,
              "v3": 0,
              "v4": 1640531535
            },
            "buffer": [
              72,  101, 108, 108, 111, 44, 32, 119,
              111, 114, 108, 100, 33,  0,  0,  0
            ],
            "buffer_usage": 14
        }"#;

        let unserialized: XxHash32 = serde_json::from_str(serialized).unwrap();
        assert_eq!(hasher, unserialized);
        Ok(())
    }

    // This test validates wraparound/truncation behavior for very large inputs
    // of a 32-bit hash, but runs very slowly in the normal "cargo test"
    // build config since it hashes 4.3GB of data. It runs reasonably quick
    // under "cargo test --release".
    /*
    #[test]
    fn len_overflow_32bit() {
        // Hash 4.3 billion (4_300_000_000) bytes, which overflows a u32.
        let bytes200: Vec<u8> = (0..200).collect();
        let mut hasher = XxHash32::with_seed(0);
        for _ in 0..(4_300_000_000u64 / 200u64) {
            hasher.write(&bytes200);
        }
        assert_eq!(hasher.total_len_64(), 0x0000_0001_004c_cb00);
        assert_eq!(hasher.total_len(), 0x004c_cb00);
        // retult is tested against the C implementation
        assert_eq!(hasher.finish(), 0x1522_4ca7);
    }
    */
}
