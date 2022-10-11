use crate::UnalignedBuffer;
use core::{cmp, hash::Hasher};

#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};

const CHUNK_SIZE: usize = 32;

pub const PRIME_1: u64 = 11_400_714_785_074_694_791;
pub const PRIME_2: u64 = 14_029_467_366_897_019_727;
pub const PRIME_3: u64 = 1_609_587_929_392_839_161;
pub const PRIME_4: u64 = 9_650_029_242_287_828_579;
pub const PRIME_5: u64 = 2_870_177_450_012_600_261;

#[cfg_attr(feature = "serialize", derive(Deserialize, Serialize))]
#[derive(Copy, Clone, PartialEq)]
struct XxCore {
    v1: u64,
    v2: u64,
    v3: u64,
    v4: u64,
}

/// Calculates the 64-bit hash.
#[cfg_attr(feature = "serialize", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct XxHash64 {
    total_len: u64,
    seed: u64,
    core: XxCore,
    #[cfg_attr(feature = "serialize", serde(flatten))]
    buffer: Buffer,
}

impl XxCore {
    fn with_seed(seed: u64) -> XxCore {
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
        I: IntoIterator<Item = [u64; 4]>,
    {
        #[inline(always)]
        fn ingest_one_number(mut current_value: u64, mut value: u64) -> u64 {
            value = value.wrapping_mul(PRIME_2);
            current_value = current_value.wrapping_add(value);
            current_value = current_value.rotate_left(31);
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
    fn finish(&self) -> u64 {
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

        #[inline(always)]
        fn mix_one(mut hash: u64, mut value: u64) -> u64 {
            value = value.wrapping_mul(PRIME_2);
            value = value.rotate_left(31);
            value = value.wrapping_mul(PRIME_1);
            hash ^= value;
            hash = hash.wrapping_mul(PRIME_1);
            hash.wrapping_add(PRIME_4)
        }

        hash = mix_one(hash, self.v1);
        hash = mix_one(hash, self.v2);
        hash = mix_one(hash, self.v3);
        hash = mix_one(hash, self.v4);

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
#[repr(align(8))]
#[cfg_attr(feature = "serialize", serde(transparent))]
struct AlignToU64<T>(T);

#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[derive(Debug, Copy, Clone, Default, PartialEq)]
struct Buffer {
    #[cfg_attr(feature = "serialize", serde(rename = "buffer"))]
    data: AlignToU64<[u8; CHUNK_SIZE]>,
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

impl XxHash64 {
    /// Constructs the hash with an initial seed
    pub fn with_seed(seed: u64) -> XxHash64 {
        XxHash64 {
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
                let mut u64s = UnalignedBuffer::new(self.buffer.data());
                self.core.ingest_chunks(&mut u64s);
                debug_assert!(u64s.remaining().is_empty());
                self.buffer.len = 0;
            }
            data
        }
    }

    pub(crate) fn finish(&self) -> u64 {
        let mut hash = if self.total_len >= CHUNK_SIZE as u64 {
            // We have processed at least one full chunk
            self.core.finish()
        } else {
            self.seed.wrapping_add(PRIME_5)
        };

        hash = hash.wrapping_add(self.total_len);

        let mut buffered_u64s = UnalignedBuffer::<u64>::new(self.buffer.data());
        for buffered_u64 in &mut buffered_u64s {
            let mut k1 = buffered_u64.to_le().wrapping_mul(PRIME_2);
            k1 = k1.rotate_left(31);
            k1 = k1.wrapping_mul(PRIME_1);
            hash ^= k1;
            hash = hash.rotate_left(27);
            hash = hash.wrapping_mul(PRIME_1);
            hash = hash.wrapping_add(PRIME_4);
        }

        let mut buffered_u32s = UnalignedBuffer::<u32>::new(buffered_u64s.remaining());
        for buffered_u32 in &mut buffered_u32s {
            let k1 = u64::from(buffered_u32.to_le()).wrapping_mul(PRIME_1);
            hash ^= k1;
            hash = hash.rotate_left(23);
            hash = hash.wrapping_mul(PRIME_2);
            hash = hash.wrapping_add(PRIME_3);
        }

        let buffered_u8s = buffered_u32s.remaining();
        for &buffered_u8 in buffered_u8s {
            let k1 = u64::from(buffered_u8).wrapping_mul(PRIME_5);
            hash ^= k1;
            hash = hash.rotate_left(11);
            hash = hash.wrapping_mul(PRIME_1);
        }

        // The final intermixing
        hash ^= hash >> 33;
        hash = hash.wrapping_mul(PRIME_2);
        hash ^= hash >> 29;
        hash = hash.wrapping_mul(PRIME_3);
        hash ^= hash >> 32;

        hash
    }

    pub fn seed(&self) -> u64 {
        self.seed
    }

    pub fn total_len(&self) -> u64 {
        self.total_len
    }
}

impl Default for XxHash64 {
    fn default() -> XxHash64 {
        XxHash64::with_seed(0)
    }
}

impl Hasher for XxHash64 {
    fn finish(&self) -> u64 {
        XxHash64::finish(self)
    }

    fn write(&mut self, bytes: &[u8]) {
        XxHash64::write(self, bytes)
    }
}

#[cfg(feature = "std")]
pub use crate::std_support::sixty_four::RandomXxHashBuilder64;

#[cfg(test)]
mod test {
    use super::{RandomXxHashBuilder64, XxHash64};
    use std::collections::HashMap;
    use std::hash::BuildHasherDefault;
    use std::prelude::v1::*;

    #[test]
    fn ingesting_byte_by_byte_is_equivalent_to_large_chunks() {
        let bytes: Vec<_> = (0..32).map(|_| 0).collect();

        let mut byte_by_byte = XxHash64::with_seed(0);
        for byte in bytes.chunks(1) {
            byte_by_byte.write(byte);
        }

        let mut one_chunk = XxHash64::with_seed(0);
        one_chunk.write(&bytes);

        assert_eq!(byte_by_byte.core, one_chunk.core);
    }

    #[test]
    fn hash_of_nothing_matches_c_implementation() {
        let mut hasher = XxHash64::with_seed(0);
        hasher.write(&[]);
        assert_eq!(hasher.finish(), 0xef46_db37_51d8_e999);
    }

    #[test]
    fn hash_of_single_byte_matches_c_implementation() {
        let mut hasher = XxHash64::with_seed(0);
        hasher.write(&[42]);
        assert_eq!(hasher.finish(), 0x0a9e_dece_beb0_3ae4);
    }

    #[test]
    fn hash_of_multiple_bytes_matches_c_implementation() {
        let mut hasher = XxHash64::with_seed(0);
        hasher.write(b"Hello, world!\0");
        assert_eq!(hasher.finish(), 0x7b06_c531_ea43_e89f);
    }

    #[test]
    fn hash_of_multiple_chunks_matches_c_implementation() {
        let bytes: Vec<_> = (0..100).collect();
        let mut hasher = XxHash64::with_seed(0);
        hasher.write(&bytes);
        assert_eq!(hasher.finish(), 0x6ac1_e580_3216_6597);
    }

    #[test]
    fn hash_with_different_seed_matches_c_implementation() {
        let mut hasher = XxHash64::with_seed(0xae05_4331_1b70_2d91);
        hasher.write(&[]);
        assert_eq!(hasher.finish(), 0x4b6a_04fc_df7a_4672);
    }

    #[test]
    fn hash_with_different_seed_and_multiple_chunks_matches_c_implementation() {
        let bytes: Vec<_> = (0..100).collect();
        let mut hasher = XxHash64::with_seed(0xae05_4331_1b70_2d91);
        hasher.write(&bytes);
        assert_eq!(hasher.finish(), 0x567e_355e_0682_e1f1);
    }

    #[test]
    fn can_be_used_in_a_hashmap_with_a_default_seed() {
        let mut hash: HashMap<_, _, BuildHasherDefault<XxHash64>> = Default::default();
        hash.insert(42, "the answer");
        assert_eq!(hash.get(&42), Some(&"the answer"));
    }

    #[test]
    fn can_be_used_in_a_hashmap_with_a_random_seed() {
        let mut hash: HashMap<_, _, RandomXxHashBuilder64> = Default::default();
        hash.insert(42, "the answer");
        assert_eq!(hash.get(&42), Some(&"the answer"));
    }

    #[cfg(feature = "serialize")]
    type TestResult<T = ()> = Result<T, Box<dyn std::error::Error>>;

    #[cfg(feature = "serialize")]
    #[test]
    fn test_serialization_cycle() -> TestResult {
        let mut hasher = XxHash64::with_seed(0);
        hasher.write(b"Hello, world!\0");
        hasher.finish();

        let serialized = serde_json::to_string(&hasher)?;
        let unserialized: XxHash64 = serde_json::from_str(&serialized)?;
        assert_eq!(hasher, unserialized);
        Ok(())
    }

    #[cfg(feature = "serialize")]
    #[test]
    fn test_serialization_stability() -> TestResult {
        let mut hasher = XxHash64::with_seed(0);
        hasher.write(b"Hello, world!\0");
        hasher.finish();

        let serialized = r#"{
            "total_len": 14,
            "seed": 0,
            "core": {
              "v1": 6983438078262162902,
              "v2": 14029467366897019727,
              "v3": 0,
              "v4": 7046029288634856825
            },
            "buffer": [
              72,  101, 108, 108, 111, 44, 32, 119,
              111, 114, 108, 100, 33,  0,  0,  0,
              0,   0,   0,   0,   0,   0,  0,  0,
              0,   0,   0,   0,   0,   0,  0,  0
            ],
            "buffer_usage": 14
        }"#;

        let unserialized: XxHash64 = serde_json::from_str(serialized).unwrap();
        assert_eq!(hasher, unserialized);
        Ok(())
    }
}
