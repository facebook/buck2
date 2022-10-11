//! The in-progress XXH3 algorithm.
//!
//! Please read [the notes in original implementation][warning] to
//! learn about when to use these algorithms. Specifically, the
//! version of code this crate reproduces says:
//!
//! > The algorithm is currently in development, meaning its return
//!   values might still change in future versions. However, the API
//!   is stable, and can be used in production, typically for
//!   generation of ephemeral hashes (produced and consumed in same
//!   session).
//!
//! [warning]: https://github.com/Cyan4973/xxHash#new-hash-algorithms

use alloc::vec::Vec;

use core::convert::TryInto;
use core::hash::Hasher;
use core::mem;
use core::ops::{Deref, DerefMut};
use core::slice;

#[cfg(target_arch = "x86")]
use core::arch::x86::*;
#[cfg(target_arch = "x86_64")]
use core::arch::x86_64::*;

use cfg_if::cfg_if;
use static_assertions::{const_assert, const_assert_eq};

#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};

use crate::sixty_four::{
    PRIME_1 as PRIME64_1, PRIME_2 as PRIME64_2, PRIME_3 as PRIME64_3, PRIME_4 as PRIME64_4,
    PRIME_5 as PRIME64_5,
};
use crate::thirty_two::{PRIME_1 as PRIME32_1, PRIME_2 as PRIME32_2, PRIME_3 as PRIME32_3};

#[cfg(feature = "std")]
pub use crate::std_support::xxh3::{RandomHashBuilder128, RandomHashBuilder64};

#[inline(always)]
pub fn hash64(data: &[u8]) -> u64 {
    hash64_with_seed(data, 0)
}

#[inline(always)]
pub fn hash64_with_seed(data: &[u8], seed: u64) -> u64 {
    let len = data.len();

    if len <= 16 {
        hash_len_0to16_64bits(data, len, &SECRET, seed)
    } else if len <= 128 {
        hash_len_17to128_64bits(data, len, &SECRET, seed)
    } else if len <= MIDSIZE_MAX {
        hash_len_129to240_64bits(data, len, &SECRET, seed)
    } else {
        hash_long_64bits_with_seed(data, len, seed)
    }
}

#[inline(always)]
pub fn hash64_with_secret(data: &[u8], secret: &[u8]) -> u64 {
    debug_assert!(secret.len() >= SECRET_SIZE_MIN);

    let len = data.len();

    if len <= 16 {
        hash_len_0to16_64bits(data, len, secret, 0)
    } else if len <= 128 {
        hash_len_17to128_64bits(data, len, secret, 0)
    } else if len <= MIDSIZE_MAX {
        hash_len_129to240_64bits(data, len, secret, 0)
    } else {
        hash_long_64bits_with_secret(data, len, secret)
    }
}

#[inline(always)]
pub fn hash128(data: &[u8]) -> u128 {
    hash128_with_seed(data, 0)
}

#[inline(always)]
pub fn hash128_with_seed(data: &[u8], seed: u64) -> u128 {
    let len = data.len();

    if len <= 16 {
        hash_len_0to16_128bits(data, len, &SECRET, seed)
    } else if len <= 128 {
        hash_len_17to128_128bits(data, len, &SECRET, seed)
    } else if len <= MIDSIZE_MAX {
        hash_len_129to240_128bits(data, len, &SECRET, seed)
    } else {
        hash_long_128bits_with_seed(data, len, seed)
    }
}

#[inline(always)]
pub fn hash128_with_secret(data: &[u8], secret: &[u8]) -> u128 {
    debug_assert!(secret.len() >= SECRET_SIZE_MIN);

    let len = data.len();

    if len <= 16 {
        hash_len_0to16_128bits(data, len, secret, 0)
    } else if len <= 128 {
        hash_len_17to128_128bits(data, len, secret, 0)
    } else if len <= MIDSIZE_MAX {
        hash_len_129to240_128bits(data, len, secret, 0)
    } else {
        hash_long_128bits_with_secret(data, len, secret)
    }
}

/// Calculates the 64-bit hash.
#[cfg_attr(feature = "serialize", derive(Deserialize, Serialize))]
#[derive(Clone, Default)]
pub struct Hash64(State);

impl Hash64 {
    pub fn with_seed(seed: u64) -> Self {
        Self(State::with_seed(seed))
    }

    pub fn with_secret<S: Into<Vec<u8>>>(secret: S) -> Self {
        Self(State::with_secret(secret))
    }
}

impl Hasher for Hash64 {
    #[inline(always)]
    fn finish(&self) -> u64 {
        self.0.digest64()
    }

    #[inline(always)]
    fn write(&mut self, bytes: &[u8]) {
        self.0.update(bytes, AccWidth::Acc64Bits)
    }
}

/// Calculates the 128-bit hash.
#[cfg_attr(feature = "serialize", derive(Deserialize, Serialize))]
#[derive(Clone, Default)]
pub struct Hash128(State);

impl Hash128 {
    pub fn with_seed(seed: u64) -> Self {
        Self(State::with_seed(seed))
    }

    pub fn with_secret<S: Into<Vec<u8>>>(secret: S) -> Self {
        Self(State::with_secret(secret))
    }
}

impl Hasher for Hash128 {
    #[inline(always)]
    fn finish(&self) -> u64 {
        self.0.digest128() as u64
    }

    #[inline(always)]
    fn write(&mut self, bytes: &[u8]) {
        self.0.update(bytes, AccWidth::Acc128Bits)
    }
}

pub trait HasherExt: Hasher {
    fn finish_ext(&self) -> u128;
}

impl HasherExt for Hash128 {
    #[inline(always)]
    fn finish_ext(&self) -> u128 {
        self.0.digest128()
    }
}

/* ==========================================
 * XXH3 default settings
 * ========================================== */

const SECRET_DEFAULT_SIZE: usize = 192;
const SECRET_SIZE_MIN: usize = 136;

const SECRET: Secret = Secret([
    0xb8, 0xfe, 0x6c, 0x39, 0x23, 0xa4, 0x4b, 0xbe, 0x7c, 0x01, 0x81, 0x2c, 0xf7, 0x21, 0xad, 0x1c,
    0xde, 0xd4, 0x6d, 0xe9, 0x83, 0x90, 0x97, 0xdb, 0x72, 0x40, 0xa4, 0xa4, 0xb7, 0xb3, 0x67, 0x1f,
    0xcb, 0x79, 0xe6, 0x4e, 0xcc, 0xc0, 0xe5, 0x78, 0x82, 0x5a, 0xd0, 0x7d, 0xcc, 0xff, 0x72, 0x21,
    0xb8, 0x08, 0x46, 0x74, 0xf7, 0x43, 0x24, 0x8e, 0xe0, 0x35, 0x90, 0xe6, 0x81, 0x3a, 0x26, 0x4c,
    0x3c, 0x28, 0x52, 0xbb, 0x91, 0xc3, 0x00, 0xcb, 0x88, 0xd0, 0x65, 0x8b, 0x1b, 0x53, 0x2e, 0xa3,
    0x71, 0x64, 0x48, 0x97, 0xa2, 0x0d, 0xf9, 0x4e, 0x38, 0x19, 0xef, 0x46, 0xa9, 0xde, 0xac, 0xd8,
    0xa8, 0xfa, 0x76, 0x3f, 0xe3, 0x9c, 0x34, 0x3f, 0xf9, 0xdc, 0xbb, 0xc7, 0xc7, 0x0b, 0x4f, 0x1d,
    0x8a, 0x51, 0xe0, 0x4b, 0xcd, 0xb4, 0x59, 0x31, 0xc8, 0x9f, 0x7e, 0xc9, 0xd9, 0x78, 0x73, 0x64,
    0xea, 0xc5, 0xac, 0x83, 0x34, 0xd3, 0xeb, 0xc3, 0xc5, 0x81, 0xa0, 0xff, 0xfa, 0x13, 0x63, 0xeb,
    0x17, 0x0d, 0xdd, 0x51, 0xb7, 0xf0, 0xda, 0x49, 0xd3, 0x16, 0x55, 0x26, 0x29, 0xd4, 0x68, 0x9e,
    0x2b, 0x16, 0xbe, 0x58, 0x7d, 0x47, 0xa1, 0xfc, 0x8f, 0xf8, 0xb8, 0xd1, 0x7a, 0xd0, 0x31, 0xce,
    0x45, 0xcb, 0x3a, 0x8f, 0x95, 0x16, 0x04, 0x28, 0xaf, 0xd7, 0xfb, 0xca, 0xbb, 0x4b, 0x40, 0x7e,
]);

#[repr(align(64))]
#[derive(Clone)]
struct Secret([u8; SECRET_DEFAULT_SIZE]);

const_assert_eq!(mem::size_of::<Secret>() % 16, 0);

impl Default for Secret {
    #[inline(always)]
    fn default() -> Self {
        SECRET
    }
}

impl Deref for Secret {
    type Target = [u8];

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0[..]
    }
}

cfg_if! {
    if #[cfg(feature = "serialize")] {
        impl Serialize for Secret {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                serializer.serialize_bytes(self)
            }
        }

        impl<'de> Deserialize<'de> for Secret {
            fn deserialize<D>(deserializer: D) -> Result<Secret, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                deserializer.deserialize_bytes(SecretVisitor)
            }
        }

        struct SecretVisitor;

        impl<'de> serde::de::Visitor<'de> for SecretVisitor {
            type Value = Secret;

            fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
                formatter.write_str("secret with a bytes array")
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if v.len() == SECRET_DEFAULT_SIZE {
                    let mut secret = [0; SECRET_DEFAULT_SIZE];

                    secret.copy_from_slice(v);

                    Ok(Secret(secret))
                } else {
                    Err(E::custom("incomplete secret data"))
                }
            }
        }
    }
}

impl Secret {
    #[inline(always)]
    pub fn with_seed(seed: u64) -> Self {
        let mut secret = [0; SECRET_DEFAULT_SIZE];

        for off in (0..SECRET_DEFAULT_SIZE).step_by(16) {
            secret[off..].write_u64_le(SECRET[off..].read_u64_le().wrapping_add(seed));
            secret[off + 8..].write_u64_le(SECRET[off + 8..].read_u64_le().wrapping_sub(seed));
        }

        Secret(secret)
    }
}

cfg_if! {
    if #[cfg(target_feature = "avx2")] {
        #[repr(align(32))]
        #[cfg_attr(feature = "serialize", derive(Deserialize, Serialize))]
        #[derive(Clone)]
        struct Acc([u64; ACC_NB]);
    } else if #[cfg(target_feature = "sse2")] {
        #[repr(align(16))]
        #[cfg_attr(feature = "serialize", derive(Deserialize, Serialize))]
        #[derive(Clone)]
        struct Acc([u64; ACC_NB]);
    } else {
        #[repr(align(8))]
        #[cfg_attr(feature = "serialize", derive(Deserialize, Serialize))]
        #[derive(Clone)]
        struct Acc([u64; ACC_NB]);
    }
}

const ACC_SIZE: usize = mem::size_of::<Acc>();

const_assert_eq!(ACC_SIZE, 64);

impl Default for Acc {
    #[inline(always)]
    fn default() -> Self {
        Acc([
            u64::from(PRIME32_3),
            PRIME64_1,
            PRIME64_2,
            PRIME64_3,
            PRIME64_4,
            u64::from(PRIME32_2),
            PRIME64_5,
            u64::from(PRIME32_1),
        ])
    }
}

impl Deref for Acc {
    type Target = [u64];

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Acc {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

trait Buf {
    fn read_u32_le(&self) -> u32;

    fn read_u64_le(&self) -> u64;
}

trait BufMut {
    fn write_u32_le(&mut self, n: u32);

    fn write_u64_le(&mut self, n: u64);
}

impl Buf for [u8] {
    #[inline(always)]
    fn read_u32_le(&self) -> u32 {
        let buf = &self[..mem::size_of::<u32>()];
        u32::from_le_bytes(buf.try_into().unwrap())
    }

    #[inline(always)]
    fn read_u64_le(&self) -> u64 {
        let buf = &self[..mem::size_of::<u64>()];
        u64::from_le_bytes(buf.try_into().unwrap())
    }
}

impl BufMut for [u8] {
    #[inline(always)]
    fn write_u32_le(&mut self, n: u32) {
        self[..mem::size_of::<u32>()].copy_from_slice(&n.to_le_bytes()[..]);
    }

    #[inline(always)]
    fn write_u64_le(&mut self, n: u64) {
        self[..mem::size_of::<u64>()].copy_from_slice(&n.to_le_bytes()[..]);
    }
}

/* ==========================================
 * Short keys
 * ========================================== */

#[inline(always)]
fn hash_len_0to16_64bits(data: &[u8], len: usize, key: &[u8], seed: u64) -> u64 {
    debug_assert!(len <= 16);

    if len > 8 {
        hash_len_9to16_64bits(data, len, key, seed)
    } else if len >= 4 {
        hash_len_4to8_64bits(data, len, key, seed)
    } else if len > 0 {
        hash_len_1to3_64bits(data, len, key, seed)
    } else {
        0
    }
}

#[inline(always)]
fn hash_len_9to16_64bits(data: &[u8], len: usize, key: &[u8], seed: u64) -> u64 {
    debug_assert!((9..=16).contains(&len));

    let ll1 = data.read_u64_le() ^ key.read_u64_le().wrapping_add(seed);
    let ll2 = data[len - 8..].read_u64_le() ^ key[8..].read_u64_le().wrapping_sub(seed);
    let acc = (len as u64)
        .wrapping_add(ll1)
        .wrapping_add(ll2)
        .wrapping_add(mul128_fold64(ll1, ll2));

    avalanche(acc)
}

#[inline(always)]
fn hash_len_4to8_64bits(data: &[u8], len: usize, key: &[u8], seed: u64) -> u64 {
    debug_assert!((4..=8).contains(&len));

    let in1 = u64::from(data.read_u32_le());
    let in2 = u64::from(data[len - 4..].read_u32_le());
    let in64 = in1.wrapping_add(in2 << 32);
    let keyed = in64 ^ key.read_u64_le().wrapping_add(seed);
    let mix64 =
        (len as u64).wrapping_add((keyed ^ (keyed >> 51)).wrapping_mul(u64::from(PRIME32_1)));

    avalanche((mix64 ^ (mix64 >> 47)).wrapping_mul(PRIME64_2))
}

#[inline(always)]
fn hash_len_1to3_64bits(data: &[u8], len: usize, key: &[u8], seed: u64) -> u64 {
    debug_assert!((1..=3).contains(&len));

    let c1 = u32::from(data[0]);
    let c2 = u32::from(data[len >> 1]);
    let c3 = u32::from(data[len - 1]);
    let combined = c1 + (c2 << 8) + (c3 << 16) + ((len as u32) << 24);
    let keyed = u64::from(combined) ^ u64::from(key.read_u32_le()).wrapping_add(seed);
    let mixed = keyed.wrapping_mul(PRIME64_1);

    avalanche(mixed)
}

#[inline(always)]
fn hash_len_17to128_64bits(data: &[u8], len: usize, secret: &[u8], seed: u64) -> u64 {
    debug_assert!((17..=128).contains(&len));
    debug_assert!(secret.len() >= SECRET_SIZE_MIN);

    let mut acc = PRIME64_1.wrapping_mul(len as u64);

    if len > 32 {
        if len > 64 {
            if len > 96 {
                acc = acc
                    .wrapping_add(mix_16bytes(&data[48..], &secret[96..], seed))
                    .wrapping_add(mix_16bytes(&data[len - 64..], &secret[112..], seed));
            }
            acc = acc
                .wrapping_add(mix_16bytes(&data[32..], &secret[64..], seed))
                .wrapping_add(mix_16bytes(&data[len - 48..], &secret[80..], seed));
        }

        acc = acc
            .wrapping_add(mix_16bytes(&data[16..], &secret[32..], seed))
            .wrapping_add(mix_16bytes(&data[len - 32..], &secret[48..], seed));
    }

    acc = acc
        .wrapping_add(mix_16bytes(data, secret, seed))
        .wrapping_add(mix_16bytes(&data[len - 16..], &secret[16..], seed));

    avalanche(acc)
}

const MIDSIZE_MAX: usize = 240;
const MIDSIZE_STARTOFFSET: usize = 3;
const MIDSIZE_LASTOFFSET: usize = 17;

#[inline(always)]
fn hash_len_129to240_64bits(data: &[u8], len: usize, secret: &[u8], seed: u64) -> u64 {
    debug_assert!((129..=MIDSIZE_MAX).contains(&len));
    debug_assert!(secret.len() >= SECRET_SIZE_MIN);

    let acc = (len as u64).wrapping_mul(PRIME64_1);
    let acc = (0..8).fold(acc, |acc, i| {
        acc.wrapping_add(mix_16bytes(&data[16 * i..], &secret[16 * i..], seed))
    });
    let acc = avalanche(acc);

    let nb_rounds = len / 16;
    debug_assert!(nb_rounds >= 8);

    let acc = (8..nb_rounds).fold(acc, |acc, i| {
        acc.wrapping_add(mix_16bytes(
            &data[16 * i..],
            &secret[16 * (i - 8) + MIDSIZE_STARTOFFSET..],
            seed,
        ))
    });

    avalanche(acc.wrapping_add(mix_16bytes(
        &data[len - 16..],
        &secret[SECRET_SIZE_MIN - MIDSIZE_LASTOFFSET..],
        seed,
    )))
}

/* ==========================================
 * Long keys
 * ========================================== */

const STRIPE_LEN: usize = 64;
const SECRET_CONSUME_RATE: usize = 8; // nb of secret bytes consumed at each accumulation
const SECRET_MERGEACCS_START: usize = 11; // do not align on 8, so that secret is different from accumulator
const SECRET_LASTACC_START: usize = 7; // do not align on 8, so that secret is different from scrambler
const ACC_NB: usize = STRIPE_LEN / mem::size_of::<u64>();

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum AccWidth {
    Acc64Bits,
    Acc128Bits,
}

#[inline(always)]
fn hash_long_64bits_with_default_secret(data: &[u8], len: usize) -> u64 {
    hash_long_internal(data, len, &SECRET)
}

#[inline(always)]
fn hash_long_64bits_with_secret(data: &[u8], len: usize, secret: &[u8]) -> u64 {
    hash_long_internal(data, len, secret)
}

/// Generate a custom key, based on alteration of default kSecret with the seed,
/// and then use this key for long mode hashing.
///
/// This operation is decently fast but nonetheless costs a little bit of time.
/// Try to avoid it whenever possible (typically when `seed.is_none()`).
#[inline(always)]
fn hash_long_64bits_with_seed(data: &[u8], len: usize, seed: u64) -> u64 {
    if seed == 0 {
        hash_long_64bits_with_default_secret(data, len)
    } else {
        let secret = Secret::with_seed(seed);

        hash_long_internal(data, len, &secret)
    }
}

#[inline(always)]
fn hash_long_internal(data: &[u8], len: usize, secret: &[u8]) -> u64 {
    let mut acc = Acc::default();

    hash_long_internal_loop(&mut acc, data, len, secret, AccWidth::Acc64Bits);

    merge_accs(
        &acc,
        &secret[SECRET_MERGEACCS_START..],
        (len as u64).wrapping_mul(PRIME64_1),
    )
}

#[inline(always)]
fn hash_long_internal_loop(
    acc: &mut [u64],
    data: &[u8],
    len: usize,
    secret: &[u8],
    acc_width: AccWidth,
) {
    let secret_len = secret.len();
    let nb_rounds = (secret_len - STRIPE_LEN) / SECRET_CONSUME_RATE;
    let block_len = STRIPE_LEN * nb_rounds;

    debug_assert!(secret_len >= SECRET_SIZE_MIN);

    let mut chunks = data.chunks_exact(block_len);

    for chunk in &mut chunks {
        accumulate(acc, chunk, secret, nb_rounds, acc_width);
        unsafe {
            scramble_acc(acc, &secret[secret_len - STRIPE_LEN..]);
        }
    }

    /* last partial block */
    debug_assert!(len > STRIPE_LEN);

    let nb_stripes = (len % block_len) / STRIPE_LEN;

    debug_assert!(nb_stripes < (secret_len / SECRET_CONSUME_RATE));

    accumulate(acc, chunks.remainder(), secret, nb_stripes, acc_width);

    /* last stripe */
    if (len & (STRIPE_LEN - 1)) != 0 {
        unsafe {
            accumulate512(
                acc,
                &data[len - STRIPE_LEN..],
                &secret[secret_len - STRIPE_LEN - SECRET_LASTACC_START..],
                acc_width,
            );
        }
    }
}

#[inline(always)]
fn accumulate(acc: &mut [u64], data: &[u8], secret: &[u8], nb_stripes: usize, acc_width: AccWidth) {
    for n in 0..nb_stripes {
        unsafe {
            accumulate512(
                acc,
                &data[n * STRIPE_LEN..],
                &secret[n * SECRET_CONSUME_RATE..],
                acc_width,
            );
        }
    }
}

#[inline(always)]
const fn _mm_shuffle(z: u32, y: u32, x: u32, w: u32) -> i32 {
    ((z << 6) | (y << 4) | (x << 2) | w) as i32
}

#[cfg(target_feature = "avx2")]
mod avx2 {
    use super::*;

    #[target_feature(enable = "avx2")]
    pub(crate) unsafe fn accumulate512(
        acc: &mut [u64],
        data: &[u8],
        keys: &[u8],
        acc_width: AccWidth,
    ) {
        let xacc = acc.as_mut_ptr() as *mut __m256i;
        let xdata = data.as_ptr() as *const __m256i;
        let xkey = keys.as_ptr() as *const __m256i;

        for i in 0..STRIPE_LEN / mem::size_of::<__m256i>() {
            let d = _mm256_loadu_si256(xdata.add(i));
            let k = _mm256_loadu_si256(xkey.add(i));
            let dk = _mm256_xor_si256(d, k); // uint32 dk[8]  = {d0+k0, d1+k1, d2+k2, d3+k3, ...}
            let mul = _mm256_mul_epu32(dk, _mm256_shuffle_epi32(dk, 0x31)); // uint64 res[4] = {dk0*dk1, dk2*dk3, ...}

            xacc.add(i).write(if acc_width == AccWidth::Acc128Bits {
                let dswap = _mm256_shuffle_epi32(d, _mm_shuffle(1, 0, 3, 2));
                let add = _mm256_add_epi64(xacc.add(i).read(), dswap);
                _mm256_add_epi64(mul, add)
            } else {
                let add = _mm256_add_epi64(xacc.add(i).read(), d);
                _mm256_add_epi64(mul, add)
            })
        }
    }

    #[target_feature(enable = "avx2")]
    pub unsafe fn scramble_acc(acc: &mut [u64], key: &[u8]) {
        let xacc = acc.as_mut_ptr() as *mut __m256i;
        let xkey = key.as_ptr() as *const __m256i;
        let prime32 = _mm256_set1_epi32(PRIME32_1 as i32);

        for i in 0..STRIPE_LEN / mem::size_of::<__m256i>() {
            let data = xacc.add(i).read();
            let shifted = _mm256_srli_epi64(data, 47);
            let data = _mm256_xor_si256(data, shifted);

            let k = _mm256_loadu_si256(xkey.add(i));
            let dk = _mm256_xor_si256(data, k); /* U32 dk[4]  = {d0+k0, d1+k1, d2+k2, d3+k3} */
            let dk1 = _mm256_mul_epu32(dk, prime32);

            let d2 = _mm256_shuffle_epi32(dk, 0x31);
            let dk2 = _mm256_mul_epu32(d2, prime32);
            let dk2h = _mm256_slli_epi64(dk2, 32);

            xacc.add(i).write(_mm256_add_epi64(dk1, dk2h));
        }
    }
}

#[cfg(all(target_feature = "sse2", not(target_feature = "avx2")))]
mod sse2 {
    use super::*;

    #[target_feature(enable = "sse2")]
    #[allow(clippy::cast_ptr_alignment)]
    pub(crate) unsafe fn accumulate512(
        acc: &mut [u64],
        data: &[u8],
        keys: &[u8],
        acc_width: AccWidth,
    ) {
        let xacc = acc.as_mut_ptr() as *mut __m128i;
        let xdata = data.as_ptr() as *const __m128i;
        let xkey = keys.as_ptr() as *const __m128i;

        for i in 0..STRIPE_LEN / mem::size_of::<__m128i>() {
            let d = _mm_loadu_si128(xdata.add(i));
            let k = _mm_loadu_si128(xkey.add(i));
            let dk = _mm_xor_si128(d, k); // uint32 dk[4]  = {d0+k0, d1+k1, d2+k2, d3+k3} */
            let mul = _mm_mul_epu32(dk, _mm_shuffle_epi32(dk, 0x31)); // uint64 res[4] = {dk0*dk1, dk2*dk3, ...} */
            xacc.add(i).write(if acc_width == AccWidth::Acc128Bits {
                let dswap = _mm_shuffle_epi32(d, _mm_shuffle(1, 0, 3, 2));
                let add = _mm_add_epi64(xacc.add(i).read(), dswap);
                _mm_add_epi64(mul, add)
            } else {
                let add = _mm_add_epi64(xacc.add(i).read(), d);
                _mm_add_epi64(mul, add)
            })
        }
    }

    #[target_feature(enable = "sse2")]
    #[allow(clippy::cast_ptr_alignment)]
    pub unsafe fn scramble_acc(acc: &mut [u64], key: &[u8]) {
        let xacc = acc.as_mut_ptr() as *mut __m128i;
        let xkey = key.as_ptr() as *const __m128i;
        let prime32 = _mm_set1_epi32(PRIME32_1 as i32);

        for i in 0..STRIPE_LEN / mem::size_of::<__m128i>() {
            let data = xacc.add(i).read();
            let shifted = _mm_srli_epi64(data, 47);
            let data = _mm_xor_si128(data, shifted);

            let k = _mm_loadu_si128(xkey.add(i));
            let dk = _mm_xor_si128(data, k);

            let dk1 = _mm_mul_epu32(dk, prime32);

            let d2 = _mm_shuffle_epi32(dk, 0x31);
            let dk2 = _mm_mul_epu32(d2, prime32);
            let dk2h = _mm_slli_epi64(dk2, 32);

            xacc.add(i).write(_mm_add_epi64(dk1, dk2h));
        }
    }
}

#[cfg(not(any(target_feature = "avx2", target_feature = "sse2")))]
mod generic {
    use super::*;

    #[inline(always)]
    pub(crate) unsafe fn accumulate512(
        acc: &mut [u64],
        data: &[u8],
        key: &[u8],
        acc_width: AccWidth,
    ) {
        for i in (0..ACC_NB).step_by(2) {
            let in1 = data[8 * i..].read_u64_le();
            let in2 = data[8 * (i + 1)..].read_u64_le();
            let key1 = key[8 * i..].read_u64_le();
            let key2 = key[8 * (i + 1)..].read_u64_le();
            let data_key1 = key1 ^ in1;
            let data_key2 = key2 ^ in2;
            acc[i] = acc[i].wrapping_add(mul32_to64(data_key1, data_key1 >> 32));
            acc[i + 1] = acc[i + 1].wrapping_add(mul32_to64(data_key2, data_key2 >> 32));

            if acc_width == AccWidth::Acc128Bits {
                acc[i] = acc[i].wrapping_add(in2);
                acc[i + 1] = acc[i + 1].wrapping_add(in1);
            } else {
                acc[i] = acc[i].wrapping_add(in1);
                acc[i + 1] = acc[i + 1].wrapping_add(in2);
            }
        }
    }

    #[inline(always)]
    fn mul32_to64(a: u64, b: u64) -> u64 {
        (a & 0xFFFFFFFF).wrapping_mul(b & 0xFFFFFFFF)
    }

    #[inline(always)]
    pub unsafe fn scramble_acc(acc: &mut [u64], key: &[u8]) {
        for i in 0..ACC_NB {
            let key64 = key[8 * i..].read_u64_le();
            let mut acc64 = acc[i];
            acc64 ^= acc64 >> 47;
            acc64 ^= key64;
            acc64 = acc64.wrapping_mul(u64::from(PRIME32_1));
            acc[i] = acc64;
        }
    }
}

cfg_if! {
    if #[cfg(target_feature = "avx2")] {
        use avx2::{accumulate512, scramble_acc};
    } else if #[cfg(target_feature = "sse2")] {
        use sse2::{accumulate512, scramble_acc};
    } else {
        use generic::{accumulate512, scramble_acc};
    }
}

#[inline(always)]
fn merge_accs(acc: &[u64], secret: &[u8], start: u64) -> u64 {
    avalanche(
        start
            .wrapping_add(mix2accs(acc, secret))
            .wrapping_add(mix2accs(&acc[2..], &secret[16..]))
            .wrapping_add(mix2accs(&acc[4..], &secret[32..]))
            .wrapping_add(mix2accs(&acc[6..], &secret[48..])),
    )
}

#[inline(always)]
fn mix2accs(acc: &[u64], secret: &[u8]) -> u64 {
    mul128_fold64(
        acc[0] ^ secret.read_u64_le(),
        acc[1] ^ secret[8..].read_u64_le(),
    )
}

#[inline(always)]
fn mix_16bytes(data: &[u8], key: &[u8], seed: u64) -> u64 {
    let ll1 = data.read_u64_le();
    let ll2 = data[8..].read_u64_le();

    mul128_fold64(
        ll1 ^ key.read_u64_le().wrapping_add(seed),
        ll2 ^ key[8..].read_u64_le().wrapping_sub(seed),
    )
}

#[inline(always)]
fn mul128_fold64(ll1: u64, ll2: u64) -> u64 {
    let lll = u128::from(ll1).wrapping_mul(u128::from(ll2));

    (lll as u64) ^ ((lll >> 64) as u64)
}

#[inline(always)]
fn avalanche(mut h64: u64) -> u64 {
    h64 ^= h64 >> 37;
    h64 = h64.wrapping_mul(PRIME64_3);
    h64 ^ (h64 >> 32)
}

/* ===   XXH3 streaming   === */

const INTERNAL_BUFFER_SIZE: usize = 256;
const INTERNAL_BUFFER_STRIPES: usize = INTERNAL_BUFFER_SIZE / STRIPE_LEN;

const_assert!(INTERNAL_BUFFER_SIZE >= MIDSIZE_MAX);
const_assert_eq!(INTERNAL_BUFFER_SIZE % STRIPE_LEN, 0);

#[repr(align(64))]
#[cfg_attr(feature = "serialize", derive(Deserialize, Serialize))]
#[derive(Clone)]
struct State {
    acc: Acc,
    secret: With,
    buf: Vec<u8>,
    seed: u64,
    total_len: usize,
    nb_stripes_so_far: usize,
}

#[cfg_attr(feature = "serialize", derive(Deserialize, Serialize))]
#[derive(Clone)]
enum With {
    Default(Secret),
    Custom(Secret),
    Ref(Vec<u8>),
}

impl Deref for With {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            With::Default(secret) | With::Custom(secret) => &secret.0[..],
            With::Ref(secret) => secret,
        }
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new(0, With::Default(Secret::default()))
    }
}

impl State {
    fn new(seed: u64, secret: With) -> Self {
        State {
            acc: Acc::default(),
            secret,
            buf: Vec::with_capacity(INTERNAL_BUFFER_SIZE),
            seed,
            total_len: 0,
            nb_stripes_so_far: 0,
        }
    }

    fn with_seed(seed: u64) -> Self {
        Self::new(seed, With::Custom(Secret::with_seed(seed)))
    }

    fn with_secret<S: Into<Vec<u8>>>(secret: S) -> State {
        let secret = secret.into();

        debug_assert!(secret.len() >= SECRET_SIZE_MIN);

        Self::new(0, With::Ref(secret))
    }

    #[inline(always)]
    fn secret_limit(&self) -> usize {
        self.secret.len() - STRIPE_LEN
    }

    #[inline(always)]
    fn nb_stripes_per_block(&self) -> usize {
        self.secret_limit() / SECRET_CONSUME_RATE
    }

    #[inline(always)]
    fn update(&mut self, mut input: &[u8], acc_width: AccWidth) {
        let len = input.len();

        if len == 0 {
            return;
        }

        self.total_len += len;

        if self.buf.len() + len <= self.buf.capacity() {
            self.buf.extend_from_slice(input);
            return;
        }

        let nb_stripes_per_block = self.nb_stripes_per_block();
        let secret_limit = self.secret_limit();

        if !self.buf.is_empty() {
            // some data within internal buffer: fill then consume it
            let (load, rest) = input.split_at(self.buf.capacity() - self.buf.len());
            self.buf.extend_from_slice(load);
            input = rest;
            self.nb_stripes_so_far = consume_stripes(
                &mut self.acc,
                self.nb_stripes_so_far,
                nb_stripes_per_block,
                &self.buf,
                INTERNAL_BUFFER_STRIPES,
                &self.secret,
                secret_limit,
                acc_width,
            );
            self.buf.clear();
        }

        // consume input by full buffer quantities
        let mut chunks = input.chunks_exact(INTERNAL_BUFFER_SIZE);

        for chunk in &mut chunks {
            self.nb_stripes_so_far = consume_stripes(
                &mut self.acc,
                self.nb_stripes_so_far,
                nb_stripes_per_block,
                chunk,
                INTERNAL_BUFFER_STRIPES,
                &self.secret,
                secret_limit,
                acc_width,
            );
        }

        // some remaining input data : buffer it
        self.buf.extend_from_slice(chunks.remainder())
    }

    #[inline(always)]
    fn digest_long(&self, acc_width: AccWidth) -> Acc {
        let mut acc = self.acc.clone();
        let secret_limit = self.secret_limit();

        if self.buf.len() >= STRIPE_LEN {
            // digest locally, state remains unaltered, and can continue ingesting more data afterwards
            let total_nb_stripes = self.buf.len() / STRIPE_LEN;
            let _nb_stripes_so_far = consume_stripes(
                &mut acc,
                self.nb_stripes_so_far,
                self.nb_stripes_per_block(),
                &self.buf,
                total_nb_stripes,
                &self.secret,
                secret_limit,
                acc_width,
            );
            if (self.buf.len() % STRIPE_LEN) != 0 {
                unsafe {
                    accumulate512(
                        &mut acc,
                        &self.buf[self.buf.len() - STRIPE_LEN..],
                        &self.secret[secret_limit - SECRET_LASTACC_START..],
                        acc_width,
                    );
                }
            }
        } else if !self.buf.is_empty() {
            // one last stripe
            let mut last_stripe = [0u8; STRIPE_LEN];
            let catchup_size = STRIPE_LEN - self.buf.len();

            last_stripe[..catchup_size].copy_from_slice(unsafe {
                slice::from_raw_parts(
                    self.buf.as_ptr().add(self.buf.capacity() - catchup_size),
                    catchup_size,
                )
            });
            last_stripe[catchup_size..].copy_from_slice(&self.buf);

            unsafe {
                accumulate512(
                    &mut acc,
                    &last_stripe[..],
                    &self.secret[secret_limit - SECRET_LASTACC_START..],
                    acc_width,
                );
            }
        }

        acc
    }

    #[inline(always)]
    fn digest64(&self) -> u64 {
        if self.total_len > MIDSIZE_MAX {
            let acc = self.digest_long(AccWidth::Acc64Bits);

            merge_accs(
                &acc,
                &self.secret[SECRET_MERGEACCS_START..],
                (self.total_len as u64).wrapping_mul(PRIME64_1),
            )
        } else if self.seed != 0 {
            hash64_with_seed(&self.buf, self.seed)
        } else {
            hash64_with_secret(&self.buf, &self.secret[..self.secret_limit() + STRIPE_LEN])
        }
    }

    #[inline(always)]
    fn digest128(&self) -> u128 {
        let secret_limit = self.secret_limit();

        if self.total_len > MIDSIZE_MAX {
            let acc = self.digest_long(AccWidth::Acc128Bits);

            debug_assert!(secret_limit + STRIPE_LEN >= ACC_SIZE + SECRET_MERGEACCS_START);

            let total_len = self.total_len as u64;

            let low64 = merge_accs(
                &acc,
                &self.secret[SECRET_MERGEACCS_START..],
                total_len.wrapping_mul(PRIME64_1),
            );
            let high64 = merge_accs(
                &acc,
                &self.secret[secret_limit + STRIPE_LEN - ACC_SIZE - SECRET_MERGEACCS_START..],
                !total_len.wrapping_mul(PRIME64_2),
            );

            u128::from(low64) + (u128::from(high64) << 64)
        } else if self.seed != 0 {
            hash128_with_seed(&self.buf, self.seed)
        } else {
            hash128_with_secret(&self.buf, &self.secret[..secret_limit + STRIPE_LEN])
        }
    }
}

#[inline(always)]
#[allow(clippy::too_many_arguments)]
fn consume_stripes(
    acc: &mut [u64],
    nb_stripes_so_far: usize,
    nb_stripes_per_block: usize,
    data: &[u8],
    total_stripes: usize,
    secret: &[u8],
    secret_limit: usize,
    acc_width: AccWidth,
) -> usize {
    debug_assert!(nb_stripes_so_far < nb_stripes_per_block);

    if nb_stripes_per_block - nb_stripes_so_far <= total_stripes {
        let nb_stripes = nb_stripes_per_block - nb_stripes_so_far;

        accumulate(
            acc,
            data,
            &secret[nb_stripes_so_far * SECRET_CONSUME_RATE..],
            nb_stripes,
            acc_width,
        );
        unsafe {
            scramble_acc(acc, &secret[secret_limit..]);
        }
        accumulate(
            acc,
            &data[nb_stripes * STRIPE_LEN..],
            secret,
            total_stripes - nb_stripes,
            acc_width,
        );

        total_stripes - nb_stripes
    } else {
        accumulate(
            acc,
            data,
            &secret[nb_stripes_so_far * SECRET_CONSUME_RATE..],
            total_stripes,
            acc_width,
        );

        nb_stripes_so_far + total_stripes
    }
}

/* ==========================================
 * XXH3 128 bits (=> XXH128)
 * ========================================== */

#[inline(always)]
fn hash_len_0to16_128bits(data: &[u8], len: usize, secret: &[u8], seed: u64) -> u128 {
    debug_assert!(len <= 16);

    if len > 8 {
        hash_len_9to16_128bits(data, len, secret, seed)
    } else if len >= 4 {
        hash_len_4to8_128bits(data, len, secret, seed)
    } else if len > 0 {
        hash_len_1to3_128bits(data, len, secret, seed)
    } else {
        0
    }
}

#[inline(always)]
fn hash_len_1to3_128bits(data: &[u8], len: usize, key: &[u8], seed: u64) -> u128 {
    debug_assert!((1..=3).contains(&len));

    let c1 = u32::from(data[0]);
    let c2 = u32::from(data[len >> 1]);
    let c3 = u32::from(data[len - 1]);
    let combinedl = c1 + (c2 << 8) + (c3 << 16) + ((len as u32) << 24);
    let combinedh = combinedl.swap_bytes();
    let keyedl = u64::from(combinedl) ^ u64::from(key.read_u32_le()).wrapping_add(seed);
    let keyedh = u64::from(combinedh) ^ u64::from(key[4..].read_u32_le()).wrapping_sub(seed);
    let mixedl = keyedl.wrapping_mul(PRIME64_1);
    let mixedh = keyedh.wrapping_mul(PRIME64_2);

    u128::from(avalanche(mixedl)) + (u128::from(avalanche(mixedh)) << 64)
}

#[inline(always)]
fn hash_len_4to8_128bits(data: &[u8], len: usize, key: &[u8], seed: u64) -> u128 {
    debug_assert!((4..=8).contains(&len));

    let in1 = u64::from(data.read_u32_le());
    let in2 = u64::from(data[len - 4..].read_u32_le());
    let in64l = in1.wrapping_add(in2 << 32);
    let in64h = in64l.swap_bytes();
    let keyedl = in64l ^ key.read_u64_le().wrapping_add(seed);
    let keyedh = in64h ^ key[8..].read_u64_le().wrapping_sub(seed);
    let mix64l1 =
        (len as u64).wrapping_add((keyedl ^ (keyedl >> 51)).wrapping_mul(u64::from(PRIME32_1)));
    let mix64l2 = (mix64l1 ^ (mix64l1 >> 47)).wrapping_mul(PRIME64_2);
    let mix64h1 = (keyedh ^ (keyedh >> 47))
        .wrapping_mul(PRIME64_1)
        .wrapping_sub(len as u64);
    let mix64h2 = (mix64h1 ^ (mix64h1 >> 43)).wrapping_mul(PRIME64_4);

    u128::from(avalanche(mix64l2)) + (u128::from(avalanche(mix64h2)) << 64)
}

#[inline(always)]
fn hash_len_9to16_128bits(data: &[u8], len: usize, key: &[u8], seed: u64) -> u128 {
    debug_assert!((9..=16).contains(&len));

    let ll1 = data.read_u64_le() ^ key.read_u64_le().wrapping_add(seed);
    let ll2 = data[len - 8..].read_u64_le() ^ key[8..].read_u64_le().wrapping_sub(seed);
    let inlow = ll1 ^ ll2;

    let m128 = u128::from(inlow).wrapping_mul(u128::from(PRIME64_1));
    let high64 = ((m128 >> 64) as u64).wrapping_add(ll2.wrapping_mul(PRIME64_1));
    let low64 = (m128 as u64) ^ (high64 >> 32);

    let h128 = u128::from(low64).wrapping_mul(u128::from(PRIME64_2));
    let high64 = ((h128 >> 64) as u64).wrapping_add(high64.wrapping_mul(PRIME64_2));
    let low64 = h128 as u64;

    u128::from(avalanche(low64)) + (u128::from(avalanche(high64)) << 64)
}

#[inline(always)]
fn hash_len_17to128_128bits(data: &[u8], len: usize, secret: &[u8], seed: u64) -> u128 {
    debug_assert!((17..=128).contains(&len));
    debug_assert!(secret.len() >= SECRET_SIZE_MIN);

    let mut acc1 = PRIME64_1.wrapping_mul(len as u64);
    let mut acc2 = 0u64;

    if len > 32 {
        if len > 64 {
            if len > 96 {
                acc1 = acc1.wrapping_add(mix_16bytes(&data[48..], &secret[96..], seed));
                acc2 = acc2.wrapping_add(mix_16bytes(&data[len - 64..], &secret[112..], seed));
            }
            acc1 = acc1.wrapping_add(mix_16bytes(&data[32..], &secret[64..], seed));
            acc2 = acc2.wrapping_add(mix_16bytes(&data[len - 48..], &secret[80..], seed));
        }

        acc1 = acc1.wrapping_add(mix_16bytes(&data[16..], &secret[32..], seed));
        acc2 = acc2.wrapping_add(mix_16bytes(&data[len - 32..], &secret[48..], seed));
    }

    acc1 = acc1.wrapping_add(mix_16bytes(data, secret, seed));
    acc2 = acc2.wrapping_add(mix_16bytes(&data[len - 16..], &secret[16..], seed));

    let low64 = acc1.wrapping_add(acc2);
    let high64 = acc1
        .wrapping_mul(PRIME64_1)
        .wrapping_add(acc2.wrapping_mul(PRIME64_4))
        .wrapping_add((len as u64).wrapping_sub(seed).wrapping_mul(PRIME64_2));

    u128::from(avalanche(low64)) + (u128::from(0u64.wrapping_sub(avalanche(high64))) << 64)
}

#[inline(always)]
fn hash_len_129to240_128bits(data: &[u8], len: usize, secret: &[u8], seed: u64) -> u128 {
    debug_assert!((129..=MIDSIZE_MAX).contains(&len));
    debug_assert!(secret.len() >= SECRET_SIZE_MIN);

    let acc1 = (len as u64).wrapping_mul(PRIME64_1);
    let acc2 = 0u64;

    let (acc1, acc2) = (0..4).fold((acc1, acc2), |(acc1, acc2), i| {
        (
            acc1.wrapping_add(mix_16bytes(&data[32 * i..], &secret[32 * i..], seed)),
            acc2.wrapping_add(mix_16bytes(
                &data[32 * i + 16..],
                &secret[32 * i + 16..],
                0u64.wrapping_sub(seed),
            )),
        )
    });
    let acc1 = avalanche(acc1);
    let acc2 = avalanche(acc2);

    let nb_rounds = len / 32;
    debug_assert!(nb_rounds >= 4);

    let (acc1, acc2) = (4..nb_rounds).fold((acc1, acc2), |(acc1, acc2), i| {
        (
            acc1.wrapping_add(mix_16bytes(
                &data[32 * i..],
                &secret[32 * (i - 4) + MIDSIZE_STARTOFFSET..],
                seed,
            )),
            acc2.wrapping_add(mix_16bytes(
                &data[32 * i + 16..],
                &secret[32 * (i - 4) + 16 + MIDSIZE_STARTOFFSET..],
                0u64.wrapping_sub(seed),
            )),
        )
    });

    // last bytes
    let acc1 = acc1.wrapping_add(mix_16bytes(
        &data[len - 16..],
        &secret[SECRET_SIZE_MIN - MIDSIZE_LASTOFFSET..],
        seed,
    ));
    let acc2 = acc2.wrapping_add(mix_16bytes(
        &data[len - 32..],
        &secret[SECRET_SIZE_MIN - MIDSIZE_LASTOFFSET - 16..],
        0u64.wrapping_sub(seed),
    ));

    let low64 = acc1.wrapping_add(acc2);
    let high64 = acc1
        .wrapping_mul(PRIME64_1)
        .wrapping_add(acc2.wrapping_mul(PRIME64_4))
        .wrapping_add((len as u64).wrapping_sub(seed).wrapping_mul(PRIME64_2));

    u128::from(avalanche(low64)) + (u128::from(0u64.wrapping_sub(avalanche(high64))) << 64)
}

#[inline]
fn hash_long_128bits_with_default_secret(data: &[u8], len: usize) -> u128 {
    hash_long_128bits_internal(data, len, &SECRET)
}

#[inline]
fn hash_long_128bits_with_secret(data: &[u8], len: usize, secret: &[u8]) -> u128 {
    hash_long_128bits_internal(data, len, secret)
}

#[inline]
fn hash_long_128bits_with_seed(data: &[u8], len: usize, seed: u64) -> u128 {
    if seed == 0 {
        hash_long_128bits_with_default_secret(data, len)
    } else {
        let secret = Secret::with_seed(seed);

        hash_long_128bits_internal(data, len, &secret)
    }
}

#[inline(always)]
fn hash_long_128bits_internal(data: &[u8], len: usize, secret: &[u8]) -> u128 {
    let mut acc = Acc::default();

    hash_long_internal_loop(&mut acc, data, len, secret, AccWidth::Acc128Bits);

    debug_assert!(secret.len() >= acc.len() + SECRET_MERGEACCS_START);

    let low64 = merge_accs(
        &acc,
        &secret[SECRET_MERGEACCS_START..],
        (len as u64).wrapping_mul(PRIME64_1),
    );
    let high64 = merge_accs(
        &acc,
        &secret[secret.len() - ACC_SIZE - SECRET_MERGEACCS_START..],
        !(len as u64).wrapping_mul(PRIME64_2),
    );

    u128::from(low64) + (u128::from(high64) << 64)
}

/* ===   XXH3 128-bit streaming   === */

/* all the functions are actually the same as for 64-bit streaming variant,
just the reset one is different (different initial acc values for 0,5,6,7),
and near the end of the digest function */

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::*;

    const PRIME: u64 = 2654435761;
    const PRIME64: u64 = 11400714785074694797;
    const SANITY_BUFFER_SIZE: usize = 2243;

    fn sanity_buffer() -> [u8; SANITY_BUFFER_SIZE] {
        let mut buf = [0; SANITY_BUFFER_SIZE];
        let mut byte_gen: u64 = PRIME;

        for b in buf.iter_mut() {
            *b = (byte_gen >> 56) as u8;
            byte_gen = byte_gen.wrapping_mul(PRIME64);
        }

        buf
    }

    #[test]
    fn hash_64bits_sanity_check() {
        let buf = sanity_buffer();

        let test_cases = vec![
            (&[][..], 0, 0), /* zero-length hash is always 0 */
            (&[][..], PRIME64, 0),
            (&buf[..1], 0, 0x7198D737CFE7F386),       /*  1 -  3 */
            (&buf[..1], PRIME64, 0xB70252DB7161C2BD), /*  1 -  3 */
            (&buf[..6], 0, 0x22CBF5F3E1F6257C),       /*  4 -  8 */
            (&buf[..6], PRIME64, 0x6398631C12AB94CE), /*  4 -  8 */
            (&buf[..12], 0, 0xD5361CCEEBB5A0CC),      /*  9 - 16 */
            (&buf[..12], PRIME64, 0xC4C125E75A808C3D), /*  9 - 16 */
            (&buf[..24], 0, 0x46796F3F78B20F6B),      /* 17 - 32 */
            (&buf[..24], PRIME64, 0x60171A7CD0A44C10), /* 17 - 32 */
            (&buf[..48], 0, 0xD8D4D3590D136E11),      /* 33 - 64 */
            (&buf[..48], PRIME64, 0x05441F2AEC2A1296), /* 33 - 64 */
            (&buf[..80], 0, 0xA1DC8ADB3145B86A),      /* 65 - 96 */
            (&buf[..80], PRIME64, 0xC9D55256965B7093), /* 65 - 96 */
            (&buf[..112], 0, 0xE43E5717A61D3759),     /* 97 -128 */
            (&buf[..112], PRIME64, 0x5A5F89A3FECE44A5), /* 97 -128 */
            (&buf[..195], 0, 0x6F747739CBAC22A5),     /* 129-240 */
            (&buf[..195], PRIME64, 0x33368E23C7F95810), /* 129-240 */
            (&buf[..403], 0, 0x4834389B15D981E8),     /* one block, last stripe is overlapping */
            (&buf[..403], PRIME64, 0x85CE5DFFC7B07C87), /* one block, last stripe is overlapping */
            (&buf[..512], 0, 0x6A1B982631F059A8),     /* one block, finishing at stripe boundary */
            (&buf[..512], PRIME64, 0x10086868CF0ADC99), /* one block, finishing at stripe boundary */
            (&buf[..2048], 0, 0xEFEFD4449323CDD4),      /* 2 blocks, finishing at block boundary */
            (&buf[..2048], PRIME64, 0x01C85E405ECA3F6E), /* 2 blocks, finishing at block boundary */
            (&buf[..2240], 0, 0x998C0437486672C7),      /* 3 blocks, finishing at stripe boundary */
            (&buf[..2240], PRIME64, 0x4ED38056B87ABC7F), /* 3 blocks, finishing at stripe boundary */
            (&buf[..2243], 0, 0xA559D20581D742D3),       /* 3 blocks, last stripe is overlapping */
            (&buf[..2243], PRIME64, 0x96E051AB57F21FC8), /* 3 blocks, last stripe is overlapping */
        ];

        for (buf, seed, result) in test_cases {
            {
                let hash = hash64_with_seed(buf, seed);

                assert_eq!(
                    hash,
                    result,
                    "hash64_with_seed(&buf[..{}], seed={}) failed, got 0x{:X}, expected 0x{:X}",
                    buf.len(),
                    seed,
                    hash,
                    result
                );
            }

            // streaming API test

            // single ingestio
            {
                let mut hasher = Hash64::with_seed(seed);
                hasher.write(buf);
                let hash = hasher.finish();

                assert_eq!(
                    hash,
                    result,
                    "Hash64::update(&buf[..{}]) with seed={} failed, got 0x{:X}, expected 0x{:X}",
                    buf.len(),
                    seed,
                    hash,
                    result
                );
            }

            if buf.len() > 3 {
                // 2 ingestions
                let mut hasher = Hash64::with_seed(seed);
                hasher.write(&buf[..3]);
                hasher.write(&buf[3..]);
                let hash = hasher.finish();

                assert_eq!(
                    hash,
                    result,
                    "Hash64::update(&buf[..3], &buf[3..{}]) with seed={} failed, got 0x{:X}, expected 0x{:X}",
                    buf.len(),
                    seed,
                    hash,
                    result
                );
            }

            // byte by byte ingestion
            {
                let mut hasher = Hash64::with_seed(seed);

                for chunk in buf.chunks(1) {
                    hasher.write(chunk);
                }

                let hash = hasher.finish();

                assert_eq!(
                    hash,
                    result,
                    "Hash64::update(&buf[..{}].chunks(1)) with seed={} failed, got 0x{:X}, expected 0x{:X}",
                    buf.len(),
                    seed,
                    hash,
                    result
                );
            }
        }
    }

    #[test]
    fn hash_64bits_with_secret_sanity_check() {
        let buf = sanity_buffer();
        let secret = &buf[7..7 + SECRET_SIZE_MIN + 11];

        let test_cases = vec![
            (&[][..], secret, 0),                       /* zero-length hash is always 0 */
            (&buf[..1], secret, 0x7F69735D618DB3F0),    /*  1 -  3 */
            (&buf[..6], secret, 0xBFCC7CB1B3554DCE),    /*  6 -  8 */
            (&buf[..12], secret, 0x8C50DC90AC9206FC),   /*  9 - 16 */
            (&buf[..24], secret, 0x1CD2C2EE9B9A0928),   /* 17 - 32 */
            (&buf[..48], secret, 0xA785256D9D65D514),   /* 33 - 64 */
            (&buf[..80], secret, 0x6F3053360D21BBB7),   /* 65 - 96 */
            (&buf[..112], secret, 0x560E82D25684154C),  /* 97 -128 */
            (&buf[..195], secret, 0xBA5BDDBC5A767B11),  /* 129-240 */
            (&buf[..403], secret, 0xFC3911BBA656DB58),  /* one block, last stripe is overlapping */
            (&buf[..512], secret, 0x306137DD875741F1), /* one block, finishing at stripe boundary */
            (&buf[..2048], secret, 0x2836B83880AD3C0C), /* > one block, at least one scrambling */
            (&buf[..2243], secret, 0x3446E248A00CB44A), /* > one block, at least one scrambling, last stripe unaligned */
        ];

        for (buf, secret, result) in test_cases {
            {
                let hash = hash64_with_secret(buf, secret);

                assert_eq!(
                    hash,
                    result,
                    "hash64_with_secret(&buf[..{}], secret) failed, got 0x{:X}, expected 0x{:X}",
                    buf.len(),
                    hash,
                    result
                );
            }

            // streaming API test

            // single ingestio
            {
                let mut hasher = Hash64::with_secret(secret);
                hasher.write(buf);
                let hash = hasher.finish();

                assert_eq!(
                    hash,
                    result,
                    "Hash64::update(&buf[..{}]) with secret failed, got 0x{:X}, expected 0x{:X}",
                    buf.len(),
                    hash,
                    result
                );
            }

            // byte by byte ingestion
            {
                let mut hasher = Hash64::with_secret(secret);

                for chunk in buf.chunks(1) {
                    hasher.write(chunk);
                }

                let hash = hasher.finish();

                assert_eq!(
                    hash,
                    result,
                    "Hash64::update(&buf[..{}].chunks(1)) with secret failed, got 0x{:X}, expected 0x{:X}",
                    buf.len(),
                    hash,
                    result
                );
            }
        }
    }

    #[test]
    fn hash_128bits_sanity_check() {
        let buf = sanity_buffer();

        let test_cases = vec![
            (&[][..], 0, 0u64, 0u64), /* zero-length hash is { seed, -seed } by default */
            (&[][..], PRIME, 0, 0),
            (&buf[..1], 0, 0x7198D737CFE7F386, 0x3EE70EA338F3F1E8), /* 1-3 */
            (&buf[..1], PRIME, 0x8E05996EC27C0F46, 0x90DFC659A8BDCC0C), /* 1-3 */
            (&buf[..6], 0, 0x22CBF5F3E1F6257C, 0xD4E6C2B94FFC3BFA), /* 4-8 */
            (&buf[..6], PRIME, 0x97B28D3079F8541F, 0xEFC0B954298E6555), /* 4-8 */
            (&buf[..12], 0, 0x0E0CD01F05AC2F0D, 0x2B55C95951070D4B), /* 9-16 */
            (&buf[..12], PRIME, 0xA9DE561CA04CDF37, 0x609E31FDC00A43C9), /* 9-16 */
            (&buf[..24], 0, 0x46796F3F78B20F6B, 0x58FF55C3926C13FA), /* 17-32 */
            (&buf[..24], PRIME, 0x30D5C4E9EB415C55, 0x8868344B3A4645D0), /* 17-32 */
            (&buf[..48], 0, 0xD8D4D3590D136E11, 0x5527A42843020A62), /* 33-64 */
            (&buf[..48], PRIME, 0x1D8834E1A5407A1C, 0x44375B9FB060F541), /* 33-64 */
            (&buf[..81], 0, 0x4B9B448ED8DFD3DD, 0xE805A6D1A43D70E5), /* 65-96 */
            (&buf[..81], PRIME, 0xD2D6B075945617BA, 0xE58BE5736F6E7550), /* 65-96 */
            (&buf[..103], 0, 0xC5A9F97B29EFA44E, 0x254DB7BE881E125C), /* 97-128 */
            (&buf[..103], PRIME, 0xFA2086367CDB177F, 0x0AEDEA68C988B0C0), /* 97-128 */
            (&buf[..192], 0, 0xC3142FDDD9102A3F, 0x06F1747E77185F97), /* 129-240 */
            (&buf[..192], PRIME, 0xA89F07B35987540F, 0xCF1B35FB2C557F54), /* 129-240 */
            (&buf[..222], 0, 0xA61AC4EB3295F86B, 0x33FA7B7598C28A07), /* 129-240 */
            (&buf[..222], PRIME, 0x54135EB88AD8B75E, 0xBC45CE6AE50BCF53), /* 129-240 */
            (&buf[..403], 0, 0xB0C48E6D18E9D084, 0xB16FC17E992FF45D), /* one block, last stripe is overlapping */
            (&buf[..403], PRIME64, 0x0A1D320C9520871D, 0xCE11CB376EC93252), /* one block, last stripe is overlapping */
            (&buf[..512], 0, 0xA03428558AC97327, 0x4ECF51281BA406F7), /* one block, finishing at stripe boundary */
            (&buf[..512], PRIME64, 0xAF67A482D6C893F2, 0x1382D92F25B84D90), /* one block, finishing at stripe boundary */
            (&buf[..2048], 0, 0x21901B416B3B9863, 0x212AF8E6326F01E0), /* two blocks, finishing at block boundary */
            (&buf[..2048], PRIME, 0xBDBB2282577DADEC, 0xF78CDDC2C9A9A692), /* two blocks, finishing at block boundary */
            (&buf[..2240], 0, 0x00AD52FA9385B6FE, 0xC705BAD3356CE302), /* two blocks, ends at stripe boundary */
            (&buf[..2240], PRIME, 0x10FD0072EC68BFAA, 0xE1312F3458817F15), /* two blocks, ends at stripe boundary */
            (&buf[..2237], 0, 0x970C91411533862C, 0x4BBD06FF7BFF0AB1), /* two blocks, ends at stripe boundary */
            (&buf[..2237], PRIME, 0xD80282846D814431, 0x14EBB157B84D9785), /* two blocks, ends at stripe boundary */
        ];

        for (buf, seed, lo, hi) in test_cases {
            let result = u128::from(lo) + (u128::from(hi) << 64);

            {
                let hash = hash128_with_seed(buf, seed);

                assert_eq!(
                    hash,
                    result,
                    "hash128_with_seed(&buf[..{}], seed={}) failed, got 0x{:X}, expected 0x{:X}",
                    buf.len(),
                    seed,
                    hash,
                    result
                );
            }

            // streaming API test

            // single ingestio
            {
                let mut hasher = Hash128::with_seed(seed);
                hasher.write(buf);
                let hash = hasher.finish_ext();

                assert_eq!(
                    hash,
                    result,
                    "Hash128::update(&buf[..{}]) with seed={} failed, got 0x{:X}, expected 0x{:X}",
                    buf.len(),
                    seed,
                    hash,
                    result
                );
            }

            if buf.len() > 3 {
                // 2 ingestions
                let mut hasher = Hash128::with_seed(seed);
                hasher.write(&buf[..3]);
                hasher.write(&buf[3..]);
                let hash = hasher.finish_ext();

                assert_eq!(
                    hash,
                    result,
                    "Hash64::update(&buf[..3], &buf[3..{}]) with seed={} failed, got 0x{:X}, expected 0x{:X}",
                    buf.len(),
                    seed,
                    hash,
                    result
                );
            }

            // byte by byte ingestion
            {
                let mut hasher = Hash128::with_seed(seed);

                for chunk in buf.chunks(1) {
                    hasher.write(chunk);
                }

                let hash = hasher.finish_ext();

                assert_eq!(
                    hash,
                    result,
                    "Hash64::update(&buf[..{}].chunks(1)) with seed={} failed, got 0x{:X}, expected 0x{:X}",
                    buf.len(),
                    seed,
                    hash,
                    result
                );
            }
        }
    }
}
