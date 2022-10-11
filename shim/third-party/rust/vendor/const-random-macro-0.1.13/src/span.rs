use proc_macro::Span;
use std::option_env;

use lazy_static::lazy_static;
use tiny_keccak::{Hasher, Sha3};

lazy_static! {
    static ref SEED: Vec<u8> = {
        if let Some(value) = option_env!("CONST_RANDOM_SEED") {
            value.as_bytes().to_vec()
        } else {
            let mut value = [0u8; 32];
            getrandom::getrandom(&mut value).unwrap();
            value.to_vec()
        }
    };
}

pub(crate) fn gen_random<T: Random>() -> T {
    Random::random()
}

pub(crate) trait Random {
    fn random() -> Self;
}

fn hash_stuff() -> impl Hasher {
    let span = Span::call_site();
    let mut hasher = Sha3::v256();
    hasher.update(&*SEED);
    hasher.update(&format!("{:?}", span).as_bytes());
    hasher
}

impl Random for u64 {
    fn random() -> Self {
        let mut output = [0; 8];
        hash_stuff().finalize(&mut output);
        Self::from_ne_bytes(output)
    }
}

impl Random for u128 {
    fn random() -> Self {
        let mut output = [0; 16];
        hash_stuff().finalize(&mut output);
        Self::from_ne_bytes(output)
    }
}

impl Random for u8 {
    fn random() -> Self {
        u64::random() as u8
    }
}

impl Random for u16 {
    fn random() -> Self {
        u64::random() as u16
    }
}

impl Random for u32 {
    fn random() -> Self {
        u64::random() as u32
    }
}

impl Random for i8 {
    fn random() -> Self {
        i64::random() as i8
    }
}

impl Random for i16 {
    fn random() -> Self {
        i64::random() as i16
    }
}

impl Random for i32 {
    fn random() -> Self {
        i64::random() as i32
    }
}

impl Random for i64 {
    fn random() -> Self {
        u64::random() as i64
    }
}

impl Random for i128 {
    fn random() -> Self {
        u128::random() as i128
    }
}
