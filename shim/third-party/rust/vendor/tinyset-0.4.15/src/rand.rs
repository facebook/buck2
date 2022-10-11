#[cfg(not(feature = "rand"))]
use std::sync::atomic::{AtomicU64, Ordering};

#[cfg(not(feature = "rand"))]
static SEED: AtomicU64 = AtomicU64::new(0);

#[cfg(not(feature = "rand"))]
pub fn rand32() -> u32 {
    rand64() as u32
}

#[cfg(feature = "rand")]
pub fn rand32() -> u32 {
    rand::random::<u32>()
}

#[cfg(not(feature = "rand"))]
pub fn rand64() -> u64 {
    use std::num::Wrapping;
    // This is the SplitMix64 algorithm.  It's pretty crude,
    // but should actually be good enough in most cases.
    let z = Wrapping(SEED.fetch_add(0x9e3779b97f4a7c15, Ordering::Relaxed));
    if z == Wrapping(0) {
        let seed = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .subsec_nanos();
        SEED.store(seed as u64, Ordering::Relaxed);
        return rand64();
    }
    let z = (z ^ (z >> 30)) * Wrapping(0xbf58476d1ce4e5b9);
    let z = (z ^ (z >> 27)) * Wrapping(0x94d049bb133111eb);
    (z ^ (z >> 31)).0
}

#[cfg(feature = "rand")]
pub fn rand64() -> u64 {
    rand::random::<u64>()
}

#[cfg(not(feature = "rand"))]
pub fn rand_usize() -> usize {
    rand64() as usize
}

#[cfg(feature = "rand")]
pub fn rand_usize() -> usize {
    rand::random::<usize>()
}
