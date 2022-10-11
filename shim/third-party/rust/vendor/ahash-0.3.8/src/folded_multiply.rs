use crate::convert::*;
use core::ops::Add;
use core::ops::Mul;

pub(crate) trait FoldedMultiply: Mul + Add + Sized {
    fn folded_multiply(self, by: Self) -> Self;
}

impl FoldedMultiply for u64 {
    #[inline(always)]
    fn folded_multiply(self, by: u64) -> u64 {
        let result: [u64; 2] = (self as u128).wrapping_mul(by as u128).convert();
        result[0].wrapping_add(result[1])
    }
}

#[cfg(all(any(target_arch = "x86", target_arch = "x86_64"), target_feature = "sse2", not(miri)))]
#[inline(always)]
pub(crate) fn add_by_64s(a: [u64; 2], b: [u64; 2]) -> [u64; 2] {
    use core::mem::transmute;
    unsafe {
        #[cfg(target_arch = "x86")]
        use core::arch::x86::*;
        #[cfg(target_arch = "x86_64")]
        use core::arch::x86_64::*;
        transmute(_mm_add_epi64(transmute(a), transmute(b)))
    }
}

#[cfg(not(all(any(target_arch = "x86", target_arch = "x86_64"), target_feature = "sse2", not(miri))))]
#[inline(always)]
pub(crate) fn add_by_64s(a: [u64; 2], b: [u64; 2]) -> [u64; 2] {
    [a[0].wrapping_add(b[0]), a[1].wrapping_add(b[1])]
}
