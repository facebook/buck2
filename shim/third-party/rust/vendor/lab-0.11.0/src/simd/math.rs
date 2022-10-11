/*
An almost-literal transliteration of AVX-optimized sin(), cos(), exp() and log()
functions by Giovanni Garberoglio, available at http://software-lisc.fbk.eu/avx_mathfun/

Copyright (C) 2012 Giovanni Garberoglio
Interdisciplinary Laboratory for Computational Science (LISC)
Fondazione Bruno Kessler and University of Trento
via Sommarive, 18
I-38123 Trento (Italy)

which was itself a translation of Simple SSE and SSE2 optimized sin, cos, log,
and exp by Julien Pommier, available at http://gruntthepeon.free.fr/ssemath/

Copyright (C) 2007  Julien Pommier

Both are provided under the zlib license:

This software is provided 'as-is', without any express or implied
warranty.  In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
 claim that you wrote the original software. If you use this software
 in a product, an acknowledgment in the product documentation would be
 appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be
 misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.

*/

use std::arch::x86_64::*;
use std::mem;

static X7F: i32 = 0x7f;
static SQRTHF: f32 = 0.70710678118654752;
static LOG_P0: f32 = 7.0376836292E-2;
static LOG_P1: f32 = -1.1514610310E-1;
static LOG_P2: f32 = 1.1676998740E-1;
static LOG_P3: f32 = -1.2420140846E-1;
static LOG_P4: f32 = 1.4249322787E-1;
static LOG_P5: f32 = -1.6668057665E-1;
static LOG_P6: f32 = 2.0000714765E-1;
static LOG_P7: f32 = -2.4999993993E-1;
static LOG_P8: f32 = 3.3333331174E-1;
static LOG_Q1: f32 = -2.12194440e-4;
static LOG_Q2: f32 = 0.693359375;

pub unsafe fn log256_ps(x: __m256) -> __m256 {
    let one = _mm256_set1_ps(1.0);
    let p5 = _mm256_set1_ps(0.5);

    let invalid_mask = _mm256_cmp_ps(x, _mm256_setzero_ps(), _CMP_LE_OS);

    let _ps256_min_norm_pos: __m256 = mem::transmute(_mm256_set1_epi32(0x00800000));
    let mut x = _mm256_max_ps(x, _ps256_min_norm_pos); /* cut off denormalized stuff */

    let mut imm0 = _mm256_srli_epi32(_mm256_castps_si256(x), 23);

    /* keep only the fractional part */
    let _ps256_inv_mant_mask: __m256 = mem::transmute(_mm256_set1_epi32(!0x7f800000));
    x = _mm256_and_ps(x, _ps256_inv_mant_mask);
    x = _mm256_or_ps(x, p5);

    // this is again another AVX2 instruction
    imm0 = _mm256_sub_epi32(imm0, _mm256_set1_epi32(X7F));
    let mut e = _mm256_cvtepi32_ps(imm0);

    e = _mm256_add_ps(e, one);

    let mask = _mm256_cmp_ps(x, _mm256_set1_ps(SQRTHF), _CMP_LT_OS);
    let mut tmp = _mm256_and_ps(x, mask);
    x = _mm256_sub_ps(x, one);
    e = _mm256_sub_ps(e, _mm256_and_ps(one, mask));
    x = _mm256_add_ps(x, tmp);

    let z = _mm256_mul_ps(x, x);

    let mut y = _mm256_set1_ps(LOG_P0);
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(LOG_P1));
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(LOG_P2));
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(LOG_P3));
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(LOG_P4));
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(LOG_P5));
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(LOG_P6));
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(LOG_P7));
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(LOG_P8));
    y = _mm256_mul_ps(y, x);

    y = _mm256_mul_ps(y, z);

    tmp = _mm256_mul_ps(e, _mm256_set1_ps(LOG_Q1));
    y = _mm256_add_ps(y, tmp);

    tmp = _mm256_mul_ps(z, p5);
    y = _mm256_sub_ps(y, tmp);

    tmp = _mm256_mul_ps(e, _mm256_set1_ps(LOG_Q2));
    x = _mm256_add_ps(x, y);
    x = _mm256_add_ps(x, tmp);
    x = _mm256_or_ps(x, invalid_mask); // negative arg will be NAN
    return x;
}

static EXP_HI: f32 = 88.3762626647949;
static EXP_LO: f32 = -88.3762626647949;
static LOG2EF: f32 = 1.44269504088896341;
static EXP_C1: f32 = 0.693359375;
static EXP_C2: f32 = -2.12194440e-4;
static EXP_P0: f32 = 1.9875691500E-4;
static EXP_P1: f32 = 1.3981999507E-3;
static EXP_P2: f32 = 8.3334519073E-3;
static EXP_P3: f32 = 4.1665795894E-2;
static EXP_P4: f32 = 1.6666665459E-1;
static EXP_P5: f32 = 5.0000001201E-1;

pub unsafe fn exp256_ps(x: __m256) -> __m256 {
    let one = _mm256_set1_ps(1.0);

    let mut x = _mm256_min_ps(x, _mm256_set1_ps(EXP_HI));
    x = _mm256_max_ps(x, _mm256_set1_ps(EXP_LO));

    /* express exp(x) as exp(g + n*log(2)) */
    let mut fx = _mm256_mul_ps(x, _mm256_set1_ps(LOG2EF));
    fx = _mm256_add_ps(fx, _mm256_set1_ps(0.5));

    /* how to perform a floorf with SSE: just below */
    //imm0 = _mm256_cvttps_epi32(fx);
    //tmp  = _mm256_cvtepi32_ps(imm0);

    let mut tmp = _mm256_floor_ps(fx);

    /* if greater, substract 1 */
    //v8sf mask = _mm256_cmpgt_ps(tmp, fx);
    let mut mask = _mm256_cmp_ps(tmp, fx, _CMP_GT_OS);
    mask = _mm256_and_ps(mask, one);
    fx = _mm256_sub_ps(tmp, mask);

    tmp = _mm256_mul_ps(fx, _mm256_set1_ps(EXP_C1));
    let mut z = _mm256_mul_ps(fx, _mm256_set1_ps(EXP_C2));
    x = _mm256_sub_ps(x, tmp);
    x = _mm256_sub_ps(x, z);

    z = _mm256_mul_ps(x, x);

    let mut y = _mm256_set1_ps(EXP_P0);
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(EXP_P1));
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(EXP_P2));
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(EXP_P3));
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(EXP_P4));
    y = _mm256_mul_ps(y, x);
    y = _mm256_add_ps(y, _mm256_set1_ps(EXP_P5));
    y = _mm256_mul_ps(y, z);
    y = _mm256_add_ps(y, x);
    y = _mm256_add_ps(y, one);

    /* build 2^n */
    let mut imm0 = _mm256_cvttps_epi32(fx);
    // another two AVX2 instructions
    imm0 = _mm256_add_epi32(imm0, _mm256_set1_epi32(X7F));
    imm0 = _mm256_slli_epi32(imm0, 23);
    let pow2n = _mm256_castsi256_ps(imm0);
    y = _mm256_mul_ps(y, pow2n);
    return y;
}

pub unsafe fn powf256_ps(x: __m256, y: __m256) -> __m256 {
    let invalid_mask = _mm256_cmp_ps(x, _mm256_setzero_ps(), _CMP_LE_OS);
    let result = exp256_ps(_mm256_mul_ps(y, log256_ps(x)));
    _mm256_or_ps(result, invalid_mask)
}

#[cfg(test)]
mod test {
    use super::{exp256_ps, log256_ps, powf256_ps};
    use approx::assert_relative_eq;
    use std::arch::x86_64::*;
    use std::{f32, mem};

    #[test]
    fn test_log256_ps() {
        let scalar_result: Vec<_> = {
            let vals: [f32; 8] = [0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0];
            vals.iter().copied().map(f32::ln).collect()
        };
        let avx_result: Vec<_> = unsafe {
            let vals = _mm256_set_ps(0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0);
            let result = log256_ps(vals);
            let result: [f32; 8] = mem::transmute(result);
            result.iter().rev().copied().collect()
        };

        assert_relative_eq!(scalar_result.as_slice(), avx_result.as_slice())
    }

    #[test]
    fn test_negative_log_returns_nan() {
        let avx_result: Vec<_> = unsafe {
            let vals = _mm256_set_ps(-0.5, 1.0, -2.0, 3.0, -4.0, 5.0, -6.0, 7.0);
            let result = log256_ps(vals);
            let result: [f32; 8] = mem::transmute(result);
            result.iter().rev().copied().collect()
        };
        assert!(f32::is_nan(avx_result[0]));
        assert!(!f32::is_nan(avx_result[1]));
        assert!(f32::is_nan(avx_result[2]));
        assert!(!f32::is_nan(avx_result[3]));
        assert!(f32::is_nan(avx_result[4]));
        assert!(!f32::is_nan(avx_result[5]));
        assert!(f32::is_nan(avx_result[6]));
        assert!(!f32::is_nan(avx_result[7]));
    }

    #[test]
    fn test_exp256_ps() {
        let scalar_result: Vec<_> = {
            let vals: [f32; 8] = [-1.5, 0.5, 1.0, 2.0, 4.0, 5.0, 6.0, 10.0];
            vals.iter().copied().map(f32::exp).collect()
        };
        let avx_result: Vec<_> = unsafe {
            let vals = _mm256_set_ps(-1.5, 0.5, 1.0, 2.0, 4.0, 5.0, 6.0, 10.0);
            let result = exp256_ps(vals);
            let result: [f32; 8] = mem::transmute(result);
            result.iter().rev().copied().collect()
        };

        assert_relative_eq!(scalar_result.as_slice(), avx_result.as_slice())
    }

    #[test]
    fn test_powf256_ps() {
        let exponent = 4.0;
        let scalar_result: Vec<_> = {
            let vals: [f32; 8] = [0.25, 0.5, 1.0, 2.0, 4.0, 5.0, 6.0, 10.0];
            vals.iter().map(|&n| n.powf(exponent)).collect()
        };
        let avx_result: Vec<_> = unsafe {
            let vals = _mm256_set_ps(0.25, 0.5, 1.0, 2.0, 4.0, 5.0, 6.0, 10.0);
            let three = _mm256_set1_ps(exponent);
            let result = powf256_ps(vals, three);
            let result: [f32; 8] = mem::transmute(result);
            result.iter().rev().copied().collect()
        };

        assert_relative_eq!(scalar_result.as_slice(), avx_result.as_slice())
    }
}
