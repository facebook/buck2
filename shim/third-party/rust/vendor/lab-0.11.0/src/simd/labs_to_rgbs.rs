use crate::simd::math::powf256_ps;
use crate::{Lab, CBRT_EPSILON, EPSILON, KAPPA, S_0};
use std::arch::x86_64::*;
use std::{f32, iter, mem};

static BLANK_LAB: Lab = Lab {
    l: f32::NAN,
    a: f32::NAN,
    b: f32::NAN,
};

pub fn labs_to_rgbs(labs: &[Lab]) -> Vec<[u8; 3]> {
    let chunks = labs.chunks_exact(8);
    let remainder = chunks.remainder();
    let mut vs = chunks.fold(Vec::with_capacity(labs.len()), |mut v, labs| {
        let rgbs = unsafe { slice_labs_to_slice_rgbs(labs) };
        v.extend_from_slice(&rgbs);
        v
    });

    // While we could simplify this block by just calling the scalar version
    // of the code on the remainder, there are some variations between scalar
    // and SIMD floating point math (especially on TravisCI for some reason?)
    // and I don't want the trailing N items to be computed by a different
    // algorithm.
    if remainder.len() > 0 {
        let labs: Vec<Lab> = remainder
            .iter()
            .cloned()
            .chain(iter::repeat(BLANK_LAB))
            .take(8)
            .collect();

        let rgbs = unsafe { slice_labs_to_slice_rgbs(&labs) };
        vs.extend_from_slice(&rgbs[..remainder.len()]);
    }

    vs
}

pub fn labs_to_rgb_bytes(labs: &[Lab]) -> Vec<u8> {
    let chunks = labs.chunks_exact(8);
    let remainder = chunks.remainder();
    let mut vs = chunks.fold(Vec::with_capacity(labs.len()), |mut v, labs| {
        let bytes = unsafe { slice_labs_to_rgb_bytes(labs) };
        v.extend_from_slice(&bytes);
        v
    });

    if remainder.len() > 0 {
        let labs: Vec<Lab> = remainder
            .iter()
            .cloned()
            .chain(iter::repeat(BLANK_LAB))
            .take(8)
            .collect();

        let bytes = unsafe { slice_labs_to_rgb_bytes(&labs) };
        vs.extend_from_slice(&bytes[..remainder.len() * 3]);
    }

    vs
}

#[allow(dead_code)]
pub fn labs_to_rgbs_chunk(labs: &[Lab]) -> [[u8; 3]; 8] {
    unsafe { slice_labs_to_slice_rgbs(labs) }
}

#[inline]
unsafe fn slice_labs_to_slice_rgbs(labs: &[Lab]) -> [[u8; 3]; 8] {
    let (l, a, b) = lab_slice_to_simd(labs);
    let (x, y, z) = labs_to_xyzs(l, a, b);
    let (r, g, b) = xyzs_to_rgbs(x, y, z);
    simd_to_rgb_array(r, g, b)
}

#[inline]
unsafe fn slice_labs_to_rgb_bytes(labs: &[Lab]) -> [u8; 8 * 3] {
    let (l, a, b) = lab_slice_to_simd(labs);
    let (x, y, z) = labs_to_xyzs(l, a, b);
    let (r, g, b) = xyzs_to_rgbs(x, y, z);
    simd_to_rgb_bytes(r, g, b)
}

#[inline]
unsafe fn lab_slice_to_simd(labs: &[Lab]) -> (__m256, __m256, __m256) {
    let labs = &labs[..8];
    let l = _mm256_set_ps(
        labs[0].l, labs[1].l, labs[2].l, labs[3].l, labs[4].l, labs[5].l, labs[6].l, labs[7].l,
    );
    let a = _mm256_set_ps(
        labs[0].a, labs[1].a, labs[2].a, labs[3].a, labs[4].a, labs[5].a, labs[6].a, labs[7].a,
    );
    let b = _mm256_set_ps(
        labs[0].b, labs[1].b, labs[2].b, labs[3].b, labs[4].b, labs[5].b, labs[6].b, labs[7].b,
    );
    (l, a, b)
}

#[inline]
unsafe fn labs_to_xyzs(l: __m256, a: __m256, b: __m256) -> (__m256, __m256, __m256) {
    let fy = _mm256_div_ps(
        _mm256_add_ps(l, _mm256_set1_ps(16.0)),
        _mm256_set1_ps(116.0),
    );
    let fx = _mm256_add_ps(_mm256_div_ps(a, _mm256_set1_ps(500.0)), fy);
    let fz = _mm256_sub_ps(fy, _mm256_div_ps(b, _mm256_set1_ps(200.0)));

    let xr = {
        let false_branch = {
            let temp1 = _mm256_mul_ps(fx, _mm256_set1_ps(116.0));
            let temp2 = _mm256_sub_ps(temp1, _mm256_set1_ps(16.0));
            _mm256_div_ps(temp2, _mm256_set1_ps(KAPPA))
        };
        let unpacked_false_branch: [f32; 8] = mem::transmute(false_branch);
        let mut unpacked: [f32; 8] = mem::transmute(fx);
        for (i, el) in unpacked.iter_mut().enumerate() {
            if *el > CBRT_EPSILON {
                *el = el.powi(3);
            } else {
                *el = unpacked_false_branch[i];
            }
        }
        mem::transmute(unpacked)
    };

    let yr = {
        let false_branch = _mm256_div_ps(l, _mm256_set1_ps(KAPPA));
        let unpacked_false_branch: [f32; 8] = mem::transmute(false_branch);
        let unpacked_fy: [f32; 8] = mem::transmute(fy);
        let mut unpacked: [f32; 8] = mem::transmute(l);
        for (i, el) in unpacked.iter_mut().enumerate() {
            if *el > EPSILON * KAPPA {
                *el = unpacked_fy[i].powi(3);
            } else {
                *el = unpacked_false_branch[i];
            }
        }
        mem::transmute(unpacked)
    };

    let zr = {
        let false_branch = {
            let temp1 = _mm256_mul_ps(fz, _mm256_set1_ps(116.0));
            let temp2 = _mm256_sub_ps(temp1, _mm256_set1_ps(16.0));
            _mm256_div_ps(temp2, _mm256_set1_ps(KAPPA))
        };
        let unpacked_false_branch: [f32; 8] = mem::transmute(false_branch);
        let mut unpacked: [f32; 8] = mem::transmute(fz);
        for (i, el) in unpacked.iter_mut().enumerate() {
            if *el > CBRT_EPSILON {
                *el = el.powi(3);
            } else {
                *el = unpacked_false_branch[i];
            }
        }
        mem::transmute(unpacked)
    };

    (
        _mm256_mul_ps(xr, _mm256_set1_ps(0.9504492182750991)),
        yr,
        _mm256_mul_ps(zr, _mm256_set1_ps(1.0889166484304715)),
    )
}

#[inline]
unsafe fn xyzs_to_rgbs(x: __m256, y: __m256, z: __m256) -> (__m256, __m256, __m256) {
    let r = {
        let prod_x = _mm256_mul_ps(x, _mm256_set1_ps(3.240812398895283));
        let prod_y = _mm256_mul_ps(y, _mm256_set1_ps(-1.5373084456298136));
        let prod_z = _mm256_mul_ps(z, _mm256_set1_ps(-0.4985865229069666));
        let sum = _mm256_add_ps(_mm256_add_ps(prod_x, prod_y), prod_z);
        xyzs_to_rgbs_map(sum)
    };
    let g = {
        let prod_x = _mm256_mul_ps(x, _mm256_set1_ps(-0.9692430170086407));
        let prod_y = _mm256_mul_ps(y, _mm256_set1_ps(1.8759663029085742));
        let prod_z = _mm256_mul_ps(z, _mm256_set1_ps(0.04155503085668564));
        let sum = _mm256_add_ps(_mm256_add_ps(prod_x, prod_y), prod_z);
        xyzs_to_rgbs_map(sum)
    };
    let b = {
        let prod_x = _mm256_mul_ps(x, _mm256_set1_ps(0.055638398436112804));
        let prod_y = _mm256_mul_ps(y, _mm256_set1_ps(-0.20400746093241362));
        let prod_z = _mm256_mul_ps(z, _mm256_set1_ps(1.0571295702861434));
        let sum = _mm256_add_ps(_mm256_add_ps(prod_x, prod_y), prod_z);
        xyzs_to_rgbs_map(sum)
    };

    (r, g, b)
}

#[inline]
unsafe fn xyzs_to_rgbs_map(c: __m256) -> __m256 {
    let mask = _mm256_cmp_ps(c, _mm256_set1_ps(S_0), _CMP_GT_OQ);
    let false_branch = _mm256_mul_ps(c, _mm256_set1_ps(12.92));
    let true_branch = {
        let raised = powf256_ps(c, _mm256_set1_ps(1.0 / 2.4));
        let temp2 = _mm256_mul_ps(raised, _mm256_set1_ps(1.055));
        _mm256_sub_ps(temp2, _mm256_set1_ps(0.055))
    };
    let blended = _mm256_blendv_ps(false_branch, true_branch, mask);
    _mm256_mul_ps(blended, _mm256_set1_ps(255.0))
}

#[inline]
unsafe fn simd_to_rgb_array(r: __m256, g: __m256, b: __m256) -> [[u8; 3]; 8] {
    let r: [f32; 8] = mem::transmute(_mm256_round_ps(r, _MM_FROUND_TO_NEAREST_INT));
    let g: [f32; 8] = mem::transmute(_mm256_round_ps(g, _MM_FROUND_TO_NEAREST_INT));
    let b: [f32; 8] = mem::transmute(_mm256_round_ps(b, _MM_FROUND_TO_NEAREST_INT));

    let mut rgbs: [mem::MaybeUninit<[u8; 3]>; 8] = mem::MaybeUninit::uninit().assume_init();
    for (((&r, &g), &b), rgb) in r
        .iter()
        .zip(g.iter())
        .zip(b.iter())
        .rev()
        .zip(rgbs.iter_mut())
    {
        *rgb = mem::MaybeUninit::new([r as u8, g as u8, b as u8]);
    }
    mem::transmute(rgbs)
}

#[inline]
unsafe fn simd_to_rgb_bytes(r: __m256, g: __m256, b: __m256) -> [u8; 8 * 3] {
    let r: [f32; 8] = mem::transmute(_mm256_round_ps(r, _MM_FROUND_TO_NEAREST_INT));
    let g: [f32; 8] = mem::transmute(_mm256_round_ps(g, _MM_FROUND_TO_NEAREST_INT));
    let b: [f32; 8] = mem::transmute(_mm256_round_ps(b, _MM_FROUND_TO_NEAREST_INT));

    let mut bytes: [mem::MaybeUninit<u8>; 8 * 3] = mem::MaybeUninit::uninit().assume_init();
    for (((&r, &g), &b), rgb) in r
        .iter()
        .zip(g.iter())
        .zip(b.iter())
        .rev()
        .zip(bytes.chunks_exact_mut(3))
    {
        rgb[0] = mem::MaybeUninit::new(r as u8);
        rgb[1] = mem::MaybeUninit::new(g as u8);
        rgb[2] = mem::MaybeUninit::new(b as u8);
    }
    mem::transmute(bytes)
}

#[cfg(test)]
mod test {
    use crate::{labs_to_rgbs, simd, Lab};
    use lazy_static::lazy_static;
    use rand;
    use rand::distributions::Standard;
    use rand::Rng;

    lazy_static! {
        static ref RGBS: Vec<[u8; 3]> = {
            let rand_seed = [0u8; 32];
            let rng: rand::rngs::StdRng = rand::SeedableRng::from_seed(rand_seed);
            rng.sample_iter(&Standard).take(512).collect()
        };
    }

    #[test]
    fn test_simd_labs_to_rgbs() {
        let labs = simd::rgbs_to_labs(&RGBS);
        let rgbs = simd::labs_to_rgbs(&labs);
        assert_eq!(rgbs.as_slice(), RGBS.as_slice());
    }

    #[test]
    fn test_simd_labs_to_rgb_bytes() {
        // Assert that returning a single slice of bytes returns the same values as
        // returning them in rgb triples.
        #[rustfmt::skip]
        let labs = vec![
            Lab { l: 65.55042, a: 64.48197, b: -11.685503 },
            Lab { l: 48.25341, a: 74.3235, b: 62.362576 },
            Lab { l: 69.485344, a: 36.825745, b: 75.4871 },
            Lab { l: 93.01275, a: -13.856977, b: 91.47719 },
            Lab { l: 46.70882, a: -50.29225, b: 42.086266 },
            Lab { l: 70.86147, a: -38.99568, b: -11.459422 },
            Lab { l: 40.166237, a: 55.847153, b: -94.75334 },
            Lab { l: 28.53371, a: 58.779716, b: -44.23661 },
        ];
        let rgbs = simd::labs_to_rgbs(&labs).iter().fold(
            Vec::with_capacity(labs.len() * 3),
            |mut acc, rgb| {
                acc.extend_from_slice(rgb);
                acc
            },
        );
        let bytes = simd::labs_to_rgb_bytes(&labs);
        assert_eq!(rgbs, bytes);
    }

    #[test]
    fn test_simd_labs_to_rgbs_unsaturated() {
        let labs = vec![Lab {
            l: 66.6348,
            a: 52.260696,
            b: 14.850557,
        }];
        let rgbs_non_simd = labs_to_rgbs(&labs);
        let rgbs_simd = simd::labs_to_rgbs(&labs);
        assert_eq!(rgbs_simd, rgbs_non_simd);
    }
}
