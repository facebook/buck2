/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// The code in this directory is adapted from `probminhash`
// (https://github.com/jean-pierreBoth/probminhash).

//! implementation of the paper :
//! *SetSkectch : filling the gap between MinHash and HyperLogLog*  
//! See  <https://arxiv.org/abs/2101.00314> or <https://vldb.org/pvldb/vol14/p2244-ertl.pdf>.
//!
//! We implement Setsketch1 algorithm which supposes that the size of the data set
//! to sketch is large compared to the size of sketch.
//! The purpose of this implementation is to provide Local Sensitive sketching of a set
//! adapted to the Jaccard distance with some precaution, see function [get_jaccard_bounds](SetSketchParams::get_jaccard_bounds).   
//! Moreover the sketches produced are mergeable see function [merge](SetSketcher::merge).
//!
//! The cardinal of the set can be estimated with the basic (unoptimized) function [get_cardinal_stats](SetSketcher::get_cardinal_stats)

use std::hash::BuildHasher;
use std::hash::BuildHasherDefault;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::LazyLock;

use rand::Rng;
use rand_distr::Exp1;
use rand_xoshiro::Xoshiro256PlusPlus;
use rand_xoshiro::rand_core::SeedableRng;

use crate::fyshuffle::FyShuffle;

/// Parameter set used by `SetSketcher`
#[derive(Copy, Clone, Debug)]
pub struct SetSketchParams {
    // As per the paper; q is implicit in the choice of register type
    b: f64,
    m: usize,
    a: f64,
    // Derived values
    lnb: f64,
    inva: f64,
}

impl SetSketchParams {
    pub fn new(b: f64, m: usize, a: f64) -> Self {
        SetSketchParams {
            b,
            m,
            a,
            lnb: (b - 1.).ln_1p(),
            inva: 1. / a,
        }
    }

    pub fn recommended() -> &'static Self {
        &RECOMMENDED_PARAMS
    }
}

static RECOMMENDED_PARAMS: LazyLock<SetSketchParams> =
    LazyLock::new(|| SetSketchParams::new(1.001, 4096, 20.));

/// Integer type to use for registers.
type I = u16;

/// A sketch of some set.
///
/// This cannot be used to sketch any new values (but can be merged). In exchange, it doesn't
/// require a `T` or a hasher, unlike `SetSketcher`.
pub struct SetSketch {
    params: &'static SetSketchParams,
    k_vec: Vec<I>,
}

impl SetSketch {
    pub fn new(params: &'static SetSketchParams) -> Self {
        let k_vec: Vec<I> = vec![0; params.m];

        SetSketch { params, k_vec }
    }

    /// The function returns a 2-uple with first field cardinal estimator and second field the
    /// **relative standard deviation**.  
    ///
    /// It is a relatively cpu costly function (the computed logs are not cached in the SetSketcher
    /// structure) that involves log and exp calls on the whole sketch vector.
    pub fn get_cardinal_stats(&self) -> (f64, f64) {
        let sumbk = self.k_vec.iter().fold(0.0f64, |acc: f64, c| {
            acc + (-(*c as f64) * self.params.lnb).exp()
        });
        let cardinality: f64 = self.params.m as f64 * (1. - 1. / self.params.b)
            / (self.params.a * self.params.lnb * sumbk);

        let rel_std_dev = ((self.params.b + 1.) / (self.params.b - 1.) * self.params.lnb - 1.)
            / self.params.m as f64;
        let rel_std_dev = rel_std_dev.sqrt();
        (cardinality, rel_std_dev)
    }

    pub fn merge(&mut self, other: &SetSketch) {
        for i in 0..self.k_vec.len() {
            self.k_vec[i] = self.k_vec[i].max(other.k_vec[i]);
        }
    }

    pub fn get_registers(&self) -> &[I] {
        &self.k_vec
    }
}

/// This structure implements Setsketch1 algorithm
///   
/// The default parameters ensure capacity to represent a set up to 10^28 elements.
pub struct SetSketcher<T, H: Hasher + Default> {
    data: SetSketch,
    // minimum of values stored in vec_k
    lower_k: f64,
    nbmin: usize,
    permut_generator: FyShuffle,
    /// the Hasher to use if data arrive unhashed. Anyway the data type we sketch must satisfy the
    /// trait Hash
    b_hasher: BuildHasherDefault<H>,
    /// just to mark the type we sketch
    t_marker: PhantomData<T>,
}

impl<T, H: Hasher + Default> Deref for SetSketcher<T, H> {
    type Target = SetSketch;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T, H: Hasher + Default> DerefMut for SetSketcher<T, H> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T, H> SetSketcher<T, H>
where
    T: Hash,
    H: Hasher + Default,
{
    pub fn new(params: &'static SetSketchParams, b_hasher: BuildHasherDefault<H>) -> Self {
        SetSketcher::<T, H> {
            data: SetSketch::new(params),
            lower_k: 0.,
            nbmin: 0,
            permut_generator: FyShuffle::new(params.m),
            b_hasher,
            t_marker: PhantomData,
        }
    }

    pub fn sketch(&mut self, to_sketch: &T) {
        let hval1: u64 = self.b_hasher.hash_one(&to_sketch);
        let mut rng = Xoshiro256PlusPlus::seed_from_u64(hval1);
        self.permut_generator.reset();

        let mut x_pred: f64 = 0.;
        for j in 0..self.params.m {
            let x_j = x_pred
                + (self.params.inva / (self.params.m - j) as f64) * rng.sample::<f64, Exp1>(Exp1); // use Ziggurat
            x_pred = x_j;
            let lb_xj = x_j.ln() / self.params.lnb; // log base b of x_j
            let z = (1. - lb_xj).floor();
            let k = z.clamp(0.0, I::MAX as f64) as I;
            if k as f64 <= self.lower_k {
                break;
            }
            let i = self.permut_generator.next(&mut rng);
            if k > self.k_vec[i] {
                self.k_vec[i] = k;
                self.nbmin += 1;
                if self.nbmin.is_multiple_of(self.params.m) {
                    let flow = self
                        .k_vec
                        .iter()
                        .fold(self.k_vec[0], |min: I, x| if x < &min { *x } else { min })
                        as f64;
                    self.lower_k = flow;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use fnv::FnvHasher;

    use super::*;

    #[test]
    fn test_merge_1() {
        let vbmax = 2000;
        let va: Vec<usize> = (0..1000).collect();
        let vb: Vec<usize> = (900..vbmax).collect();
        let union = 2000.;

        let params = SetSketchParams::recommended();

        let mut sethasher_a: SetSketcher<usize, FnvHasher> =
            SetSketcher::new(params, BuildHasherDefault::<FnvHasher>::default());
        // now compute sketches
        for va in va.iter() {
            sethasher_a.sketch(va);
        }
        let mut sethasher_b: SetSketcher<usize, FnvHasher> =
            SetSketcher::new(params, BuildHasherDefault::<FnvHasher>::default());
        // now compute sketches
        for vb in vb.iter() {
            sethasher_b.sketch(vb);
        }
        // merging vb into va
        sethasher_a.merge(&sethasher_b);
        let (mean, std) = sethasher_a.get_cardinal_stats();
        assert!((mean - union).abs() / union <= 2.0 * std);
    }
}
