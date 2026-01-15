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
use rand_distr::StandardUniform;
use rand_xoshiro::Xoshiro256PlusPlus;
use rand_xoshiro::rand_core::SeedableRng;

use crate::fyshuffle::FyShuffle;

/// Parameter set used by `SetSketcher`
#[derive(Copy, Clone, Debug)]
pub struct SetSketchParams {
    // As per the paper; q is implicit in the choice of register type
    b: f64,
    m: usize,
    // Derived values
    lnb: f64,
    inva: f64,
    rel_cardinality_factor: f64,
}

impl SetSketchParams {
    pub fn new(b: f64, m: usize, a: f64) -> Self {
        let lnb = (b - 1.).ln_1p();
        let rel_cardinality_factor = m as f64 * (1. - 1. / b) / (a * lnb);
        SetSketchParams {
            b,
            m,
            lnb: (b - 1.).ln_1p(),
            inva: 1. / a,
            rel_cardinality_factor,
        }
    }

    pub fn recommended() -> &'static Self {
        &RECOMMENDED_PARAMS
    }

    pub fn rel_cardinality_factor(&self) -> f64 {
        self.rel_cardinality_factor
    }

    /// Returns the relative standard deviation of cardinality estimates resulting from these params
    pub fn card_estimate_std_dev(&self) -> f64 {
        let rel_std_dev = ((self.b + 1.) / (self.b - 1.) * self.lnb - 1.) / self.m as f64;
        rel_std_dev.sqrt()
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
#[derive(Clone)]
pub struct SetSketch {
    params: &'static SetSketchParams,
    k_vec: Vec<I>,
}

impl SetSketch {
    pub fn new(params: &'static SetSketchParams) -> Self {
        let k_vec: Vec<I> = vec![0; params.m];

        SetSketch { params, k_vec }
    }

    /// Load up the sketch found in the given registers
    ///
    /// Panics if the length of the sketch doesn't match what's in the params.
    pub fn from_registers(params: &'static SetSketchParams, registers: Vec<I>) -> Self {
        assert_eq!(params.m, registers.len());
        SetSketch {
            params,
            k_vec: registers,
        }
    }

    pub fn params(&self) -> &'static SetSketchParams {
        self.params
    }

    /// The function returns an approximation of the cardinality of the sketched set.  
    ///
    /// It is a relatively cpu costly function (the computed logs are not cached in the SetSketcher
    /// structure) that involves log and exp calls on the whole sketch vector.
    pub fn cardinality(&self) -> f64 {
        self.rel_cardinality() * self.params.rel_cardinality_factor
    }

    /// Returns a value that is equal to the approximate cardinality of the underlying set times a
    /// constant factor.
    ///
    /// The constant factor can be retrived from `SetSketchParams::rel_cardinality_factor`.
    pub(crate) fn rel_cardinality(&self) -> f64 {
        let sumbk = self.k_vec.iter().copied().fold(0.0f64, |acc: f64, c| {
            acc + (-(c as f64) * self.params.lnb).exp()
        });
        1. / sumbk
    }

    /// Given two sketches, returns the cardinality of the intersection of those sketches.
    ///
    /// The error of this is a constant factor in the size of the original sketches, not in the
    /// output value.
    pub fn absolute_overlap(&self, other: &Self) -> f64 {
        let self_c = self.cardinality();
        let other_c = other.cardinality();
        let merged_c = self.clone().union(other).cardinality();
        self_c + other_c - merged_c
    }

    pub fn union(mut self, other: &SetSketch) -> Self {
        self.merge(other);
        self
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
        self.sketch_weighted_locality_unstable(to_sketch, 1);
    }

    /// Just pulls out the logic that takes a sample from an exponential distribution with parameter
    /// `a` and transforms it into the value we actually store in the register.
    fn exp_sample_into_register_value(s: f64, params: &'static SetSketchParams) -> I {
        let lb_s = s.ln() / params.lnb;
        let z = (1. - lb_s).floor();
        z.clamp(0.0, I::MAX as f64) as I
    }

    /// Add the given value to the sketch using the given weight.
    ///
    /// The cardinality estimate that results from weighted insertions is the sum of the weights.
    ///
    /// The recommended parameters are tuned for cardinality estimates between 1 and 10^28; weights
    /// that cause cardinality estimates outside of that range may be expected to be poorly behaved.
    ///
    /// When the same item is sketched more than once using different weights, the following
    /// behaviors apply:
    ///
    ///  1. Within one sketch, only the largest weight "counts"; re-inserting with smaller weights
    ///     is a nop.
    ///  2. Cardinality estimates based on the sketch remain fully correct. This also applies to any
    ///     other values derived from cardinality estimates, such as inclusion-exclusion based
    ///     Jaccard similarity (the absolute_overlap function).
    ///  3. However, locality sensitivity becomes incorrect; concretely, sketches normally have a
    ///     coupling property in which two sketches are equal on any given pair of registers with
    ///     probability exactly their Jaccard similarity. This property is violated when the two
    ///     sketches sketch the same value with different weights. The only exception to this is
    ///     when the difference between the weights differs by factor << 1/b (1000 under the
    ///     recommended parameters)
    ///
    /// Use of this function can be mixed with the unweighted `sketch`; `sketch` is just an alias
    /// for this with weight 1.
    pub fn sketch_weighted_locality_unstable(&mut self, to_sketch: &T, weight: u64) {
        if weight == 0 {
            return;
        }
        let hval1: u64 = self.b_hasher.hash_one(&to_sketch);
        let mut rng = Xoshiro256PlusPlus::seed_from_u64(hval1);
        self.permut_generator.reset();

        let mut x_pred: f64 = 0.;
        // The inverse of the parameter of the exponential distribution we use; the miminum of `N`
        // samples from an exponential distribution with param `a` is itself exponentially
        // distributed with param `aN`, so this is the only thing we have to do to account for
        // weight
        let inv_exp_param = self.params.inva / (weight as f64);
        for j in 0..self.params.m {
            let x_j = x_pred
                + (inv_exp_param / (self.params.m - j) as f64) * rng.sample::<f64, Exp1>(Exp1); // use Ziggurat
            x_pred = x_j;
            let k = Self::exp_sample_into_register_value(x_j, self.params);
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

/// Samples `n` times from an exponential distribution with parameter 1 and returns the
/// minimum of the n samples.
///
/// This is optimized to take time logarithmic in n.
///
/// Note that unlike other strategies, this has correct marginal behavior. Concretely, when
/// calling this twice with the same RNG state, but n = 1000 in the one case and n = 1001 in
/// the other case, the return value will be the same with probability 1000/1001.
fn sample_min_exp_stable(rng: &mut Xoshiro256PlusPlus, n: u64) -> f64 {
    let mut current_index = 0;
    let mut min = 1.0f64;
    loop {
        // We implement the index jumping algorithm. Say that the last index we considered
        // is i. Instead of now going and sampling once for i+1, i+2, etc. we directly
        // compute and sample from the distribution of "how many more samples will I need
        // until I see a new minimum value."
        //
        // It's better in every way if we begin by ignoring the exponential distribution and
        // just do this in uniform space
        let delta = if min == 1. {
            1
        } else {
            // Each successive value is less than the current minimum with probability `min` (again,
            // since we're in uniform space). So the number of samples until we see a new minimum is
            // geometrically distributed with parameter `min`. This samples from such a
            // distribution. We avoid using `rand_distr`'s `Geometric` because it's 1) optimized for
            // repeated sampling instead of a single sample and 2) numerically unstable for very
            // small `min`
            let u: f64 = rng.sample::<f64, _>(StandardUniform);
            // Note: Very important for numerical stability here and below to use `ln_1p`, not
            // `(1-min).ln()`; `1-min` loses all the float precision
            (u.ln() / (-min).ln_1p()).floor() as u64 + 1
        };
        let next_min_index = current_index + delta;
        if next_min_index > n {
            // The next minimum would come after the number of samples we have, so we're done
            break;
        }
        current_index = next_min_index;
        // The new min value is uniformly distributed conditional on it indeed being a min
        // value
        min *= rng.sample::<f64, _>(StandardUniform);
    }

    // Transfer into exponential space
    -(-min).ln_1p()
}

impl<T, H> SetSketcher<T, H>
where
    T: Hash,
    H: Hasher + Default,
{
    /// Add the given value to the sketch using the given weight.
    ///
    /// See first the documentation for `sketch_weighted_locality_unstable`.
    ///
    /// This function has generally similar semantics to that one but differs in two ways:
    ///
    ///  1. It recovers the locality sensitivity property that is documented to be lost there.
    ///  2. However, it's quite a bit slower. Unweighted sketching and locality unstable weighted
    ///     sketching are amortized `O(1)`. This is `O(m log W)` for `W` the weight and `m` the
    ///     number of registers.
    ///
    /// This function also must not be mixed with unweighted sketches (for a given item anyway).
    pub fn sketch_weighted_locality_stable(&mut self, to_sketch: &T, weight: u64) {
        if weight == 0 {
            return;
        }
        let hval: u64 = self.b_hasher.hash_one(&to_sketch);
        let mut rng = Xoshiro256PlusPlus::seed_from_u64(hval);
        for r in self.data.k_vec.iter_mut() {
            // `sample_min_exp_stable` uses an amount of randomness that depends on the value of
            // `n`, so we need to do this to make sure that the rng state at the beginning of
            // subsequent loop iterations doesn't depend on n
            let mut iter_rng = rng.clone();
            rng.jump();

            let s = sample_min_exp_stable(&mut iter_rng, weight) * self.data.params.inva;
            let k = Self::exp_sample_into_register_value(s, self.data.params);
            if k > *r {
                *r = k;
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use fnv::FnvHasher;

    use super::*;

    fn check_cardinality_is_about(s: &SetSketch, val: u64) {
        let val = val as f64;
        let mean = s.cardinality();
        let stddev = s.params().card_estimate_std_dev();
        if (mean - val).abs() / val > 3.0 * stddev {
            panic!("Mean {} != expected {}", mean, val);
        }
    }

    fn usize_sketcher() -> SetSketcher<usize, FnvHasher> {
        SetSketcher::new(
            SetSketchParams::recommended(),
            BuildHasherDefault::<FnvHasher>::default(),
        )
    }

    #[test]
    fn test_merge_1() {
        let vbmax = 2000;
        let va: Vec<usize> = (0..1000).collect();
        let vb: Vec<usize> = (900..vbmax).collect();
        let union = 2000;

        let mut sethasher_a = usize_sketcher();
        // now compute sketches
        for va in va.iter() {
            sethasher_a.sketch(va);
        }
        let mut sethasher_b = usize_sketcher();
        // now compute sketches
        for vb in vb.iter() {
            sethasher_b.sketch(vb);
        }
        // merging vb into va
        sethasher_a.merge(&sethasher_b);
        check_cardinality_is_about(&sethasher_a, union);
    }

    fn weighted_styles()
    -> impl IntoIterator<Item = fn(&mut SetSketcher<usize, FnvHasher>, &usize, u64)> {
        [
            SetSketcher::sketch_weighted_locality_stable,
            SetSketcher::sketch_weighted_locality_unstable,
        ]
    }

    #[test]
    fn check_weighted_single() {
        for sketch in weighted_styles() {
            for v in [1, 10, 10000] {
                let mut s = usize_sketcher();
                sketch(&mut s, &0, v);
                check_cardinality_is_about(&s, v);
            }
        }
    }

    #[test]
    fn check_weighted_sums_reasonably() {
        for sketch in weighted_styles() {
            let mut a = usize_sketcher();
            for i in 1..=100 {
                sketch(&mut a, &i, i as u64);
            }
            check_cardinality_is_about(&a, 5050);
        }
    }

    #[test]
    fn check_well_behaved_under_different_weights() {
        for sketch in weighted_styles() {
            let mut a = usize_sketcher();
            sketch(&mut a, &0, 100);
            for i in 1..=10 {
                sketch(&mut a, &i, 10);
            }
            sketch(&mut a, &0, 105);
            check_cardinality_is_about(&a, 205);
        }
    }

    #[test]
    fn check_large_weights() {
        for sketch in weighted_styles() {
            let mut a = usize_sketcher();
            const MULT: u64 = 1_000_000_000_000;
            for i in 1..=100 {
                sketch(&mut a, &i, i as u64 * MULT);
            }
            check_cardinality_is_about(&a, 5050 * MULT);
        }
    }

    #[test]
    fn check_non_locality_sensitivity_on_mismatched_weights() {
        for weight in [1000, 20000] {
            let mut a = usize_sketcher();
            a.sketch_weighted_locality_unstable(&0, weight);
            let mut b = usize_sketcher();
            b.sketch_weighted_locality_unstable(&0, weight + 1);

            let matches = a
                .get_registers()
                .iter()
                .zip(b.get_registers().iter())
                .filter(|(a, b)| a == b)
                .count();
            // See the note about locality sensitivity on the function above
            if weight == 1000 {
                assert!(matches < a.params.m / 10);
            } else {
                assert!(matches > a.params.m * 9 / 10);
            }
        }
    }

    #[test]
    fn check_locality_sensitivity_on_mismatched_weights() {
        let mut a = usize_sketcher();
        a.sketch_weighted_locality_stable(&0, 100);
        let mut b = usize_sketcher();
        b.sketch_weighted_locality_stable(&0, 150);

        let matches = a
            .get_registers()
            .iter()
            .zip(b.get_registers().iter())
            .filter(|(a, b)| a == b)
            .count();
        // See the note about locality sensitivity on the function above
        let expected_matches = a.params.m * 2 / 3;
        assert!(matches > expected_matches - 100);
        assert!(matches < expected_matches + 100);
    }
}
