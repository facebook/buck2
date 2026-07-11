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

use base64::Engine;
use rand::RngExt as _;
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
#[derive(Clone, Debug)]
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

    /// Computes a fast approximant of the proportion of the elements in `self` not found in
    /// `other`, returned as a confidence interval `(low, high)`.
    ///
    /// Uses only a prefix of registers, making the comparison O(1) — fast enough to run over a
    /// large candidate set as a first-pass filter before computing exact overlaps with
    /// [`SetSketch::absolute_overlap`].
    ///
    /// **Important:** This method relies on the locality-sensitive property of the sketch
    /// registers (i.e. that the register value for an element is independent of which set it
    /// belongs to). It must **not** be used with non-locality-sensitive weighting schemes.
    /// For sketches built with `sketch_weighted`, the proportions are proportions of weight
    /// mass rather than of element count.
    pub fn approx_proportion_not_included(&self, other: &SetSketch) -> (f64, f64) {
        // Implementation notes:
        //
        // Elements across the two sets fall into three categories: unique to `self` (proportion
        // `p_a`), unique to `other` (`p_b`), or in the intersection (`p_i`), with
        // `p_a + p_b + p_i = 1`.
        //
        // For each register, the stored value is the max hash over all sketched elements.
        // Because hashes are independent, the "winning" element falls into each subset with
        // probability proportional to its size:
        //   `Pr(S > O) = p_a,  Pr(S < O) = p_b,  Pr(S == O) = p_i`
        // where S and O are register values from `self` and `other` respectively.
        //
        // Counting `S > O` and `S == O` occurrences over a register prefix gives a Bernoulli
        // estimate of `p_a` and `p_i`. The desired return value is `p_a / (p_a + p_i)`,
        // wrapped in a Wilson confidence interval.
        const PREFIX_SIZE: usize = 48;

        let mut p_a = 0usize;
        let mut p_i = 0usize;
        for i in 0..PREFIX_SIZE {
            if self.k_vec[i] == other.k_vec[i] {
                p_i += 1;
            }
            if self.k_vec[i] > other.k_vec[i] {
                p_a += 1;
            }
        }
        static CONF_INTERVALS: LazyLock<[[(f64, f64); PREFIX_SIZE + 1]; PREFIX_SIZE + 1]> =
            LazyLock::new(|| {
                std::array::from_fn(|i| std::array::from_fn(|j| wilson_confidence_interval(i, j)))
            });
        CONF_INTERVALS[p_a][p_i]
    }

    /// Decode a base64-encoded sketch string into a `SetSketch`.
    ///
    /// The string is expected to be a base64-encoded sequence of native-endian
    /// `u16` register values. Both padded and unpadded base64 are accepted.
    pub fn from_base64(base64_str: &str) -> Result<Self, SetSketchDecodeError> {
        let params = SetSketchParams::recommended();
        let bytes = base64::engine::general_purpose::STANDARD_NO_PAD
            .decode(base64_str.trim())
            .map_err(SetSketchDecodeError::Base64)?;

        if bytes.len() % 2 != 0 {
            return Err(SetSketchDecodeError::OddByteCount(bytes.len()));
        }

        let registers: Vec<I> = bytes
            .chunks_exact(2)
            .map(|chunk| I::from_ne_bytes([chunk[0], chunk[1]]))
            .collect();

        if registers.len() != params.m {
            return Err(SetSketchDecodeError::WrongRegisterCount {
                expected: params.m,
                actual: registers.len(),
            });
        }

        Ok(Self {
            params,
            k_vec: registers,
        })
    }

    /// Decode a versioned, base64-encoded sketch string into a `SetSketch`.
    ///
    /// The input format is `"<version>:<base64_data>"` (e.g. `"v1:AAAA..."`).  The version
    /// prefix is returned alongside the decoded sketch so callers can act on it if needed.
    pub fn from_base64_versioned(
        versioned_str: &str,
    ) -> Result<(&str, Self), SetSketchDecodeError> {
        let (version, base64_str) = versioned_str
            .split_once(':')
            .ok_or(SetSketchDecodeError::MissingVersionPrefix)?;
        let sketch = Self::from_base64(base64_str)?;
        Ok((version, sketch))
    }

    /// Like [`from_base64_versioned`](Self::from_base64_versioned), but discards the version
    /// prefix and returns only the decoded sketch.
    pub fn decode_base64_versioned(versioned_str: &str) -> Result<Self, SetSketchDecodeError> {
        let (_version, sketch) = Self::from_base64_versioned(versioned_str)?;
        Ok(sketch)
    }

    /// Decode a base64-encoded sketch that may or may not have a version prefix.
    ///
    /// If the string contains a `':'`, the portion before it is treated as a version prefix
    /// and stripped.  Otherwise the entire string is decoded as raw base64.
    pub fn from_base64_maybe_versioned(s: &str) -> Result<Self, SetSketchDecodeError> {
        match s.split_once(':') {
            Some((_version, base64_str)) => Self::from_base64(base64_str),
            None => Self::from_base64(s),
        }
    }

    /// Convenience constructor that pairs [`from_registers`](Self::from_registers) with
    /// [`SetSketchParams::recommended`].
    pub fn from_recommended_registers(registers: Vec<I>) -> Self {
        Self::from_registers(SetSketchParams::recommended(), registers)
    }

    /// Returns the cardinality estimate as an `i64`, truncating toward zero.
    pub fn get_size_approx(&self) -> i64 {
        self.cardinality() as i64
    }
}

/// Errors that can occur when decoding a `SetSketch` from base64.
#[derive(Debug)]
pub enum SetSketchDecodeError {
    Base64(base64::DecodeError),
    OddByteCount(usize),
    WrongRegisterCount { expected: usize, actual: usize },
    MissingVersionPrefix,
}

impl std::fmt::Display for SetSketchDecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Base64(e) => write!(f, "base64 decode error: {}", e),
            Self::OddByteCount(n) => {
                write!(f, "invalid sketch data: byte count {} must be even", n)
            }
            Self::WrongRegisterCount { expected, actual } => {
                write!(
                    f,
                    "wrong number of registers: expected {}, got {}",
                    expected, actual
                )
            }
            Self::MissingVersionPrefix => {
                write!(f, "expected versioned format \"<version>:<base64_data>\"")
            }
        }
    }
}

impl std::error::Error for SetSketchDecodeError {}

/// For a set of Bernoulli samples with `s` successes and `f` failures, returns a confidence
/// interval for the underlying Bernoulli parameter.
///
/// This uses the Wilson confidence interval which is ~good enough most of the time; importantly it
/// does return reasonable results for `s==0`, `f==0`, or small `s+f`.
fn wilson_confidence_interval(s: usize, f: usize) -> (f64, f64) {
    // Desired z-score; 1.96 gives a 95% confidence interval
    const Z: f64 = 1.96;

    let s = s as f64;
    let f = f as f64;
    let n = s + f;
    if n == 0. {
        return (0.0, 1.0);
    }

    let zsq = Z * Z;
    let mid = (s + zsq / 2.) / (n + zsq);
    let wid = Z / (n + zsq) * (s * f / n + zsq / 4.).sqrt();
    (mid - wid, mid + wid)
}

impl Default for SetSketch {
    fn default() -> Self {
        Self::new(SetSketchParams::recommended())
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

    pub fn into_sketch(self) -> SetSketch {
        self.data
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
    ///
    /// For new sketch corpuses, prefer `sketch_weighted`, which is just as fast but keeps
    /// locality sensitivity.
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

/// Derives the seed of the dart stream for a given (element hash, octave) pair.
///
/// SplitMix64-style finalizer. Together with the dart stream layout this defines the
/// register values of every sketch produced by `sketch_weighted`; it must never change
/// once such sketches are persisted.
fn mix_octave_seed(hval: u64, octave: u32) -> u64 {
    let mut z = hval ^ 0x9E3779B97F4A7C15u64.wrapping_mul(octave as u64 + 1);
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
    z ^ (z >> 31)
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
    ///
    /// For new sketch corpuses, prefer `sketch_weighted`, which offers the same guarantees in
    /// amortized O(1).
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

impl<T, H> SetSketcher<T, H>
where
    T: Hash,
    H: Hasher + Default,
{
    /// Add the given value to the sketch using the given weight, preserving locality
    /// sensitivity, in amortized O(1).
    ///
    /// This supersedes both `sketch_weighted_locality_unstable` (same speed, but locality
    /// sensitive) and `sketch_weighted_locality_stable` (same guarantees, but amortized
    /// O(1) plus O(log w) instead of O(m log w)).
    ///
    /// Semantics:
    ///
    ///  1. The cardinality estimate resulting from weighted insertions is the sum of the
    ///     weights (exactly as for the other weighted insertion functions; the tuned
    ///     parameter range caveats there apply here too).
    ///  2. Within one sketch, re-inserting the same item with any set of weights is
    ///     exactly equivalent to inserting it once with the largest of those weights.
    ///  3. Locality sensitivity holds across different weights. For any register, two
    ///     sketches agree with probability
    ///     `sum_e min(w_A(e), w_B(e)) / sum_e max(w_A(e), w_B(e))` (weighted Jaccard),
    ///     and `self`'s register exceeds `other`'s with probability
    ///     `sum_e (w_A(e) - w_B(e))_+ / sum_e max(...)`. Consequently
    ///     `approx_proportion_not_included` works, with its proportions reinterpreted as
    ///     proportions of weight mass rather than of element count. An item's register
    ///     contributions are also exactly monotone in its weight.
    ///
    /// Compatibility: this function realizes an item's register contributions differently
    /// from every other insertion path, including plain `sketch`. Sketches that will ever
    /// be compared or merged must insert any given item through this function in all of
    /// them or in none of them; mixing paths for the same item makes it count roughly
    /// twice toward cardinality and destroys its locality sensitivity. Migrating means
    /// versioning persisted sketches and routing everything, including weight-1 inserts,
    /// through this function.
    ///
    /// How it works: inserting `(item, w)` inserts a family of virtual sub-items of fixed,
    /// exponentially growing weights — one per octave `[2^(j-1), 2^j)` of the mass
    /// interval `[0, w)` — with the octave straddling `w` retained only up to `w`. Every
    /// octave has a fixed weight, so no seed is ever used at two different rates (the
    /// failure mode of the unstable path), and the octaves of `[0, w)` form a nested
    /// chain, so different weights of one item share all sub-items except the topmost,
    /// whose retentions nest. See `dart_insert` for the primitive and its contract.
    pub fn sketch_weighted(&mut self, to_sketch: &T, weight: u64) {
        if weight == 0 {
            return;
        }
        let hval: u64 = self.b_hasher.hash_one(&to_sketch);
        // Highest octave intersecting [0, weight). Largest octave first: it carries half
        // the total enumeration rate, so `lower_k` rises to near its final value before
        // the cheap low octaves are visited.
        let top = 64 - weight.leading_zeros();
        for j in (0..=top).rev() {
            let lo: u128 = if j == 0 { 0 } else { 1u128 << (j - 1) };
            let hi: u128 = 1u128 << j;
            if lo >= weight as u128 {
                continue;
            }
            let item_weight = (hi - lo) as f64;
            let retained = ((weight as u128).min(hi) - lo) as f64;
            self.dart_insert(mix_octave_seed(hval, j), item_weight, retained);
        }
    }

    /// The weighted-insertion primitive: adds a virtual item of fixed weight
    /// `item_weight`, keeping only `retained_mass` of it
    /// (`0 < retained_mass <= item_weight`).
    ///
    /// The item's contribution is realized as a Poisson stream of "darts" `(x, u, index)`
    /// with x-rate `a * m * item_weight`, each dart landing in a uniformly random
    /// register with an independent uniform mark `u` in `[0, 1)`. A dart is applied iff
    /// `u < retained_mass / item_weight`; a register's contribution is the encoding of
    /// the smallest applied `x` landing on it. Contract, for fixed `(seed, item_weight)`:
    ///
    ///  1. Marginal: each register receives an `Exp(a * retained_mass)` contribution, so
    ///     the item adds exactly `retained_mass` to the cardinality estimate.
    ///  2. Nested thinning: across sketches applying retained masses `c <= c'`, register
    ///     contributions are ordered accordingly and equal with probability exactly
    ///     `c / c'`, independently per register. (`retained_mass == item_weight` is not a
    ///     special code path: `u < 1` always holds.)
    ///  3. The same seed must never be used with a different `item_weight`; the weight is
    ///     part of the virtual item's identity. All variation goes through
    ///     `retained_mass`.
    ///
    /// Realization requirements, each forced by clause 2: the stream is unbounded (a set
    /// of exactly m points is not closed under thinning: keeping one sample w.p. p has
    /// survival `1 - p + p*exp(-lambda*x)`, not `exp(-p*lambda*x)`); register marks are
    /// i.i.d. rather than a permutation (sampling without replacement is not
    /// thinning-closed); and every dart draws its marks in a fixed order whether or not
    /// it is used, because consumers of one stream differ in `retained_mass` and
    /// `lower_k` but must see bit-identical streams.
    fn dart_insert(&mut self, seed: u64, item_weight: f64, retained_mass: f64) {
        debug_assert!(retained_mass > 0. && retained_mass <= item_weight);
        let mut rng = Xoshiro256PlusPlus::seed_from_u64(seed);
        let scale = self.params.inva / (self.params.m as f64 * item_weight);
        let keep_fraction = retained_mass / item_weight;
        let mut x: f64 = 0.;
        let mut darts_since_refresh: usize = 0;
        loop {
            x += scale * rng.sample::<f64, Exp1>(Exp1);
            let k = Self::exp_sample_into_register_value(x, self.params);
            // Darts arrive in increasing x, so k is non-increasing along the stream: once
            // one dart cannot beat the smallest register, no later dart can beat any
            // register.
            if k as f64 <= self.lower_k {
                break;
            }
            // `lower_k` is a stale bound, and unlike the other insertion paths this
            // stream is unbounded, so staleness would be unbounded cost (a stale bound of
            // 0 with a heavy item enumerates ~a*m*item_weight darts). Recompute every m
            // darts, in addition to the every-m-improvements refresh below that all
            // insertion paths share; refreshes read registers only and never touch the
            // RNG, so they cannot desynchronize the stream across sketches.
            darts_since_refresh += 1;
            if darts_since_refresh >= self.params.m {
                darts_since_refresh = 0;
                self.refresh_lower_k();
                if k as f64 <= self.lower_k {
                    break;
                }
            }
            // Fixed draw order for every dart, used or not.
            let u: f64 = rng.sample::<f64, _>(StandardUniform);
            let idx_bits: u64 = rng.sample::<u64, _>(StandardUniform);
            if u >= keep_fraction {
                // The dart falls in the part of the item's mass we were asked to leave
                // out.
                continue;
            }
            // Fixed-point map of idx_bits onto 0..m (bias < m/2^64). Like
            // `mix_octave_seed`, this map is part of the persisted-sketch format.
            let i = ((idx_bits as u128 * self.params.m as u128) >> 64) as usize;
            if k > self.k_vec[i] {
                self.k_vec[i] = k;
                self.nbmin += 1;
                // Unlike the intra-stream refresh above, this one keeps `lower_k` fresh
                // *across* streams: improvements accumulate as the sketch grows, so this
                // fires every constant-factor growth in total mass. Without it, streams
                // shorter than m darts would never refresh and every insertion into a
                // warm sketch would pay ~m darts against a permanently stale bound.
                if self.nbmin.is_multiple_of(self.params.m) {
                    self.refresh_lower_k();
                }
            }
        }
    }

    fn refresh_lower_k(&mut self) {
        let flow = self
            .k_vec
            .iter()
            .fold(self.k_vec[0], |min: I, x| if x < &min { *x } else { min })
            as f64;
        self.lower_k = flow;
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
            SetSketcher::sketch_weighted,
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

    #[test]
    fn check_dart_locality_and_monotonicity_on_mismatched_weights() {
        for (wa, wb) in [(100u64, 150u64), (5, 8), (5, 6), (1000, 1500), (7, 970)] {
            let mut a = usize_sketcher();
            a.sketch_weighted(&0, wa);
            let mut b = usize_sketcher();
            b.sketch_weighted(&0, wb);

            let mut matches = 0usize;
            for (ra, rb) in a.get_registers().iter().zip(b.get_registers().iter()) {
                // Exact: the smaller weight's darts are a subset of the larger's.
                assert!(ra <= rb, "w={wa} vs {wb}: register {ra} > {rb}");
                if ra == rb {
                    matches += 1;
                }
            }
            // The match probability is the weighted Jaccard similarity `wa / wb`, plus a
            // correction for both registers landing in the zero bucket (negligible under
            // the recommended params, where `Pr[register = 0] = exp(-w / inva)`).
            let q = wa as f64 / wb as f64;
            let p = q + (1. - q) * (-(wb as f64) / a.params.inva).exp();
            let m = a.params.m as f64;
            let sigma = (m * p * (1. - p)).sqrt();
            assert!(
                ((matches as f64) - m * p).abs() < 5. * sigma + 8.,
                "w={wa} vs {wb}: {matches} matches, expected ~{}",
                m * p
            );
        }
    }

    #[test]
    fn check_dart_max_weight_wins_exactly() {
        let mut b = usize_sketcher();
        b.sketch_weighted(&7, 5);

        let mut a = usize_sketcher();
        a.sketch_weighted(&7, 5);
        a.sketch_weighted(&7, 3);
        assert_eq!(a.get_registers(), b.get_registers());

        let mut c = usize_sketcher();
        c.sketch_weighted(&7, 3);
        c.sketch_weighted(&7, 5);
        assert_eq!(c.get_registers(), b.get_registers());
    }

    #[test]
    fn check_dart_weighted_jaccard_multi_element() {
        // A: 0..100 at weight 2 (mass 200). B: 50..150 at weight 3 (mass 300).
        // sum max = 50*2 + 50*3 + 50*3 = 400; per register:
        //   Pr[S == O] = sum min / sum max     = 100/400
        //   Pr[S > O]  = sum (wA - wB)+ / max  = 100/400
        //   Pr[S < O]  =                         200/400
        let mut a = usize_sketcher();
        for v in 0..100usize {
            a.sketch_weighted(&v, 2);
        }
        let mut b = usize_sketcher();
        for v in 50..150usize {
            b.sketch_weighted(&v, 3);
        }
        check_cardinality_is_about(&a, 200);
        check_cardinality_is_about(&b, 300);
        check_cardinality_is_about(&a.clone().union(&b), 400);

        let (mut eq, mut gt, mut lt) = (0usize, 0usize, 0usize);
        for (ra, rb) in a.get_registers().iter().zip(b.get_registers().iter()) {
            match ra.cmp(rb) {
                std::cmp::Ordering::Equal => eq += 1,
                std::cmp::Ordering::Greater => gt += 1,
                std::cmp::Ordering::Less => lt += 1,
            }
        }
        let m = a.params.m as f64;
        for (count, p, name) in [(eq, 0.25, "eq"), (gt, 0.25, "gt"), (lt, 0.5, "lt")] {
            let sigma = (m * p * (1. - p)).sqrt();
            assert!(
                ((count as f64) - m * p).abs() < 5. * sigma + 8.,
                "{name}: {count} vs expected ~{}",
                m * p
            );
        }

        // The O(1) prefix filter reads weight-mass proportions:
        // p_a / (p_a + p_i) = 0.25 / 0.5 = 0.5, up to 48-register prefix noise. Bounds kept
        // deliberately loose.
        let (low, high) = a.approx_proportion_not_included(&b);
        assert!(low < 0.62, "low bound {low}");
        assert!(high > 0.38, "high bound {high}");
    }

    #[test]
    fn check_dart_heavy_weight_into_partially_filled_sketch() {
        // Regression canary for `lower_k` staleness: without the per-stream refresh in
        // `dart_insert` this enumerates ~a*m*w (order 1e17) darts and effectively hangs.
        let mut a = usize_sketcher();
        for v in 0..3usize {
            a.sketch_weighted(&v, 1);
        }
        a.sketch_weighted(&1000, 1_000_000_000_000);
        check_cardinality_is_about(&a, 1_000_000_000_003);
    }

    #[test]
    fn check_dart_insert_nesting_law_directly() {
        // The primitive's contract in isolation: same (seed, item_weight), different
        // retained mass. Match probability is 2/8 plus the zero-bucket correction
        // (negligible under the recommended params).
        let seed = 0x5EED_u64;
        let mut full = usize_sketcher();
        full.dart_insert(seed, 8., 8.);
        let mut thin = usize_sketcher();
        thin.dart_insert(seed, 8., 2.);

        check_cardinality_is_about(&full, 8);
        check_cardinality_is_about(&thin, 2);

        let mut matches = 0usize;
        for (rt, rf) in thin.get_registers().iter().zip(full.get_registers().iter()) {
            assert!(rt <= rf);
            if rt == rf {
                matches += 1;
            }
        }
        let p = 0.25 + 0.75 * (-8.0f64 / full.params.inva).exp();
        let m = full.params.m as f64;
        let sigma = (m * p * (1. - p)).sqrt();
        assert!(
            ((matches as f64) - m * p).abs() < 5. * sigma + 8.,
            "{matches} matches, expected ~{}",
            m * p
        );
    }

    #[test]
    fn test_approx_proportion_not_included() {
        let a_vals: Vec<usize> = (0..100).collect();
        let b_vals: Vec<usize> = (70..150).collect();

        let mut a = usize_sketcher();
        for v in &a_vals {
            a.sketch(v);
        }
        let mut b = usize_sketcher();
        for v in &b_vals {
            b.sketch(v);
        }

        check_cardinality_is_about(&a, 100);
        check_cardinality_is_about(&b, 80);

        let merged = a.clone().union(&b);
        check_cardinality_is_about(&merged, 150);

        let (low, high) = a.approx_proportion_not_included(&b);
        assert!(low > 0.5, "low bound {low} should be > 0.5");
        assert!(high < 0.9, "high bound {high} should be < 0.9");
    }
}
