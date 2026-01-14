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

//! Fisher Yates permutation generator

use rand::distr::Distribution;
use rand::distr::Uniform;

/// Fisher Yates random permutation generation (sampling without replacement), with lazy generation
/// of an array of size n
pub(crate) struct FyShuffle {
    m: usize,
    /// uniform distribution on [0,1)
    unif_01: Uniform<f64>,
    v: Vec<usize>,
    lastidx: usize,
}

impl FyShuffle {
    /// Initialize a random permutation generator on a set of size m
    pub(crate) fn new(m: usize) -> FyShuffle {
        let v: Vec<usize> = (0..m).collect();
        FyShuffle {
            m,
            unif_01: Uniform::<f64>::new(0., 1.).unwrap(),
            v,
            lastidx: m,
        }
    }

    // See https://www.geeksforgeeks.org/generate-a-random-permutation-of-1-to-n/
    // and The algorithm design manual S.Skiena P.458

    /// Generates next randomly choosen item of the set (you get sampling without replacement)
    ///
    /// After being call m times, it is possible to get the full permutation with the function get_values
    /// as the permutation is fully sampled.
    pub(crate) fn next(&mut self, rng: &mut impl rand::Rng) -> usize {
        if self.lastidx >= self.m {
            self.lastidx = 0;
        }
        let xsi = self.unif_01.sample(rng);
        // sample between self.lastidx (included) and self.m (excluded)
        let idx = self.lastidx + (xsi * (self.m - self.lastidx) as f64) as usize;
        let val = self.v[idx];
        self.v.swap(idx, self.lastidx);
        self.lastidx += 1;
        val
    }

    pub(crate) fn reset(&mut self) {
        self.lastidx = 0;
        for i in 0..self.m {
            self.v[i] = i;
        }
    }

    /// Returns the set of permuted index
    #[cfg(test)]
    pub(crate) fn get_values(&self) -> &Vec<usize> {
        &self.v
    }
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use rand::prelude::*;
    use rand_xoshiro::Xoshiro256PlusPlus;

    use super::*;

    #[test]
    // We check we have a unifom distribution of values at each rank of v
    // variance is 5/4
    fn test_fyshuffle_1() {
        let mut rng = Xoshiro256PlusPlus::seed_from_u64(45679);
        let m = 4;
        let mut fypermut = FyShuffle::new(m);
        let nb_permut = 500000;
        let mut freq = vec![0_usize; m];

        for _ in 0..nb_permut {
            for _ in 0..m {
                fypermut.next(&mut rng);
            }
            let v = fypermut.get_values();
            for k in 0..v.len() {
                freq[k] += v[k];
            }
            fypermut.reset();
        }

        let th_freq = 1.5;
        let th_var = 5. / 4.;
        let sigma = (th_var / (nb_permut as f64)).sqrt();
        for f in freq.iter() {
            let rel_error = ((*f as f64) / (nb_permut as f64) - th_freq) / sigma;
            assert!(rel_error.abs() < 3.)
        }
    }

    #[test]
    fn test_fyshuffle_2() {
        let mut rng = Xoshiro256PlusPlus::seed_from_u64(45679);
        let m = 4;
        let mut p_count = HashMap::<[usize; 4], usize>::new();

        let mut h = [0usize; 4];
        let mut fypermut = FyShuffle::new(m);
        let nb_iterations = 1000000;
        for _ in 0..nb_iterations {
            for _ in 0..m + 5 {
                let _val = fypermut.next(&mut rng);
                // hash values
            }
            let values = fypermut.get_values();
            h[..m].copy_from_slice(&values[..m]);
            match p_count.get_mut(&h) {
                Some(count) => {
                    *count += 1;
                }
                _ => {
                    p_count.insert(h, 1);
                }
            }
            fypermut.reset();
        }
        // check all 24 permut are in equal count
        assert_eq!(p_count.len(), 24);
        let count = p_count.into_values().collect::<Vec<usize>>();
        let mu_th = nb_iterations as f64 / 24_f64;
        let sigma = (1. / (24.) * (23. / 24.) * nb_iterations as f64).sqrt();

        let nb_s = (0..count.len())
            .map(|i| (count[i] as f64 - mu_th) / sigma)
            .collect::<Vec<f64>>();
        for x in nb_s.iter().take(count.len()).copied() {
            assert!(x.abs() < 3.);
        }
    }
}
