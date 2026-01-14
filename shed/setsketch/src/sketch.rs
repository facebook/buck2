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
//!

#![allow(clippy::comparison_chain)]

use serde::{Deserialize, Serialize};
use serde_json::to_writer;

use std::fs::OpenOptions;
use std::io::{BufReader, BufWriter};
use std::path::Path;

use rand::prelude::*;
use rand_distr::Exp1;
use rand_xoshiro::Xoshiro256PlusPlus;
use std::hash::{BuildHasher, BuildHasherDefault, Hash, Hasher};
use std::marker::PhantomData;

use num::{Bounded, FromPrimitive, Integer, ToPrimitive};

use rayon::prelude::*;

use argmin::core::{CostFunction, Executor};
use argmin::solver::goldensectionsearch::GoldenSectionSearch;
#[cfg(feature = "slog")]
use argmin_observer_slog::SlogLogger;
#[cfg(feature = "slog")]
use argmin::core::observers::ObserverMode;

use anyhow::anyhow;

use crate::fyshuffle::*;

#[cfg_attr(doc, katexit::katexit)]
/// Parameters defining the Sketcher
/// - choice of a : given $\epsilon$ a is chosen verifying  $$ a \ge  \frac{1}{\epsilon} * log(\frac{m}{b})  $$ so that the probability
///   of any sketch value being  negative is less than $\epsilon$
/// *(lemma 4 of setsketch paper)*.  
///
///
/// - choice of q:  if $$ q >=  log_{b} (\frac{m  n  a}{\epsilon})$$ then a sketch value is less than q+1 with proba less than $\epsilon$ up to n data to sketch.
///  *(see lemma 5 of paper)*.  
///
/// The default initialization corresponds to $m = 4096, b = 1.001,  a = 20 , q = 2^{16} -2 = 65534$ and guarantees the absence of negative value in sketch with proba $8.28 \space 10^{-6}$ and probability of
/// sketch value greater than q+1 with probability less than $2.93 \space 10^{-6}$.  
/// With low probability truncature the sketches can thus be represented by a u16 vector.
///
///    
///
#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct SetSketchParams {
    // b must be <= 2
    b: f64,
    // size of sketch
    m: u64,
    // default is 20.
    a: f64,
    //
    q: u64,
}

impl Default for SetSketchParams {
    fn default() -> Self {
        SetSketchParams {
            b: 1.001,
            m: 4096,
            a: 20.,
            q: 2_u64.pow(16) - 2,
        }
    }
} // end of SetSketchParams

impl SetSketchParams {
    #[cfg_attr(doc, katexit::katexit)]
    /// - m is the number of sketch.
    /// - b is a parameter in the interval ]1., 2.[, in fact near 1. is better.
    /// - a is a parameter to be adjusted to reduce probability $\epsilon$ of having negative values in sketchs.
    /// - q minimal size in bits of sketch values. related to m,a, and  $\epsilon$
    pub fn new(b: f64, m: u64, a: f64, q: u64) -> Self {
        SetSketchParams { b, m, a, q }
    }

    //
    pub fn get_a(&self) -> f64 {
        self.a
    }

    //
    pub fn get_b(&self) -> f64 {
        self.b
    }

    //
    pub fn get_q(&self) -> u64 {
        self.q
    }

    //
    pub fn get_m(&self) -> u64 {
        self.m
    }

    //
    pub fn set_m(&mut self, nb_sketch: usize) {
        self.m = nb_sketch as u64;
    }

    /// get bounds for J given parameters and first estimate for jaccard.    
    /// Returns a 2-uple (lower, upper).    
    /// For the parameters choice the difference between lower and upper bound should be smaller than accepted error over the range of jaccard
    /// needed by problem treated.  
    /// For the default parameters the difference between lower and upper is less 0.5% of jaccard value.
    pub fn get_jaccard_bounds(&self, jac: f64) -> (f64, f64) {
        assert!(jac <= 1.);
        // we want to compute b^(D0 / 2m) with article notation.
        // for us jac = D_0/m
        //  let b_aux = self.b.powf(D_0/ (2. * self.m as f64)); //   b^(jac/2m)
        let b_aux = self.b.powf(jac * 0.5);
        let jsup = (b_aux * b_aux - 1.) / (self.b - 1.);
        //
        let b_inf = 2. * (b_aux * self.b.sqrt() - 1.) / (self.b - 1.) - 1.;
        let jinf = b_inf.max(0.);
        //
        log::debug!("b_inf : {:.5e}, b_aux : {:.3e}", b_inf, b_aux);
        //
        assert!(jac >= 1. || jinf <= jsup);
        //
        (jinf, jsup)
    }

    pub fn dump_json(&self, dirpath: &Path) -> Result<(), String> {
        //
        let filepath = dirpath.join("parameters.json");
        //
        log::info!("dumping SetSketchParams in json file : {:?}", filepath);
        //
        let fileres = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&filepath);
        if fileres.is_err() {
            log::error!(
                "SetSketchParams dump : dump could not open file {:?}",
                filepath.as_os_str()
            );
            println!(
                "SetSketchParams dump: could not open file {:?}",
                filepath.as_os_str()
            );
            return Err("SetSketchParams dump failed".to_string());
        }
        //
        let mut writer = BufWriter::new(fileres.unwrap());
        to_writer(&mut writer, &self).unwrap();
        //
        Ok(())
    } // end of dump_json

    /// reload from a json dump. Used in request module to ensure coherence with database constitution
    pub fn reload_json(dirpath: &Path) -> Result<Self, String> {
        log::info!("in reload_json");
        //
        let filepath = dirpath.join("parameters.json");
        let fileres = OpenOptions::new().read(true).open(&filepath);
        if fileres.is_err() {
            log::error!(
                "SetSketchParams reload_json : reload could not open file {:?}",
                filepath.as_os_str()
            );
            println!(
                "SetSketchParams reload_json: could not open file {:?}",
                filepath.as_os_str()
            );
            return Err("SetSketchParams reload_json could not open file".to_string());
        }
        //
        let loadfile = fileres.unwrap();
        let reader = BufReader::new(loadfile);
        let hll_parameters: Self = serde_json::from_reader(reader).unwrap();
        //
        Ok(hll_parameters)
    } // end of reload_json
} // end of impl SetSketchParams

/// This structure implements Setsketch1 algorithm which suppose that the size of
/// on which the algorithm runs is large compared to the size of sketch, see function [SetSketcher::get_nb_overflow].
///   
/// The default parameters ensure capacity to represent a set up to 10^28 elements.
/// I is an integer u16, u32. u16 should be sufficient for most cases (see [SetSketchParams])
pub struct SetSketcher<I: Integer, T, H: Hasher + Default> {
    // b must be <= 2. In fact we use lnb (precomputed log of b)
    _b: f64,
    // size of sketch
    m: u64,
    // default is 20
    a: f64,
    //
    q: u64,
    // random values,
    k_vec: Vec<I>,
    // minimum of values stored in vec_k
    lower_k: f64,
    //
    nbmin: u64,
    //
    permut_generator: FYshuffle,
    //
    nb_overflow: u64,
    // we store ln(b)
    lnb: f64,
    /// the Hasher to use if data arrive unhashed. Anyway the data type we sketch must satisfy the trait Hash
    b_hasher: BuildHasherDefault<H>,
    /// just to mark the type we sketch
    t_marker: PhantomData<T>,
}

impl<I, T, H> Default for SetSketcher<I, T, H>
where
    I: Integer + Bounded + ToPrimitive + FromPrimitive + Copy + Clone,
    H: Hasher + Default,
{
    /// the default parameters give 4096 sketch with a capacity for counting up to 10^19 elements
    fn default() -> SetSketcher<I, T, H> {
        let params = SetSketchParams::default();
        let m: usize = 4096;
        let k_vec: Vec<I> = (0..m).map(|_| I::zero()).collect();
        let lnb = (params.get_b() - 1.).ln_1p(); // this is ln(b) for b near 1.
        SetSketcher::<I, T, H> {
            _b: params.get_b(),
            m: params.get_m(),
            a: params.get_a(),
            q: params.get_q(),
            k_vec,
            lower_k: 0.,
            nbmin: 0,
            permut_generator: FYshuffle::new(m),
            nb_overflow: 0,
            lnb,
            b_hasher: BuildHasherDefault::<H>::default(),
            t_marker: PhantomData,
        }
    }
}

impl<I, T, H> SetSketcher<I, T, H>
where
    I: Integer + ToPrimitive + FromPrimitive + Bounded + Copy + Clone + std::fmt::Debug,
    T: Hash,
    H: Hasher + Default,
{
    /// allocate a new sketcher
    pub fn new(params: SetSketchParams, b_hasher: BuildHasherDefault<H>) -> Self {
        //
        let k_vec: Vec<I> = (0..params.get_m()).map(|_| I::zero()).collect();
        let lnb = (params.get_b() - 1.).ln_1p(); // this is ln(b) for b near 1.
                                                 //
        SetSketcher::<I, T, H> {
            _b: params.get_b(),
            m: params.get_m(),
            a: params.get_a(),
            q: params.get_q(),
            k_vec,
            lower_k: 0.,
            nbmin: 0,
            permut_generator: FYshuffle::new(params.get_m() as usize),
            nb_overflow: 0,
            lnb,
            b_hasher,
            t_marker: PhantomData,
        }
    }

    /// return logarithm base of sketches
    pub fn get_b(&self) -> f64 {
        self._b
    }

    // We implement algo sketch1 as we will use it for large number of data and so correlation are expected to be very low.
    /// take into account one more data
    pub fn sketch(&mut self, to_sketch: &T) -> anyhow::Result<()> {
        //
        let hval1: u64 = self.b_hasher.hash_one(&to_sketch);
        //
        let imax: u64 = I::max_value().to_u64().unwrap(); // max value of I as a u64
                                                          //
        let mut rng = Xoshiro256PlusPlus::seed_from_u64(hval1);
        self.permut_generator.reset();
        //
        let iq1: i64 = self.q as i64 + 1;
        let inva: f64 = 1. / self.a;
        //
        let mut x_pred: f64 = 0.;
        for j in 0..self.m {
            //
            let x_j = x_pred + (inva / (self.m - j) as f64) * rng.sample::<f64, Exp1>(Exp1); // use Ziggurat
            x_pred = x_j;
            //
            let lb_xj = x_j.ln() / self.lnb; // log base b of x_j
                                             //
            if lb_xj > -self.lower_k {
                break;
            }
            //
            let z: i64 = iq1.min((1. - lb_xj).floor() as i64);
            log::trace!(
                "j : {}, x_j : {:.5e} , lb_xj : {:.5e}, z : {:.5e}",
                j,
                x_j,
                lb_xj,
                z
            );
            let k = 0.max(z) as u64;
            //
            if k as f64 <= self.lower_k {
                break;
            }
            // now work with permutation sampling
            let i = self.permut_generator.next(&mut rng);
            //
            if k > self.k_vec[i].to_u64().unwrap() {
                log::trace!("setting slot i: {}, f_k : {:.3e}", i, k);
                // we must enforce that f_k fits into I
                if k > imax {
                    self.nb_overflow += 1;
                    self.k_vec[i] = I::from_u64(imax).unwrap();
                    log::warn!(
                        "I overflow , got a k value {:.3e} over I::max : {:#}",
                        k,
                        imax
                    );
                } else {
                    self.k_vec[i] = I::from_u64(k).unwrap();
                }
                self.nbmin += 1;
                if self.nbmin % self.m == 0 {
                    let flow = self
                        .k_vec
                        .iter()
                        .fold(self.k_vec[0], |min: I, x| if x < &min { *x } else { min })
                        .to_f64()
                        .unwrap();
                    if flow > self.lower_k {
                        // no register can decrease so self.lower_k must not decrease
                        log::debug!(
                            "j : {}, nbmin = {} , setting low to : {:?}",
                            j,
                            self.nbmin,
                            flow
                        );
                        self.lower_k = flow;
                    }
                }
            }
        }
        //
        Ok(())
    } // end of sketch

    /// returns the lowest value of sketch
    /// a null value is a diagnostic of bad skecthing. As the algorithm suppose
    /// that the size of the set sketched is large compared to the number of sketch this
    /// has a very low probability.
    pub fn get_low_sketch(&self) -> i64 {
        self.lower_k.floor() as i64
    }

    /// returns the number of time value sketcher overflowed the number of bits allocated
    /// should be less than number of values sketched / 100_000 if parameters are well chosen.
    pub fn get_nb_overflow(&self) -> u64 {
        self.nb_overflow
    }

    /// Arg to_sketch is an array ( a slice) of values to hash.
    /// It can be used in streaming to update current sketch
    pub fn sketch_slice(&mut self, to_sketch: &[T]) -> anyhow::Result<()> {
        //
        if to_sketch.is_empty() {
            println!(" empty arg");
            return Err(anyhow!("empty sketch"));
        }
        //
        for val in to_sketch {
            self.sketch(val).unwrap();
        }
        //
        Ok(())
    } // end of sketch_slice

    /// The function returns a 2-uple with first field cardinal estimator and second field the **relative standard deviation**.  
    ///
    /// It is a relatively cpu costly function (the computed logs are not cached in the SetSketcher structure) that involves log and exp calls on the whole sketch vector.
    pub fn get_cardinal_stats(&self) -> (f64, f64) {
        let sumbk = self.k_vec.iter().fold(0.0f64, |acc: f64, c| {
            acc + (-c.to_f64().unwrap() * (self._b - 1.).ln_1p()).exp()
        });
        let cardinality: f64 = self.m as f64 * (1. - 1. / self._b) / (self.a * self.lnb * sumbk);
        //
        let rel_std_dev = ((self._b + 1.) / (self._b - 1.) * self.lnb - 1.) / self.m as f64;
        let rel_std_dev = rel_std_dev.sqrt();
        (cardinality, rel_std_dev)
    }

    // reset state
    pub fn reinit(&mut self) {
        //
        self.permut_generator.reset();
        self.k_vec = (0..self.m).map(|_| I::zero()).collect();
        self.lower_k = 0.;
        self.nbmin = 0;
        self.nb_overflow = 0;
    } // end of reinit

    /// Setsketch is mergeable with another sketch if parameters are the same.  
    /// We can thus get a sketch for a union and so estimate the cardinal of the union of 2 sets.  
    /// Self replaces its sketch by the union of itself and the other if the result is OK
    /// otherwise it return Err and is left unchanged
    pub fn merge(&mut self, other: &SetSketcher<I, T, H>) -> anyhow::Result<()> {
        // check parameters equality
        if self.m != other.m || self.q != other.q {
            return Err(anyhow!("non mergeable : different sketching parameters"));
        }
        if (self._b - other._b).abs() / self._b >= f64::EPSILON
            || (self.a - other.a).abs() / self.a >= f64::EPSILON
        {
            return Err(anyhow!("non mergeable : different sketching parameters"));
        }
        // takes max
        for i in 0..self.k_vec.len() {
            self.k_vec[i] = self.k_vec[i].max(other.k_vec[i]);
        }
        //
        self.nb_overflow += other.nb_overflow;
        //
        Ok(())
    } // end of merge

    /// get signature sketch. Same as get_hsketch
    pub fn get_signature(&self) -> &Vec<I> {
        &self.k_vec
    }

    // in fact I prefer get_signature
    #[inline(always)]
    pub fn get_hsketch(&self) -> &Vec<I> {
        self.get_signature()
    }
} // end of impl SetSketch<F:

//========================================================================================

/// computes minus likelyhood , to be minimized by Brent method.
/// We use notations of Ertl paper
#[derive(Copy, Clone, Debug)]
struct MleCost {
    //
    dplus: f64,
    dless: f64,
    dequal: f64,
    u: f64,
    v: f64,
    b: f64,
}

impl MleCost {
    fn new(dplus: f64, dless: f64, dequal: f64, u: f64, v: f64, b: f64) -> Self {
        MleCost {
            dplus,
            dless,
            dequal,
            u,
            v,
            b,
        }
    }

    // -ln(1. - x * (b-1)/b) / ln(b) =  -ln(1. - x * (b-1)/b) / (b-1).ln_1p()
    //  then we can distinguish if x < 0 we can use ln_1p for the numerator too
    //  We know x is in [-1. , 1.] so x * (b-1)/b) is small
    fn pb(&self, x: f64) -> f64 {
        let val = if x <= 0. {
            let arg = -x * (self.b - 1.) / self.b;
            -arg.ln_1p() / (self.b - 1.).ln_1p()
        } else {
            let arg = 1. - x * (self.b - 1.) / self.b;
            -arg.ln() / (self.b - 1.).ln_1p()
        };
        //
        assert!(!val.is_nan());
        //
        val
    } // end of pb
} // end of MleCost

// We need to associate a cost function to MleJaccard to run argmin
impl CostFunction for MleCost {
    type Param = f64;
    type Output = f64;
    //
    fn cost(&self, j: &Self::Param) -> Result<Self::Output, argmin::core::Error> {
        //
        let pbplus = self.pb(self.u - self.v * j);
        let pbless = self.pb(self.v - self.u * j);
        let log_likelyhood = self.dplus * pbplus.ln()
            + self.dless * pbless.ln()
            + self.dequal * (1. - pbplus - pbless).ln();
        // return - likelyhood as we want to maximize
        Ok(-log_likelyhood)
    } // end of cost
}

/// This function implements Jaccard joint estimation based on likelyhood estimator
/// as described in Ertl paper paragraph 3.2.
/// It is interesting when estimating low Jaccard values (under 0.1), where the setsketch
/// estimator can suffer of a loss of precision. Of course it more expensive that
/// base estimator.
/// The structure needs the parameters used during sketching.  
/// **It is the user responsability to use it with skecthes from the same parameters**
pub struct MleJaccard {
    // b must be <= 2. In fact we use lnb (precomputed log of b)
    b: f64,
    // size of sketch
    m: u64,
    // default is 20
    a: f64,
    // we store ln(b)
    lnb: f64,
} // end of MleJaccard

impl From<SetSketchParams> for MleJaccard {
    fn from(params: SetSketchParams) -> Self {
        MleJaccard::new(params.get_b(), params.get_m(), params.get_a())
    }
} // end of From<SetSketchParams>

impl MleJaccard {
    pub fn new(b: f64, m: u64, a: f64) -> Self {
        let lnb = (b - 1.).ln_1p(); // this is ln(b) for b near 1.
        MleJaccard { b, m, a, lnb }
    }

    pub fn get_cardinal_estimate<I>(&self, sketch: &[I]) -> f64
    where
        I: Integer + Bounded + ToPrimitive + FromPrimitive + Copy + Clone + Send + Sync,
        [I]: ParallelSlice<I>,
    {
        //
        assert_eq!(self.m, sketch.len() as u64);
        // we know we use large value sof m
        let sumbk: f64 = sketch
            .into_par_iter()
            .map(|c| (-(*c).to_f64().unwrap() * (self.b - 1.).ln_1p()).exp())
            .sum();
        let cardinality: f64 = self.m as f64 * (1. - 1. / self.b) / (self.a * self.lnb * sumbk);
        //
        cardinality
    } // end of get_cardinal_estimate

    /// This function implements Jaccard joint estimation based on likelyhood estimator
    /// as described in Ertl paper paragraph 3.2
    pub fn get_mle<I>(&self, sketch1: &[I], sketch2: &[I]) -> Option<f64>
    where
        I: Integer + Bounded + ToPrimitive + FromPrimitive + Copy + Clone + Send + Sync,
        [I]: ParallelSlice<I>,
    {
        //
        assert_eq!(self.m, sketch1.len() as u64);
        assert_eq!(self.m, sketch2.len() as u64);
        // we know we use large value sof m
        let card1 = self.get_cardinal_estimate(sketch1);
        let card2 = self.get_cardinal_estimate(sketch2);
        log::info!("mle_jaccard card1 : {}, card2 : {}", card1, card2);
        let u = card1 / (card1 + card2);
        let v = card2 / (card1 + card2);
        //
        let mut dplus: u32 = 0;
        let mut dless: u32 = 0;
        let mut dequal: u32 = 0;
        for i in 0..sketch1.len() {
            if sketch1[i] > sketch2[i] {
                dplus += 1;
            } else if sketch1[i] < sketch2[i] {
                dless += 1;
            } else {
                dequal += 1;
            }
        }
        //
        let b_inf = 0.;
        let aux = card1 / card2;
        log::debug!("get_cardinal_estimate : {}", aux);
        let b_sup = aux.min(1. / aux);
        log::info!(
            "mle_jaccard interval : ({} ,  {}) , dequal: {}",
            b_inf,
            b_sup,
            dequal
        );
        let jac = dequal as f64 / self.m as f64;
        //
        let solver = GoldenSectionSearch::new(b_inf, b_sup).unwrap();
        let init_param = jac;
        //
        let cost = MleCost::new(dplus as f64, dless as f64, dequal as f64, u, v, self.b);

        let exec = Executor::new(cost, solver)
            .configure(|state| state.param(init_param).max_iters(100));

        #[cfg(feature = "slog")]
        let exec = exec.add_observer(SlogLogger::term(), ObserverMode::Always);

        let res = exec.run().unwrap();

        log::info!("res : {:#}", res);

        let state = res.state();
        log::info!("state : {:#?}", state);
        log::info!(
            "best solution (J): {:#?}, cost : {:#?}",
            state.best_param,
            state.best_cost
        );

        // simple exploration around jac estimate seem better
        log::info!("\n trying simple exploration : ");
        struct Best {
            jac: f64,
            opt: f64,
        }
        let mut best = Best {
            jac: 0.,
            opt: f64::MAX,
        };
        for i in 0..5 {
            let jac_pertu = jac * (1. + i as f64 / 200.);
            let j_mle_pertu = cost.cost(&jac_pertu).unwrap();
            if j_mle_pertu < best.opt {
                best = Best {
                    jac: jac_pertu,
                    opt: j_mle_pertu,
                };
            }
            log::info!(" j : {}, le : {} ", jac_pertu, j_mle_pertu);
        }
        //
        for i in 0..5 {
            let jac_pertu = jac * (1. - i as f64 / 200.);
            let j_mle_pertu = cost.cost(&jac_pertu).unwrap();
            if j_mle_pertu < best.opt {
                best = Best {
                    jac: jac_pertu,
                    opt: j_mle_pertu,
                };
            }
            log::info!(" j : {}, le : {} ", jac_pertu, j_mle_pertu);
        }
        log::info!("jmle : {}, opt : {}", best.jac, best.opt);
        //
        let _j_b1 = self.get_mle_approx_b1(sketch1, sketch2).unwrap();
        //
        state.best_param
    } // end of mle_jaccard

    fn get_mle_approx_b1<I>(&self, sketch1: &[I], sketch2: &[I]) -> Option<f64>
    where
        I: Integer + Bounded + ToPrimitive + FromPrimitive + Copy + Clone + Send + Sync,
        [I]: ParallelSlice<I>,
    {
        //
        assert_eq!(self.m, sketch1.len() as u64);
        assert_eq!(self.m, sketch2.len() as u64);
        // we know we use large value sof m
        let card1 = self.get_cardinal_estimate(sketch1);
        let card2 = self.get_cardinal_estimate(sketch2);
        log::info!("mle_jaccard card1 : {}, card2 : {}", card1, card2);
        let u = card1 / (card1 + card2);
        let v = card2 / (card1 + card2);
        //
        let mut dplus: u32 = 0;
        let mut dless: u32 = 0;
        let mut dequal: u32 = 0;
        for i in 0..sketch1.len() {
            if sketch1[i] > sketch2[i] {
                dplus += 1;
            } else if sketch1[i] < sketch2[i] {
                dless += 1;
            } else {
                dequal += 1;
            }
        }
        let dplus = dplus as f64;
        let dless = dless as f64;
        let dequal: f64 = dequal as f64;
        let mut aux = u * u * (dless + dequal) - v * v * (dplus + dequal);
        aux = aux * aux;
        let mut j: f64 = u * u * (dless + dequal) + v * v * (dplus + dequal)
            - (aux + 4. * dless * dplus * (u * v) * (u * v)).sqrt();
        j /= 2. * u * v * self.m as f64;
        //
        log::info!(" j mle approx for b -> 1 : {:?}", j);
        //
        Some(j)
    } // end of get_mle_approx_b1
} // end of impl MleJaccard

//======================================================================================================

#[cfg(test)]
mod tests {

    use super::*;
    use crate::jaccard::*;
    use fnv::FnvHasher;
    use rand::distr::Uniform;

    #[allow(dead_code)]
    fn log_init_test() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_params_bounds() {
        //
        log_init_test();
        //
        #[allow(unused_mut)]
        let mut params = SetSketchParams::default();
        // we can possibly change params.b to check effect
        log::info!("params : {:?}", params);
        //
        let nb_frac = 50;

        for j in 1..=nb_frac {
            let jac = (j as f64) / (nb_frac as f64);
            let (jinf, jsup) = params.get_jaccard_bounds(jac);
            let delta = 100. * (jsup - jinf) / jac;
            log::info!(
                "j = {},  jinf : {:.5e}, jsup = {:.5e}, delta% : {:.3}",
                jac,
                jinf,
                jsup,
                delta
            );
        }
    } // end of test_params_bounds

    #[test]
    fn test_range_inter1_hll_fnv_f32() {
        //
        log_init_test();
        // we construct 2 ranges [a..b] [c..d], with a<b, b < d, c<d sketch them and compute jaccard.
        // we should get something like max(b,c) - min(b,c)/ (b-a+d-c)
        //
        let va: Vec<usize> = (0..1000).collect();
        let vb: Vec<usize> = (900..2000).collect();
        let inter = 100; // intersection size
        let jexact = inter as f32 / 2000 as f32;
        let nb_sketch = 2000;
        //
        let mut params = SetSketchParams::default();
        params.set_m(nb_sketch);
        let mut sethasher: SetSketcher<u16, usize, FnvHasher> =
            SetSketcher::new(params, BuildHasherDefault::<FnvHasher>::default());
        // now compute sketches
        let resa = sethasher.sketch_slice(&va);
        if !resa.is_ok() {
            println!("error in sketcing va");
            return;
        }
        let ska = sethasher.get_signature().clone();
        //
        sethasher.reinit();
        //
        let resb = sethasher.sketch_slice(&vb);
        if !resb.is_ok() {
            println!("error in sketching vb");
            return;
        }
        let skb = sethasher.get_signature();
        //
        log::debug!("ska = {:?}", ska);
        log::debug!("skb = {:?}", skb);
        //
        let jac = get_jaccard_index_estimate(&ska, &skb).unwrap();
        let sigma = (jexact * (1. - jexact) / params.get_m() as f32).sqrt();
        log::info!(
            " jaccard estimate {:.3e}, j exact : {:.3e} , sigma : {:.3e}",
            jac,
            jexact,
            sigma
        );
        // we have 10% common values and we sample a sketch of size 50 on 2000 values , we should see intersection
        assert!(jac > 0. && (jac as f32) < jexact + 3. * sigma);
    } // end of test_range_intersection_fnv_f32

    // a test with very different size of slices
    #[test]
    fn test_range_inter2_hll_fnv_f32() {
        //
        log_init_test();
        // we construct 2 ranges [a..b] [c..d], with a<b, b < d, c<d sketch them and compute jaccard.
        // we should get something like max(b,c) - min(b,c)/ (b-a+d-c)
        //
        let vb_max = 20000;
        let vb_min = 10000;
        //
        let va_min = 500;
        let va_max = 10100;
        //
        let va: Vec<usize> = (va_min..va_max).collect();
        let vb: Vec<usize> = (vb_min..vb_max).collect();
        let jexact = (va_max - vb_min) as f32 / (vb_max - va_min) as f32;
        let nb_sketch = 4000;
        //
        let mut params = SetSketchParams::default();
        params.set_m(nb_sketch);
        let mut sethasher: SetSketcher<u16, usize, FnvHasher> =
            SetSketcher::new(params, BuildHasherDefault::<FnvHasher>::default());
        // now compute sketches
        let resa = sethasher.sketch_slice(&va);
        if !resa.is_ok() {
            println!("error in sketcing va");
            return;
        }
        let low_sketch = sethasher.get_low_sketch();
        log::info!("lowest sketch : {}", low_sketch);
        assert!(low_sketch > 0);
        let cardinal = sethasher.get_cardinal_stats();
        log::info!(
            "cardinal of set a : {:.3e} relative stddev : {:.3e}",
            cardinal.0,
            cardinal.1
        );

        let ska = sethasher.get_signature().clone();
        //
        sethasher.reinit();
        //
        let resb = sethasher.sketch_slice(&vb);
        if !resb.is_ok() {
            println!("error in sketching vb");
            return;
        }
        let skb = sethasher.get_signature();
        let cardinal = sethasher.get_cardinal_stats();
        log::info!(
            "cardinal of set b : {:.3e} relative stddev : {:.3e}",
            cardinal.0,
            cardinal.1
        );
        //
        log::debug!("ska = {:?}", ska);
        log::debug!("skb = {:?}", skb);
        //
        let jac = get_jaccard_index_estimate(&ska, &skb).unwrap();
        let sigma = (jexact * (1. - jexact) / params.get_m() as f32).sqrt();
        log::info!(
            " jaccard estimate {:.3e}, j exact : {:.3e} , sigma : {:.3e}",
            jac,
            jexact,
            sigma
        );
        // we have 10% common values and we sample a sketch of size 50 on 2000 values , we should see intersection
        assert!(jac > 0. && (jac as f32) < jexact + 3. * sigma);
    } // end of test_range_intersection_fnv_f32

    #[test]
    fn test_hll_card_with_repetition() {
        //
        log_init_test();
        //
        let vamax = 200;
        let nb_sketch = 5000;
        //
        let mut params = SetSketchParams::default();
        params.set_m(nb_sketch);
        let mut sethasher: SetSketcher<u16, usize, FnvHasher> =
            SetSketcher::new(params, BuildHasherDefault::<FnvHasher>::default());
        let unif = Uniform::<usize>::new(0, vamax).unwrap();
        let mut rng = Xoshiro256PlusPlus::seed_from_u64(45679 as u64);

        for _ in 0..nb_sketch {
            sethasher.sketch(&unif.sample(&mut rng)).unwrap();
        }
        let cardinal = sethasher.get_cardinal_stats();
        log::info!(
            "cardinal of set b : {:.3e} relative stddev : {:.3e}",
            cardinal.0,
            cardinal.1
        );
    } // end of test_hll_card_with_repetition

    // test merge
    #[test]
    fn test_merge_1() {
        //
        log_init_test();
        //
        let vbmax = 2000;
        let va: Vec<usize> = (0..1000).collect();
        let vb: Vec<usize> = (900..vbmax).collect();
        let union = 2000.;
        let nb_sketch = 4000;
        //
        let mut params = SetSketchParams::default();
        params.set_m(nb_sketch);
        //
        let mut sethasher_a: SetSketcher<u16, usize, FnvHasher> =
            SetSketcher::new(params, BuildHasherDefault::<FnvHasher>::default());
        // now compute sketches
        let resa = sethasher_a.sketch_slice(&va);
        if !resa.is_ok() {
            println!("error in sketcing va");
            return;
        }
        let mut sethasher_b: SetSketcher<u16, usize, FnvHasher> =
            SetSketcher::new(params, BuildHasherDefault::<FnvHasher>::default());
        // now compute sketches
        let resb = sethasher_b.sketch_slice(&vb);
        if !resb.is_ok() {
            println!("error in sketcing vb");
            return;
        }
        // merging vb into va
        let res = sethasher_a.merge(&sethasher_b);
        assert!(res.is_ok());
        let (mean, std) = sethasher_a.get_cardinal_stats();
        log::info!("mean , std : {:.3e}, {:.3e}", mean, std);
        //
        assert!((mean - union).abs() / union <= 2.0 * std);
        // now we compute intersection between sethasher_a which represent union and b
        let jac =
            get_jaccard_index_estimate(&sethasher_a.get_signature(), &sethasher_b.get_signature())
                .unwrap();
        let jexact = vb.len() as f32 / vbmax as f32;
        let sigma = (jexact * (1. - jexact) / params.get_m() as f32).sqrt();
        log::info!(
            " jaccard estimate {:.3e}, j exact : {:.3e} , sigma : {:.3e}",
            jac,
            jexact,
            sigma
        );
        assert!(jac > 0. && (jac as f32) < jexact + 3. * sigma);
        // try to add more things in sketcha
        for i in 500..2500 {
            sethasher_a.sketch(&i).unwrap();
        }
        log::info!("after adding in merged sethasher_a");
        let (mean, std) = sethasher_a.get_cardinal_stats();
        log::info!("mean , relative std : {:.3e}, {:.3e}", mean, std);
    } // end test_merge_1

    // test to check maximum likelyhood joint estimator at low J
    // shows that mle not better than standard jaccard estimate.
    #[test]
    //#[allow(unused)]
    fn test_mle_1() {
        //
        log_init_test();
        //
        let vbmax = 2000;
        let vbmin = 995;
        let vamax = 1000;
        assert!(vamax > vbmin);
        let va: Vec<usize> = (0..vamax).collect();
        let vb: Vec<usize> = (vbmin..vbmax).collect();
        let nb_sketch = 6000;
        //
        let mut params = SetSketchParams::default();
        params.set_m(nb_sketch);
        //
        let mut sethasher_a: SetSketcher<u16, usize, FnvHasher> =
            SetSketcher::new(params, BuildHasherDefault::<FnvHasher>::default());
        // now compute sketches
        let resa = sethasher_a.sketch_slice(&va);
        if !resa.is_ok() {
            println!("error in sketcing va");
            return;
        }
        let mut sethasher_b: SetSketcher<u16, usize, FnvHasher> =
            SetSketcher::new(params, BuildHasherDefault::<FnvHasher>::default());
        // now compute sketches
        let resb = sethasher_b.sketch_slice(&vb);
        if !resb.is_ok() {
            println!("error in sketcing vb");
            return;
        }
        // now we compute intersection between sethasher_a and sethasher_b
        log::info!("test_mle : vbmax : {}, nbsketch : {}", vbmax, nb_sketch);
        let jac =
            get_jaccard_index_estimate(&sethasher_a.get_signature(), &sethasher_b.get_signature())
                .unwrap();
        let jexact = (vamax - vbmin) as f32 / vbmax as f32;
        let sigma = (jexact * (1. - jexact) / params.get_m() as f32).sqrt();
        log::info!(
            " jaccard estimate {:.3e}, j exact : {:.3e} , sigma : {:.3e}",
            jac,
            jexact,
            sigma
        );
        assert!(jac > 0. && (jac as f32) < jexact + 3. * sigma);
        // check for mle estimator
        let mle_jaccard = MleJaccard::from(params);
        let bounds = params.get_jaccard_bounds(jac);
        log::info!("bounds for jaccard estimate : {:#?}", bounds);

        let j = mle_jaccard.get_mle(&sethasher_a.get_signature(), sethasher_b.get_signature());
        log::info!("j mle : {}", j.unwrap());
    } // end test_mle_1
}
