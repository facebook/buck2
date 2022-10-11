#![warn(missing_docs)]
//! Calculate [Delta E](http://www.colorwiki.com/wiki/Delta_E:_The_Color_Difference)
//! (color difference) between two colors in CIE Lab space.
//!
//! # Examples
//!
//! ```
//! use std::error::Error;
//! use deltae::*;
//!
//! fn main() -> Result<(), Box<dyn Error>>{
//!     // Lab from a string
//!     let lab0: LabValue = "89.73, 1.88, -6.96".parse()?;
//!     // Lab directly from values
//!     let lab1 = LabValue {
//!         l: 95.08,
//!         a: -0.17,
//!         b: -10.81,
//!     }.validate()?; // Validate that the values are in range
//!
//!     // Create your own Lab type
//!     #[derive(Clone, Copy)]
//!     struct MyLab(f32, f32, f32);
//!
//!     // Types that implement Into<LabValue> also implement the Delta trait
//!     impl From<MyLab> for LabValue {
//!         fn from(mylab: MyLab) -> Self {
//!             LabValue { l: mylab.0, a: mylab.1, b: mylab.2 }
//!         }
//!     }
//!     let mylab = MyLab(95.08, -0.17, -10.81);
//!
//!     // Implement DeltaEq for your own types
//!     impl<D: Delta + Copy> DeltaEq<D> for MyLab {}
//!
//!     // Assert that colors are equivalent within a tolerance
//!     assert_delta_eq!(mylab, lab1, DE2000, 0.0, "mylab is not equal to lab1!");
//!
//!     // Calculate DeltaE between two lab values
//!     let de0 = DeltaE::new(&lab0, &lab1, DE2000);
//!     // Use the Delta trait
//!     let de1 = lab0.delta(lab1, DE2000);
//!     assert_eq!(de0, de1);
//!
//!     // Convert to other color types
//!     let lch0 = LchValue::from(lab0);
//!     let xyz0 = XyzValue::from(lab1);
//!     // If DE2000 is less than 1.0, the colors are considered equivalent
//!     assert!(lch0.delta_eq(&lab0, DE2000, 1.0));
//!     assert!(xyz0.delta_eq(&lab1, DE2000, 1.0));
//!
//!     // Calculate DeltaE between different color types
//!     let de2 = lch0.delta(xyz0, DE2000);
//!     assert_eq!(de2.round_to(4), de0.round_to(4));
//!     // There is some loss of accuracy in the conversion.
//!     // Usually rounding to 4 decimal places is more than enough.
//!
//!     // Recalculate DeltaE with different method
//!     let de3 = de2.with_method(DE1976);
//!
//!     println!("{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n",
//!         lab0, // [L:89.73, a:1.88, b:-6.96]
//!         lab1, // [L:95.08, a:-0.17, b:-10.81]
//!         lch0, // [L:89.73, c:7.2094383, h:285.11572]
//!         xyz0, // [X:0.84574246, Y:0.8780792, Z:0.8542397]
//!         de0,  // 5.316941
//!         de1,  // 5.316941
//!         de2,  // 5.316937
//!         de3,  // 6.902717
//!     );
//!
//!     Ok(())
//! }
//! ```

pub mod color;
mod convert;
mod delta;
pub mod eq;
mod round;
mod validate;

#[cfg(test)]
mod tests;

pub use DEMethod::*;
pub use color::*;
pub use delta::*;
pub use eq::*;
pub use round::*;
pub use validate::*;

use std::fmt;
use std::io;

pub(crate) type ValueResult<T> = Result<T, color::ValueError>;

/// ## The measured difference between two colors
///
/// There are many different methods of calculating color difference. Different methods have a
/// specific purpose, mainly in determining the level of tolerance for describing the difference
/// between two colors. Regardless of the [`DEMethod`] used, [`DeltaE`] is always calculated based on the
/// [`LabValue`]s of the two colors.
#[derive(Debug, Clone, Copy)]
pub struct DeltaE {
    /// The mathematical method used for calculating color difference
    method: DEMethod,
    /// The calculated Delta E value
    value: f32,
    /// The reference color
    reference: LabValue,
    /// The sample color
    sample: LabValue,
}

impl DeltaE {
    /// New [`DeltaE`] from two colors and a [`DEMethod`].
    /// ```
    /// use deltae::{LabValue, DeltaE, DEMethod::DE2000};
    ///
    /// let lab0 = LabValue::new(89.73, 1.88, -6.96).unwrap();
    /// let lab1 = LabValue::new(95.08, -0.17, -10.81).unwrap();
    /// let de0 = DeltaE::new(&lab0, &lab1, DE2000);
    /// assert_eq!(de0, 5.316941);
    /// ```
    #[inline]
    pub fn new<A, B>(a: A, b: B, method: DEMethod) -> DeltaE
    where A: Delta, B: Delta {
        a.delta(b, method)
    }

    /// Recalculate [`DeltaE`] with another [`DEMethod`]
    /// ```
    /// use deltae::{Delta, DeltaE, LabValue, DEMethod};
    ///
    /// let lab0 = LabValue::new(89.73, 1.88, -6.96).unwrap();
    /// let lab1 = LabValue::new(95.08, -0.17, -10.81).unwrap();
    /// let de2000 = lab0.delta(lab1, DEMethod::DE2000);
    /// let de1976 = de2000.with_method(DEMethod::DE1976);
    /// assert_eq!(de1976, 6.902716);
    /// ```
    #[inline]
    pub fn with_method(self, method: DEMethod) -> Self {
        self.reference.delta(self.sample, method)
    }

    /// Return a reference the [`DeltaE`] method used in the calculation
    pub fn method(&self) -> &DEMethod {
        &self.method
    }

    /// Return a reference to the [`DeltaE`] value
    pub fn value(&self) -> &f32 {
        &self.value
    }

    /// Return a reference to the reference [`LabValue`] used in the calculation. A reference color
    /// is the base color to which the sample color is being compared.
    pub fn reference(&self) -> &LabValue {
        &self.reference
    }

    /// Return a reference to the sample [`LabValue`] used in the calculation. A sample color is
    /// the color being compared to the reference color.
    pub fn sample(&self) -> &LabValue {
        &self.sample
    }
}

impl fmt::Display for DeltaE {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.value)
    }
}

impl PartialEq<f32> for DeltaE {
    fn eq(&self, f: &f32) -> bool {
        &self.value == f
    }
}

impl PartialEq for DeltaE {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

/// One should be careful when ordering DeltaE. A `DE2000:1.0` value is not
/// necessarily the same amount of color difference as a amount of color
/// difference `DE1976:1.0` value.
impl PartialOrd for DeltaE {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

/// The most common DeltaE methods
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum DEMethod{
    /// The default DeltaE method
    DE2000,
    /// An implementation of DeltaE with tolerances for Lightness and Chroma
    DECMC(f32, f32),
    /// CIE94 DeltaE implementation, weighted with a tolerance for graphics
    DE1994G,
    /// CIE94 DeltaE implementation, weighted with a tolerance for textiles
    DE1994T,
    /// The original DeltaE implementation, a basic euclidian distance formula
    DE1976,
}

/// DeltaE CMC (1:1)
pub const DECMC1: DEMethod = DECMC(1.0, 1.0);
/// DeltaE CMC (2:1)
pub const DECMC2: DEMethod = DECMC(2.0, 1.0);

impl Eq for DEMethod {}

impl Default for DEMethod {
    fn default() -> DEMethod {
        DEMethod::DE2000
    }
}

impl fmt::Display for DEMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DECMC(tl, tc) => {
                if (tl, tc) == (&1.0, &1.0) {
                    write!(f, "DECMC1")
                } else if (tl, tc) == (&2.0, &1.0) {
                    write!(f, "DECMC2")
                } else {
                    write!(f, "DECMC({:0.2}:{:0.2})", tl, tc)
                }
            }
            _ => write!(f, "{:?}", self)
        }
    }
}

