//! ## `Tolerance` and `DeltaEq` traits
//!
//! This module deals with comparing two colors by [`DeltaE`] within a certain [`Tolerance`].
//!
//! See also: [`assert_delta_eq`]
//!
//! ### Implementing `Tolerance` and `DeltaEq`
//!
//! ```
//! use deltae::*;
//!
//! struct MyTolerance(f32);
//!
//! impl Tolerance for MyTolerance {
//!     fn tolerance(self) -> f32 {
//!         self.0
//!     }
//! }
//!
//! #[derive(Copy, Clone)]
//! struct MyLab(f32, f32, f32);
//!
//! // Types that implement Into<Lab> also implement the Delta trait
//! impl From<MyLab> for LabValue {
//!     fn from(mylab: MyLab) -> LabValue {
//!         LabValue {
//!             l: mylab.0,
//!             a: mylab.1,
//!             b: mylab.2,
//!         }
//!     }
//! }
//!
//! impl<D: Delta + Copy> DeltaEq<D> for MyLab {}
//!
//! let mylab = MyLab(89.73, 1.88, -6.96);
//! let lab = LabValue::new(89.73, 1.88, -6.96).unwrap();
//! let de2000 = mylab.delta(lab, DEMethod::DE2000);
//! assert!(mylab.delta_eq(&lab, DE1976, 0.0));
//! ```
use crate::*;

/// Trait to determine whether two values are within a certain tolerance of [`DeltaE`]. Types that
/// implement Into<[`LabValue`]> implicitly implement [`Delta`]. Types that implement [`Delta`] and
/// [`Copy`] may also implement DeltaEq for other types that also implement [`Delta`] and [`Copy`].
/// ```
/// use deltae::*;
///
/// #[derive(Copy, Clone)]
/// struct MyLab(f32, f32, f32);
///
/// // Types that implement Into<Lab> implicitly implement the Delta trait
/// impl From<MyLab> for LabValue {
///     fn from(mylab: MyLab) -> LabValue {
///         LabValue {
///             l: mylab.0,
///             a: mylab.1,
///             b: mylab.2,
///         }
///     }
/// }
///
/// // Types that implement Delta and Copy may also implement DeltaEq for other types that also
/// // implement Delta and Copy
/// impl<D: Delta + Copy> DeltaEq<D> for MyLab {}
///
/// let mylab = MyLab(89.73, 1.88, -6.96);
/// let lab = LabValue::new(89.73, 1.88, -6.96).unwrap();
/// let de2000 = mylab.delta(lab, DEMethod::DE2000);
/// assert!(mylab.delta_eq(&lab, DE1976, 0.0));
/// ```
pub trait DeltaEq<D: Delta + Copy>: Delta + Copy {
    /// Return true if the value is less than or equal to the [`Tolerance`]
    fn delta_eq<T: Tolerance>(&self, other: D, method: DEMethod, tolerance: T) -> bool {
        self.delta(other, method).value() <= &tolerance.tolerance()
    }
}

/// Convenience macro for asserting two values are equivalent within a tolerance
/// ```
/// use deltae::*;
///
/// let lab0 = LabValue::new(50.0, 0.0, 0.0).unwrap();
/// let lab1 = LabValue::new(50.1, 0.1, 0.1).unwrap();
///
/// // Assert that the difference between lab0 and lab1 is less than 1.0 DE2000
/// assert_delta_eq!(lab0, lab1, DE2000, 1.0);
/// ```
#[macro_export]
macro_rules! assert_delta_eq {
    ($reference:expr, $sample:expr, $method:expr, $tolerance:expr) => {
        assert!($reference.delta_eq($sample, $method, $tolerance))
    };
    ($reference:expr, $sample:expr, $method:expr, $tolerance:expr, $($message:tt)*) => {
        assert!($reference.delta_eq($sample, $method, $tolerance), $($message)*)
    };
}

/// Trait to define a tolerance value for the [`DeltaEq`] trait
pub trait Tolerance {
    /// Return a tolerance value
    fn tolerance(self) -> f32;
}

impl Tolerance for f32 {
    fn tolerance(self) -> f32 {
        self
    }
}

impl Tolerance for f64 {
    fn tolerance(self) -> f32 {
        self as f32
    }
}

impl Tolerance for DeltaE {
    fn tolerance(self) -> f32 {
        self.value
    }
}

macro_rules! impl_delta_eq {
    ($t:ty) => {
        impl<D: Delta + Copy> DeltaEq<D> for $t {}
    }
}

impl_delta_eq!(LabValue);
impl_delta_eq!(LchValue);
impl_delta_eq!(XyzValue);
