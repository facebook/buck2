use array_macro::array;
use core::convert::Infallible;

/// Enum mapping type
///
/// This trait is internally used by `#[derive(Enum)]`. `Enum<T>` is
/// implemented by any enum type where V is a generic type representing a
/// value. The purpose of this generic type is to allow providing a value
/// type for arrays, as Rust currently does not support higher kinded types.
///
/// This trait is also implemented by `bool` and `u8`. While `u8` is
/// strictly speaking not an actual enum, there are good reasons to consider
/// it like one, as array of `u8` keys is a relatively common pattern.
pub trait Enum<V>: Sized {
    /// Representation of an enum map for type `V`, usually an array.
    type Array;
    /// Number of possible states the type can have.
    const POSSIBLE_VALUES: usize;
    /// Gets a slice from an array type.
    fn slice(array: &Self::Array) -> &[V];
    /// Gets a mutable slice from an array type.
    fn slice_mut(array: &mut Self::Array) -> &mut [V];
    /// Takes an usize, and returns an element matching `to_usize` function.
    fn from_usize(value: usize) -> Self;
    /// Returns an unique identifier for a value within range of `0..POSSIBLE_VALUES`.
    fn to_usize(self) -> usize;
    /// Creates an array using a function called for each argument.
    fn from_function<F: FnMut(Self) -> V>(f: F) -> Self::Array;
}

impl<T> Enum<T> for bool {
    type Array = [T; 2];
    const POSSIBLE_VALUES: usize = 2;
    #[inline]
    fn slice(array: &[T; 2]) -> &[T] {
        array
    }
    #[inline]
    fn slice_mut(array: &mut [T; 2]) -> &mut [T] {
        array
    }
    #[inline]
    fn from_usize(value: usize) -> Self {
        match value {
            0 => false,
            1 => true,
            _ => unreachable!(),
        }
    }
    #[inline]
    fn to_usize(self) -> usize {
        self as usize
    }
    #[inline]
    fn from_function<F: FnMut(Self) -> T>(mut f: F) -> [T; 2] {
        [f(false), f(true)]
    }
}

impl<T> Enum<T> for u8 {
    type Array = [T; 256];
    const POSSIBLE_VALUES: usize = 256;
    #[inline]
    fn slice(array: &[T; 256]) -> &[T] {
        array
    }
    #[inline]
    fn slice_mut(array: &mut [T; 256]) -> &mut [T] {
        array
    }
    #[inline]
    fn from_usize(value: usize) -> Self {
        value as u8
    }
    #[inline]
    fn to_usize(self) -> usize {
        self as usize
    }
    #[inline]
    fn from_function<F: FnMut(Self) -> T>(mut f: F) -> [T; 256] {
        array![|i| f(i as u8); 256]
    }
}

impl<T> Enum<T> for Infallible {
    type Array = [T; 0];
    const POSSIBLE_VALUES: usize = 0;
    #[inline]
    fn slice(array: &[T; 0]) -> &[T] {
        array
    }
    #[inline]
    fn slice_mut(array: &mut [T; 0]) -> &mut [T] {
        array
    }
    #[inline]
    fn from_usize(_: usize) -> Self {
        unreachable!();
    }
    #[inline]
    fn to_usize(self) -> usize {
        match self {}
    }
    #[inline]
    fn from_function<F: FnMut(Self) -> T>(_: F) -> [T; 0] {
        []
    }
}
