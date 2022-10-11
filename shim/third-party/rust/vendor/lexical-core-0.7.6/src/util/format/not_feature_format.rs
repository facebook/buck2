#![cfg(not(feature = "format"))]

use bitflags::bitflags;

bitflags! {
    /// Dummy bitflags for the float format.
    #[doc(hidden)]
    #[derive(Default)]
    pub struct NumberFormat: u64 {
        const __NONEXHAUSTIVE = 0;
    }
}


impl NumberFormat {
    #[inline]
    pub fn standard() -> Option<NumberFormat> {
        Some(NumberFormat::default())
    }

    #[inline]
    pub fn digit_separator(&self) -> u8 {
        0
    }
}
