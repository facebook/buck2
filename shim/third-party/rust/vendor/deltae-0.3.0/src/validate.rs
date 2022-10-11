use super::*;

/// Trait to validate whether a type has appropriate values
pub trait Validate where Self: Sized {
    /// Return `Err()` if the values are invalid
    fn validate(self) -> ValueResult<Self>;
}

const RANGE_PCT: std::ops::RangeInclusive<f32> = 0.0..=100.0;
const RANGE_I8: std::ops::RangeInclusive<f32> = -128.0..=128.0;
const RANGE_CHROMA: std::ops::RangeInclusive<f32> = 0.0..=181.01933;
const RANGE_360: std::ops::RangeInclusive<f32> = 0.0..=360.0;
const RANGE_01: std::ops::RangeInclusive<f32> = 0.0..=1.0;

impl Validate for LabValue {
    fn validate(self) -> ValueResult<Self> {
        if RANGE_PCT.contains(&self.l)
            && RANGE_I8.contains(&self.a)
            && RANGE_I8.contains(&self.b)
        {
            Ok(self)
        } else {
            Err(ValueError::OutOfBounds)
        }
    }
}

impl Validate for LchValue {
    fn validate(self) -> ValueResult<Self> {
        if RANGE_PCT.contains(&self.l)
            && RANGE_CHROMA.contains(&self.c)
            && RANGE_360.contains(&self.h)
        {
            Ok(self)
        } else {
            Err(ValueError::OutOfBounds)
        }
    }
}

impl Validate for XyzValue {
    fn validate(self) -> ValueResult<Self> {
        if RANGE_01.contains(&self.x)
            && RANGE_01.contains(&self.y)
            && RANGE_01.contains(&self.z)
        {
            Ok(self)
        } else {
            Err(ValueError::OutOfBounds)
        }
    }
}
