use super::*;

/// Trait for rounding values to a number of decimal places
pub trait Round {
    /// Rounds the value to a number of decimal places
    fn round_to(self, places: i32) -> Self;
}

// Round an f32 to a number of decimal places
fn round_to(val: f32, places: i32) -> f32 {
    let mult = 10_f32.powi(places);
    (val * mult).round() / mult
}

impl Round for DeltaE {
    fn round_to(self, places: i32) -> Self {
        Self {
            value: round_to(self.value, places),
            ..self
        }
    }
}

impl Round for LabValue {
    fn round_to(self, places: i32) -> LabValue {
        Self {
            l: round_to(self.l, places),
            a: round_to(self.a, places),
            b: round_to(self.b, places),
        }
    }
}

impl Round for LchValue {
    fn round_to(self, places: i32) -> LchValue {
        Self {
            l: round_to(self.l, places),
            c: round_to(self.c, places),
            h: round_to(self.h, places),
        }
    }
}

impl Round for XyzValue {
    fn round_to(self, places: i32) -> XyzValue {
        Self {
            x: round_to(self.x, places),
            y: round_to(self.y, places),
            z: round_to(self.z, places),
        }
    }
}

#[test]
fn round() {
    let val = 1.234567890;
    let rnd = round::round_to(val, 4);
    assert_eq!(rnd, 1.2346);
    assert_ne!(rnd, val);
}

