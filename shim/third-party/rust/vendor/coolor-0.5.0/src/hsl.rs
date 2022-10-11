use crate::*;

/// HSL color
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Hsl {
    /// hue in `[0,360[`
    pub h: f32,
    /// saturation in `[0,1]`
    pub s: f32,
    /// luminosity in `[0,1]`
    pub l: f32,
}

impl Hsl {
    /// Create a new HSL color from its components
    pub fn new(h: f32, s: f32, l: f32) -> Self {
        debug_assert!(h >= 0.0 && h < 360.0);
        debug_assert!(s >= 0.0 && s <= 1.0);
        debug_assert!(l >= 0.0 && l <= 1.0);
        Self { h, s, l }
    }
    /// Create a new HSL color from its components, checking the ranges
    pub fn checked(h: f32, s: f32, l: f32) -> Result<Self, CoolorError> {
        if !(h >= 0.0 && h < 360.0 && s >= 0.0 && s <= 1.0 && l >= 0.0 && l <= 1.0) {
            Ok(Self { h, s, l })
        } else {
            Err(CoolorError::InvalidHsl(h, s, l))
        }
    }
    pub fn mix(c1: Self, w1: f32, c2: Self, w2: f32) -> Self {
        debug_assert!(w1 + w2 > 0.0);
        let h = if dist(c1.h, c2.h) > 180.0 {
            // the shortest path involve crossing Tau
            let (h1, h2) = if c1.h < c2.h {
                (c1.h + 360.0, c2.h)
            } else {
                (c1.h, c2.h + 360.0)
            };
            ((w1 * h1 + w2 * h2) / (w1 + w2)) % 360.0
        } else {
            // direct way
            (w1 * c1.h + w2 * c2.h) / (w1 + w2)
        };
        //let h = (w1*c1.h + w2*c2.h) / (w1+w2);
        let s = (w1 * c1.s + w2 * c2.s) / (w1 + w2);
        let l = (w1 * c1.l + w2 * c2.l) / (w1 + w2);
        Self { h, s, l }
    }
    /// Return the nearest ANSI color
    ///
    /// This is a slow function as it literally tries all
    /// ANSI colors and picks the nearest one.
    pub fn to_ansi(self) -> AnsiColor {
        let mut best = AnsiColor { code: 16 };
        let mut smallest_distance: f32 = self.distance_to(best);
        for code in 17..=255 {
            let color = AnsiColor { code };
            let distance = self.distance_to(color);
            if distance < smallest_distance {
                best = color;
                smallest_distance = distance;
            }
        }
        best
    }
    pub fn to_rgb(self) -> Rgb {
        let h = self.h / 360.0;
        let s = self.s;
        let l = self.l;
        let rgb = if s == 0.0 {
            (l, l, l)
        } else {
            let v2 = if l < 0.5 {
                l * (1.0 + s)
            } else {
                l + s - (s * l)
            };
            let v1 = 2.0 * l - v2;
            (
                hue_to_rgb_component(v1, v2, h + (1.0 / 3.0)),
                hue_to_rgb_component(v1, v2, h),
                hue_to_rgb_component(v1, v2, h - (1.0 / 3.0)),
            )
        };
        rgb.into()
    }
    pub fn delta_h(self, other: Hsl) -> f32 {
        dist(self.h, other.h).min(dist(self.h, 360.0)) // it's a circle, 0==360
    }
    pub fn delta_s(self, other: Hsl) -> f32 {
        dist(self.s, other.s)
    }
    pub fn delta_l(self, other: Hsl) -> f32 {
        dist(self.l, other.l)
    }
    /// tentatively perceptual distance between the two colors,
    ///  except it's just as unscientific it can possibly be so
    ///  check it looks good before trying ot use it, at least...
    pub fn distance_to<H: Into<Hsl>>(self, other: H) -> f32 {
        let other: Hsl = other.into();
        self.delta_h(other) / 360.0 + self.delta_s(other) + self.delta_l(other)
    }
    /// Tell whether it's about the same color
    ///
    /// There's no theory behind this function, it should not
    /// be used outside of unit tests
    pub fn near(self, other: Hsl) -> bool {
        self.distance_to(other) < 0.01
    }
}

impl From<AnsiColor> for Hsl {
    fn from(ansi: AnsiColor) -> Self {
        ansi.to_hsl()
    }
}
impl From<Rgb> for Hsl {
    fn from(rgb: Rgb) -> Self {
        rgb.to_hsl()
    }
}

fn hue_to_rgb_component(v1: f32, v2: f32, vh: f32) -> f32 {
    let vh = (vh + 1.0) % 1.0;
    if 6.0 * vh < 1.0 {
        (v1 + (v2 - v1) * 6.0 * vh).min(1.0).max(0.0)
    } else if 2.0 * vh < 1.0 {
        v2
    } else if 3.0 * vh < 2.0 {
        (v1 + (v2 - v1) * ((2.0 / 3.0) - vh) * 6.0)
            .min(1.0)
            .max(0.0)
    } else {
        v1
    }
}

fn dist(a: f32, b: f32) -> f32 {
    if a < b {
        b - a
    } else {
        a - b
    }
}
