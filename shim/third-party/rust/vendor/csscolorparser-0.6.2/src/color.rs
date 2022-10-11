use std::convert::TryFrom;
#[cfg(feature = "lab")]
use std::f64::consts::{PI, TAU};
use std::fmt;
use std::str::FromStr;

#[cfg(feature = "rust-rgb")]
use rgb::{RGB, RGBA};
#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::{parse, ParseColorError};

#[cfg(feature = "named-colors")]
use crate::parser::NAMED_COLORS;

#[cfg(feature = "lab")]
const PI_3: f64 = PI * 3.0;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
/// The color
pub struct Color {
    /// Red
    pub r: f64,
    /// Green
    pub g: f64,
    /// Blue
    pub b: f64,
    /// Alpha
    pub a: f64,
}

impl Color {
    /// Arguments:
    ///
    /// * `r`: Red value [0..1]
    /// * `g`: Green value [0..1]
    /// * `b`: Blue value [0..1]
    /// * `a`: Alpha value [0..1]
    pub const fn new(r: f64, g: f64, b: f64, a: f64) -> Self {
        Self { r, g, b, a }
    }

    pub fn to_array(&self) -> [f64; 4] {
        [self.r, self.g, self.b, self.a]
    }

    pub fn to_rgba8(&self) -> [u8; 4] {
        [
            (self.r * 255.0 + 0.5) as u8,
            (self.g * 255.0 + 0.5) as u8,
            (self.b * 255.0 + 0.5) as u8,
            (self.a * 255.0 + 0.5) as u8,
        ]
    }

    pub fn to_rgba16(&self) -> [u16; 4] {
        [
            (self.r * 65535.0 + 0.5) as u16,
            (self.g * 65535.0 + 0.5) as u16,
            (self.b * 65535.0 + 0.5) as u16,
            (self.a * 65535.0 + 0.5) as u16,
        ]
    }

    pub fn clamp(&self) -> Self {
        Self {
            r: self.r.clamp(0.0, 1.0),
            g: self.g.clamp(0.0, 1.0),
            b: self.b.clamp(0.0, 1.0),
            a: self.a.clamp(0.0, 1.0),
        }
    }

    #[deprecated = "Use [new](#method.new) instead."]
    /// Arguments:
    ///
    /// * `r`: Red value [0..1]
    /// * `g`: Green value [0..1]
    /// * `b`: Blue value [0..1]
    pub fn from_rgb(r: f64, g: f64, b: f64) -> Self {
        Self { r, g, b, a: 1.0 }
    }

    #[deprecated = "Use [new](#method.new) instead."]
    /// Arguments:
    ///
    /// * `r`: Red value [0..1]
    /// * `g`: Green value [0..1]
    /// * `b`: Blue value [0..1]
    /// * `a`: Alpha value [0..1]
    pub fn from_rgba(r: f64, g: f64, b: f64, a: f64) -> Self {
        Self { r, g, b, a }
    }

    #[deprecated = "Use [from_rgba8](#method.from_rgba8) instead."]
    /// Arguments:
    ///
    /// * `r`: Red value [0..255]
    /// * `g`: Green value [0..255]
    /// * `b`: Blue value [0..255]
    pub fn from_rgb_u8(r: u8, g: u8, b: u8) -> Self {
        Self {
            r: r as f64 / 255.0,
            g: g as f64 / 255.0,
            b: b as f64 / 255.0,
            a: 1.0,
        }
    }

    #[deprecated = "Use [from_rgba8](#method.from_rgba8) instead."]
    /// Arguments:
    ///
    /// * `r`: Red value [0..255]
    /// * `g`: Green value [0..255]
    /// * `b`: Blue value [0..255]
    /// * `a`: Alpha value [0..255]
    pub fn from_rgba_u8(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self {
            r: r as f64 / 255.0,
            g: g as f64 / 255.0,
            b: b as f64 / 255.0,
            a: a as f64 / 255.0,
        }
    }

    /// Arguments:
    ///
    /// * `r`: Red value [0..255]
    /// * `g`: Green value [0..255]
    /// * `b`: Blue value [0..255]
    /// * `a`: Alpha value [0..255]
    pub fn from_rgba8(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self {
            r: r as f64 / 255.0,
            g: g as f64 / 255.0,
            b: b as f64 / 255.0,
            a: a as f64 / 255.0,
        }
    }

    #[deprecated = "Use [from_linear_rgba](#method.from_linear_rgba) instead."]
    /// Arguments:
    ///
    /// * `r`: Red value [0..1]
    /// * `g`: Green value [0..1]
    /// * `b`: Blue value [0..1]
    pub fn from_linear_rgb(r: f64, g: f64, b: f64) -> Self {
        Self::from_linear_rgba(r, g, b, 1.0)
    }

    /// Arguments:
    ///
    /// * `r`: Red value [0..1]
    /// * `g`: Green value [0..1]
    /// * `b`: Blue value [0..1]
    /// * `a`: Alpha value [0..1]
    pub fn from_linear_rgba(r: f64, g: f64, b: f64, a: f64) -> Self {
        fn from_linear(x: f64) -> f64 {
            if x >= 0.0031308 {
                return 1.055 * x.powf(1.0 / 2.4) - 0.055;
            }
            12.92 * x
        }
        Self::new(from_linear(r), from_linear(g), from_linear(b), a)
    }

    #[deprecated = "Use [from_linear_rgba8](#method.from_linear_rgba8) instead."]
    /// Arguments:
    ///
    /// * `r`: Red value [0..255]
    /// * `g`: Green value [0..255]
    /// * `b`: Blue value [0..255]
    pub fn from_linear_rgb_u8(r: u8, g: u8, b: u8) -> Self {
        Self::from_linear_rgba(r as f64 / 255.0, g as f64 / 255.0, b as f64 / 255.0, 1.0)
    }

    #[deprecated = "Use [from_linear_rgba8](#method.from_linear_rgba8) instead."]
    /// Arguments:
    ///
    /// * `r`: Red value [0..255]
    /// * `g`: Green value [0..255]
    /// * `b`: Blue value [0..255]
    /// * `a`: Alpha value [0..255]
    pub fn from_linear_rgba_u8(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self::from_linear_rgba(
            r as f64 / 255.0,
            g as f64 / 255.0,
            b as f64 / 255.0,
            a as f64 / 255.0,
        )
    }

    /// Arguments:
    ///
    /// * `r`: Red value [0..255]
    /// * `g`: Green value [0..255]
    /// * `b`: Blue value [0..255]
    /// * `a`: Alpha value [0..255]
    pub fn from_linear_rgba8(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self::from_linear_rgba(
            r as f64 / 255.0,
            g as f64 / 255.0,
            b as f64 / 255.0,
            a as f64 / 255.0,
        )
    }

    #[deprecated = "Use [from_hsva](#method.from_hsva) instead."]
    /// Arguments:
    ///
    /// * `h`: Hue angle [0..360]
    /// * `s`: Saturation [0..1]
    /// * `v`: Value [0..1]
    pub fn from_hsv(h: f64, s: f64, v: f64) -> Self {
        Self::from_hsva(h, s, v, 1.0)
    }

    /// Arguments:
    ///
    /// * `h`: Hue angle [0..360]
    /// * `s`: Saturation [0..1]
    /// * `v`: Value [0..1]
    /// * `a`: Alpha [0..1]
    pub fn from_hsva(h: f64, s: f64, v: f64, a: f64) -> Self {
        let (r, g, b) = hsv_to_rgb(normalize_angle(h), clamp0_1(s), clamp0_1(v));
        Self::new(clamp0_1(r), clamp0_1(g), clamp0_1(b), clamp0_1(a))
    }

    #[deprecated = "Use [from_hsla](#method.from_hsla) instead."]
    /// Arguments:
    ///
    /// * `h`: Hue angle [0..360]
    /// * `s`: Saturation [0..1]
    /// * `l`: Lightness [0..1]
    pub fn from_hsl(h: f64, s: f64, l: f64) -> Self {
        Self::from_hsla(h, s, l, 1.0)
    }

    /// Arguments:
    ///
    /// * `h`: Hue angle [0..360]
    /// * `s`: Saturation [0..1]
    /// * `l`: Lightness [0..1]
    /// * `a`: Alpha [0..1]
    pub fn from_hsla(h: f64, s: f64, l: f64, a: f64) -> Self {
        let (r, g, b) = hsl_to_rgb(normalize_angle(h), clamp0_1(s), clamp0_1(l));
        Self::new(clamp0_1(r), clamp0_1(g), clamp0_1(b), clamp0_1(a))
    }

    #[deprecated = "Use [from_hwba](#method.from_hwba) instead."]
    /// Arguments:
    ///
    /// * `h`: Hue angle [0..360]
    /// * `w`: Whiteness [0..1]
    /// * `b`: Blackness [0..1]
    pub fn from_hwb(h: f64, w: f64, b: f64) -> Self {
        Self::from_hwba(h, w, b, 1.0)
    }

    /// Arguments:
    ///
    /// * `h`: Hue angle [0..360]
    /// * `w`: Whiteness [0..1]
    /// * `b`: Blackness [0..1]
    /// * `a`: Alpha [0..1]
    pub fn from_hwba(h: f64, w: f64, b: f64, a: f64) -> Self {
        let (r, g, b) = hwb_to_rgb(normalize_angle(h), clamp0_1(w), clamp0_1(b));
        Self::new(clamp0_1(r), clamp0_1(g), clamp0_1(b), a)
    }

    #[deprecated = "Use [from_oklaba](#method.from_oklaba) instead."]
    /// Arguments:
    ///
    /// * `l`: Perceived lightness
    /// * `a`: How green/red the color is
    /// * `b`: How blue/yellow the color is
    pub fn from_oklab(l: f64, a: f64, b: f64) -> Self {
        Self::from_oklaba(l, a, b, 1.0)
    }

    /// Arguments:
    ///
    /// * `l`: Perceived lightness
    /// * `a`: How green/red the color is
    /// * `b`: How blue/yellow the color is
    /// * `alpha`: Alpha [0..1]
    pub fn from_oklaba(l: f64, a: f64, b: f64, alpha: f64) -> Self {
        let l_ = (l + 0.3963377774 * a + 0.2158037573 * b).powi(3);
        let m_ = (l - 0.1055613458 * a - 0.0638541728 * b).powi(3);
        let s_ = (l - 0.0894841775 * a - 1.2914855480 * b).powi(3);

        let r = 4.0767245293 * l_ - 3.3072168827 * m_ + 0.2307590544 * s_;
        let g = -1.2681437731 * l_ + 2.6093323231 * m_ - 0.3411344290 * s_;
        let b = -0.0041119885 * l_ - 0.7034763098 * m_ + 1.7068625689 * s_;

        Self::from_linear_rgba(r, g, b, alpha)
    }

    #[cfg(feature = "lab")]
    /// Arguments:
    ///
    /// * `l`: Lightness
    /// * `a`: Distance along the `a` axis
    /// * `b`: Distance along the `b` axis
    /// * `alpha`: Alpha [0..1]
    pub fn from_lab(l: f64, a: f64, b: f64, alpha: f64) -> Self {
        let [r, g, b] = lab::Lab {
            l: l as f32,
            a: a as f32,
            b: b as f32,
        }
        .to_rgb_normalized();
        Self::new(r as f64, g as f64, b as f64, alpha)
    }

    #[cfg(feature = "lab")]
    /// Returns: `(l, a, b, alpha)`
    pub fn to_lab(&self) -> (f64, f64, f64, f64) {
        let lab = lab::Lab::from_rgb_normalized(&[self.r as f32, self.g as f32, self.b as f32]);
        (lab.l as f64, lab.a as f64, lab.b as f64, self.a)
    }

    #[cfg(feature = "lab")]
    /// Blend this color with the other one, in the Lab color-space. `t` in the range [0..1].
    pub fn interpolate_lab(&self, other: &Color, t: f64) -> Self {
        let (l1, a1, b1, alpha1) = self.to_lab();
        let (l2, a2, b2, alpha2) = other.to_lab();
        Self::from_lab(
            l1 + t * (l2 - l1),
            a1 + t * (a2 - a1),
            b1 + t * (b2 - b1),
            alpha1 + t * (alpha2 - alpha1),
        )
    }

    #[cfg(feature = "lab")]
    /// Arguments:
    ///
    /// * `l`: Lightness
    /// * `c`: Chroma
    /// * `h`: Hue angle in radians
    /// * `alpha`: Alpha [0..1]
    pub fn from_lch(l: f64, c: f64, h: f64, alpha: f64) -> Self {
        let [r, g, b] = lab::LCh {
            l: l as f32,
            c: c as f32,
            h: h as f32,
        }
        .to_lab()
        .to_rgb_normalized();
        Self::new(r as f64, g as f64, b as f64, alpha)
    }

    #[cfg(feature = "lab")]
    /// Returns: `(l, c, h, alpha)`
    pub fn to_lch(&self) -> (f64, f64, f64, f64) {
        let lch = lab::LCh::from_lab(lab::Lab::from_rgb_normalized(&[
            self.r as f32,
            self.g as f32,
            self.b as f32,
        ]));
        (lch.l as f64, lch.c as f64, lch.h as f64, self.a)
    }

    #[cfg(feature = "lab")]
    /// Blend this color with the other one, in the LCH color-space. `t` in the range [0..1].
    pub fn interpolate_lch(&self, other: &Color, t: f64) -> Self {
        let (l1, c1, h1, alpha1) = self.to_lch();
        let (l2, c2, h2, alpha2) = other.to_lch();
        Self::from_lch(
            l1 + t * (l2 - l1),
            c1 + t * (c2 - c1),
            interp_angle_rad(h1, h2, t),
            alpha1 + t * (alpha2 - alpha1),
        )
    }

    /// Create color from CSS color string.
    ///
    /// # Examples
    /// ```
    /// use csscolorparser::Color;
    /// # use std::error::Error;
    /// # fn main() -> Result<(), Box<dyn Error>> {
    ///
    /// let c = Color::from_html("rgb(255,0,0)")?;
    ///
    /// assert_eq!(c.to_array(), [1.0, 0.0, 0.0, 1.0]);
    /// assert_eq!(c.to_rgba8(), [255, 0, 0, 255]);
    /// assert_eq!(c.to_hex_string(), "#ff0000");
    /// assert_eq!(c.to_rgb_string(), "rgb(255,0,0)");
    /// # Ok(())
    /// # }
    /// ```
    pub fn from_html<S: AsRef<str>>(s: S) -> Result<Self, ParseColorError> {
        parse(s.as_ref())
    }

    #[cfg(feature = "named-colors")]
    pub fn name(&self) -> Option<&'static str> {
        let rgb = &self.to_rgba8()[0..3];
        for (&k, &v) in NAMED_COLORS.entries() {
            if v == rgb {
                return Some(k);
            }
        }
        None
    }

    #[deprecated]
    /// Returns: `(r, g, b, a)`
    ///
    /// * Red, green, blue and alpha in the range [0..1]
    pub fn rgba(&self) -> (f64, f64, f64, f64) {
        (self.r, self.g, self.b, self.a)
    }

    #[deprecated = "Use [to_rgba8](#method.to_rgba8) instead."]
    /// Returns: `(r, g, b, a)`
    ///
    /// * Red, green, blue and alpha in the range [0..255]
    pub fn rgba_u8(&self) -> (u8, u8, u8, u8) {
        (
            (self.r * 255.0).round() as u8,
            (self.g * 255.0).round() as u8,
            (self.b * 255.0).round() as u8,
            (self.a * 255.0).round() as u8,
        )
    }

    /// Returns: `(h, s, v, a)`
    ///
    /// * `h`: Hue angle [0..360]
    /// * `s`: Saturation [0..1]
    /// * `v`: Value [0..1]
    /// * `a`: Alpha [0..1]
    pub fn to_hsva(&self) -> (f64, f64, f64, f64) {
        let (h, s, v) = rgb_to_hsv(self.r, self.g, self.b);
        (h, s, v, self.a)
    }

    /// Returns: `(h, s, l, a)`
    ///
    /// * `h`: Hue angle [0..360]
    /// * `s`: Saturation [0..1]
    /// * `l`: Lightness [0..1]
    /// * `a`: Alpha [0..1]
    pub fn to_hsla(&self) -> (f64, f64, f64, f64) {
        let (h, s, l) = rgb_to_hsl(self.r, self.g, self.b);
        (h, s, l, self.a)
    }

    /// Returns: `(h, w, b, a)`
    ///
    /// * `h`: Hue angle [0..360]
    /// * `w`: Whiteness [0..1]
    /// * `b`: Blackness [0..1]
    /// * `a`: Alpha [0..1]
    pub fn to_hwba(&self) -> (f64, f64, f64, f64) {
        let (h, w, b) = rgb_to_hwb(self.r, self.g, self.b);
        (h, w, b, self.a)
    }

    /// Returns: `(r, g, b, a)`
    ///
    /// * Red, green, blue and alpha in the range [0..1]
    pub fn to_linear_rgba(&self) -> (f64, f64, f64, f64) {
        fn to_linear(x: f64) -> f64 {
            if x >= 0.04045 {
                return ((x + 0.055) / 1.055).powf(2.4);
            }
            x / 12.92
        }
        (
            to_linear(self.r),
            to_linear(self.g),
            to_linear(self.b),
            self.a,
        )
    }

    /// Returns: `(r, g, b, a)`
    ///
    /// * Red, green, blue and alpha in the range [0..255]
    pub fn to_linear_rgba_u8(&self) -> (u8, u8, u8, u8) {
        let (r, g, b, a) = self.to_linear_rgba();
        (
            (r * 255.0).round() as u8,
            (g * 255.0).round() as u8,
            (b * 255.0).round() as u8,
            (a * 255.0).round() as u8,
        )
    }

    /// Returns: `(l, a, b, alpha)`
    pub fn to_oklaba(&self) -> (f64, f64, f64, f64) {
        let (r, g, b, _) = self.to_linear_rgba();
        let l_ = (0.4121656120 * r + 0.5362752080 * g + 0.0514575653 * b).cbrt();
        let m_ = (0.2118591070 * r + 0.6807189584 * g + 0.1074065790 * b).cbrt();
        let s_ = (0.0883097947 * r + 0.2818474174 * g + 0.6302613616 * b).cbrt();
        let l = 0.2104542553 * l_ + 0.7936177850 * m_ - 0.0040720468 * s_;
        let a = 1.9779984951 * l_ - 2.4285922050 * m_ + 0.4505937099 * s_;
        let b = 0.0259040371 * l_ + 0.7827717662 * m_ - 0.8086757660 * s_;
        (l, a, b, self.a)
    }

    /// Get the RGB hexadecimal color string.
    pub fn to_hex_string(&self) -> String {
        let [r, g, b, a] = self.to_rgba8();

        if a < 255 {
            return format!("#{:02x}{:02x}{:02x}{:02x}", r, g, b, a);
        }

        format!("#{:02x}{:02x}{:02x}", r, g, b)
    }

    /// Get the CSS `rgb()` format string.
    pub fn to_rgb_string(&self) -> String {
        let [r, g, b, _] = self.to_rgba8();

        if self.a < 1.0 {
            return format!("rgba({},{},{},{})", r, g, b, self.a);
        }

        format!("rgb({},{},{})", r, g, b)
    }

    /// Blend this color with the other one, in the RGB color-space. `t` in the range [0..1].
    pub fn interpolate_rgb(&self, other: &Color, t: f64) -> Self {
        Self {
            r: self.r + t * (other.r - self.r),
            g: self.g + t * (other.g - self.g),
            b: self.b + t * (other.b - self.b),
            a: self.a + t * (other.a - self.a),
        }
    }

    /// Blend this color with the other one, in the linear RGB color-space. `t` in the range [0..1].
    pub fn interpolate_linear_rgb(&self, other: &Color, t: f64) -> Self {
        let (r1, g1, b1, a1) = self.to_linear_rgba();
        let (r2, g2, b2, a2) = other.to_linear_rgba();
        Self::from_linear_rgba(
            r1 + t * (r2 - r1),
            g1 + t * (g2 - g1),
            b1 + t * (b2 - b1),
            a1 + t * (a2 - a1),
        )
    }

    /// Blend this color with the other one, in the HSV color-space. `t` in the range [0..1].
    pub fn interpolate_hsv(&self, other: &Color, t: f64) -> Self {
        let (h1, s1, v1, a1) = self.to_hsva();
        let (h2, s2, v2, a2) = other.to_hsva();
        Self::from_hsva(
            interp_angle(h1, h2, t),
            s1 + t * (s2 - s1),
            v1 + t * (v2 - v1),
            a1 + t * (a2 - a1),
        )
    }

    /// Blend this color with the other one, in the [Oklab](https://bottosson.github.io/posts/oklab/) color-space. `t` in the range [0..1].
    pub fn interpolate_oklab(&self, other: &Color, t: f64) -> Self {
        let (l1, a1, b1, alpha1) = self.to_oklaba();
        let (l2, a2, b2, alpha2) = other.to_oklaba();
        Self::from_oklaba(
            l1 + t * (l2 - l1),
            a1 + t * (a2 - a1),
            b1 + t * (b2 - b1),
            alpha1 + t * (alpha2 - alpha1),
        )
    }
}

impl Default for Color {
    fn default() -> Self {
        Self {
            r: 0.0,
            g: 0.0,
            b: 0.0,
            a: 1.0,
        }
    }
}

#[cfg(feature = "cint")]
mod impl_cint {
    use super::*;
    use cint::{Alpha, ColorInterop, EncodedSrgb};

    impl ColorInterop for Color {
        type CintTy = Alpha<EncodedSrgb<f64>>;
    }

    impl From<Color> for EncodedSrgb<f64> {
        fn from(c: Color) -> Self {
            let Color { r, g, b, a: _ } = c;
            EncodedSrgb { r, g, b }
        }
    }

    impl From<EncodedSrgb<f64>> for Color {
        fn from(c: EncodedSrgb<f64>) -> Self {
            let EncodedSrgb { r, g, b } = c;
            Self::new(r, g, b, 1.0)
        }
    }

    impl From<Color> for EncodedSrgb<f32> {
        fn from(c: Color) -> Self {
            let Color { r, g, b, a: _ } = c;
            let (r, g, b) = (r as f32, g as f32, b as f32);
            EncodedSrgb { r, g, b }
        }
    }

    impl From<EncodedSrgb<f32>> for Color {
        fn from(c: EncodedSrgb<f32>) -> Self {
            let EncodedSrgb { r, g, b } = c;
            let (r, g, b) = (r as f64, g as f64, b as f64);
            Self::new(r, g, b, 1.0)
        }
    }

    impl From<Color> for Alpha<EncodedSrgb<f64>> {
        fn from(c: Color) -> Self {
            let Color { r, g, b, a } = c;
            Alpha {
                color: EncodedSrgb { r, g, b },
                alpha: a,
            }
        }
    }

    impl From<Alpha<EncodedSrgb<f64>>> for Color {
        fn from(c: Alpha<EncodedSrgb<f64>>) -> Self {
            let Alpha {
                color: EncodedSrgb { r, g, b },
                alpha,
            } = c;
            Self::new(r, g, b, alpha)
        }
    }

    impl From<Color> for Alpha<EncodedSrgb<f32>> {
        fn from(c: Color) -> Self {
            let Color { r, g, b, a } = c;
            let (r, g, b, alpha) = (r as f32, g as f32, b as f32, a as f32);
            Alpha {
                color: EncodedSrgb { r, g, b },
                alpha,
            }
        }
    }

    impl From<Alpha<EncodedSrgb<f32>>> for Color {
        fn from(c: Alpha<EncodedSrgb<f32>>) -> Self {
            let Alpha {
                color: EncodedSrgb { r, g, b },
                alpha,
            } = c;
            let (r, g, b, alpha) = (r as f64, g as f64, b as f64, alpha as f64);
            Self::new(r, g, b, alpha)
        }
    }

    impl From<Color> for EncodedSrgb<u8> {
        fn from(c: Color) -> Self {
            let [r, g, b, _] = c.to_rgba8();
            EncodedSrgb { r, g, b }
        }
    }

    impl From<EncodedSrgb<u8>> for Color {
        fn from(c: EncodedSrgb<u8>) -> Self {
            let EncodedSrgb { r, g, b } = c;
            Self::from_rgba8(r, g, b, 255)
        }
    }

    impl From<Color> for Alpha<EncodedSrgb<u8>> {
        fn from(c: Color) -> Self {
            let [r, g, b, alpha] = c.to_rgba8();
            Alpha {
                color: EncodedSrgb { r, g, b },
                alpha,
            }
        }
    }

    impl From<Alpha<EncodedSrgb<u8>>> for Color {
        fn from(c: Alpha<EncodedSrgb<u8>>) -> Self {
            let Alpha {
                color: EncodedSrgb { r, g, b },
                alpha,
            } = c;
            Self::from_rgba8(r, g, b, alpha)
        }
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RGBA({},{},{},{})", self.r, self.g, self.b, self.a)
    }
}

impl FromStr for Color {
    type Err = ParseColorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse(s)
    }
}

impl TryFrom<&str> for Color {
    type Error = ParseColorError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        parse(s)
    }
}

impl From<(f64, f64, f64, f64)> for Color {
    fn from((r, g, b, a): (f64, f64, f64, f64)) -> Self {
        Self { r, g, b, a }
    }
}

impl From<(f64, f64, f64)> for Color {
    fn from((r, g, b): (f64, f64, f64)) -> Self {
        Self { r, g, b, a: 1.0 }
    }
}

impl From<[f64; 4]> for Color {
    fn from([r, g, b, a]: [f64; 4]) -> Self {
        Self { r, g, b, a }
    }
}

impl From<[f64; 3]> for Color {
    fn from([r, g, b]: [f64; 3]) -> Self {
        Self { r, g, b, a: 1.0 }
    }
}

impl From<[f32; 4]> for Color {
    fn from([r, g, b, a]: [f32; 4]) -> Self {
        Self {
            r: r as f64,
            g: g as f64,
            b: b as f64,
            a: a as f64,
        }
    }
}

impl From<[f32; 3]> for Color {
    fn from([r, g, b]: [f32; 3]) -> Self {
        Self {
            r: r as f64,
            g: g as f64,
            b: b as f64,
            a: 1.0,
        }
    }
}

impl From<(u8, u8, u8, u8)> for Color {
    fn from((r, g, b, a): (u8, u8, u8, u8)) -> Self {
        Self::from_rgba8(r, g, b, a)
    }
}

impl From<(u8, u8, u8)> for Color {
    fn from((r, g, b): (u8, u8, u8)) -> Self {
        Self::from_rgba8(r, g, b, 255)
    }
}

impl From<[u8; 4]> for Color {
    fn from([r, g, b, a]: [u8; 4]) -> Self {
        Self::from_rgba8(r, g, b, a)
    }
}

impl From<[u8; 3]> for Color {
    fn from([r, g, b]: [u8; 3]) -> Self {
        Self::from_rgba8(r, g, b, 255)
    }
}

/// Convert rust-rgb's `RGB<f64>` type into `Color`.
#[cfg(feature = "rust-rgb")]
impl From<RGB<f64>> for Color {
    fn from(item: RGB<f64>) -> Self {
        Self::new(item.r, item.g, item.b, 1.0)
    }
}

/// Convert rust-rgb's `RGBA<f64>` type into `Color`.
#[cfg(feature = "rust-rgb")]
impl From<RGBA<f64>> for Color {
    fn from(item: RGBA<f64>) -> Self {
        Self::new(item.r, item.g, item.b, item.a)
    }
}

/// Implement Serde serialization into HEX string
#[cfg(feature = "serde")]
impl Serialize for Color {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.to_hex_string())
    }
}

/// Implement Serde deserialization from string
#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for Color {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let string = String::deserialize(deserializer)?;
        Self::from_str(&string).map_err(serde::de::Error::custom)
    }
}

fn hue_to_rgb(n1: f64, n2: f64, h: f64) -> f64 {
    let h = modulo(h, 6.0);

    if h < 1.0 {
        return n1 + ((n2 - n1) * h);
    }

    if h < 3.0 {
        return n2;
    }

    if h < 4.0 {
        return n1 + ((n2 - n1) * (4.0 - h));
    }

    n1
}

// h = 0..360
// s, l = 0..1
// r, g, b = 0..1
fn hsl_to_rgb(h: f64, s: f64, l: f64) -> (f64, f64, f64) {
    if s == 0.0 {
        return (l, l, l);
    }

    let n2 = if l < 0.5 {
        l * (1.0 + s)
    } else {
        l + s - (l * s)
    };

    let n1 = 2.0 * l - n2;
    let h = h / 60.0;
    let r = hue_to_rgb(n1, n2, h + 2.0);
    let g = hue_to_rgb(n1, n2, h);
    let b = hue_to_rgb(n1, n2, h - 2.0);
    (r, g, b)
}

fn hwb_to_rgb(hue: f64, white: f64, black: f64) -> (f64, f64, f64) {
    if white + black >= 1.0 {
        let l = white / (white + black);
        return (l, l, l);
    }

    let (r, g, b) = hsl_to_rgb(hue, 1.0, 0.5);
    let r = r * (1.0 - white - black) + white;
    let g = g * (1.0 - white - black) + white;
    let b = b * (1.0 - white - black) + white;
    (r, g, b)
}

#[allow(clippy::float_cmp)]
fn hsv_to_hsl(h: f64, s: f64, v: f64) -> (f64, f64, f64) {
    let l = (2.0 - s) * v / 2.0;

    let s = if l != 0.0 {
        if l == 1.0 {
            0.0
        } else if l < 0.5 {
            s * v / (l * 2.0)
        } else {
            s * v / (2.0 - l * 2.0)
        }
    } else {
        s
    };

    (h, s, l)
}

fn hsv_to_rgb(h: f64, s: f64, v: f64) -> (f64, f64, f64) {
    let (h, s, l) = hsv_to_hsl(h, s, v);
    hsl_to_rgb(h, s, l)
}

#[allow(clippy::float_cmp)]
fn rgb_to_hsv(r: f64, g: f64, b: f64) -> (f64, f64, f64) {
    let v = r.max(g.max(b));
    let d = v - r.min(g.min(b));

    if d == 0.0 {
        return (0.0, 0.0, v);
    }

    let s = d / v;
    let dr = (v - r) / d;
    let dg = (v - g) / d;
    let db = (v - b) / d;

    let h = if r == v {
        db - dg
    } else if g == v {
        2.0 + dr - db
    } else {
        4.0 + dg - dr
    };

    let h = (h * 60.0) % 360.0;
    (normalize_angle(h), s, v)
}

#[allow(clippy::float_cmp)]
fn rgb_to_hsl(r: f64, g: f64, b: f64) -> (f64, f64, f64) {
    let min = r.min(g.min(b));
    let max = r.max(g.max(b));
    let l = (max + min) / 2.0;

    if min == max {
        return (0.0, 0.0, l);
    }

    let d = max - min;

    let s = if l < 0.5 {
        d / (max + min)
    } else {
        d / (2.0 - max - min)
    };

    let dr = (max - r) / d;
    let dg = (max - g) / d;
    let db = (max - b) / d;

    let h = if r == max {
        db - dg
    } else if g == max {
        2.0 + dr - db
    } else {
        4.0 + dg - dr
    };

    let h = (h * 60.0) % 360.0;
    (normalize_angle(h), s, l)
}

fn rgb_to_hwb(r: f64, g: f64, b: f64) -> (f64, f64, f64) {
    let (hue, _, _) = rgb_to_hsl(r, g, b);
    let white = r.min(g.min(b));
    let black = 1.0 - r.max(g.max(b));
    (hue, white, black)
}

#[inline]
fn normalize_angle(t: f64) -> f64 {
    let mut t = t % 360.0;
    if t < 0.0 {
        t += 360.0;
    }
    t
}

#[inline]
fn interp_angle(a0: f64, a1: f64, t: f64) -> f64 {
    let delta = (((a1 - a0) % 360.0) + 540.0) % 360.0 - 180.0;
    (a0 + t * delta + 360.0) % 360.0
}

#[cfg(feature = "lab")]
#[inline]
fn interp_angle_rad(a0: f64, a1: f64, t: f64) -> f64 {
    let delta = (((a1 - a0) % TAU) + PI_3) % TAU - PI;
    (a0 + t * delta + TAU) % TAU
}

#[inline]
fn clamp0_1(t: f64) -> f64 {
    t.clamp(0.0, 1.0)
}

#[inline]
fn modulo(x: f64, n: f64) -> f64 {
    (x % n + n) % n
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_angle() {
        let data = vec![
            (0.0, 0.0),
            (360.0, 0.0),
            (400.0, 40.0),
            (1155.0, 75.0),
            (-360.0, 0.0),
            (-90.0, 270.0),
            (-765.0, 315.0),
        ];
        for (x, expected) in data {
            let c = normalize_angle(x);
            assert_eq!(expected, c);
        }
    }

    #[test]
    fn test_interp_angle() {
        let data = vec![
            ((0.0, 360.0, 0.5), 0.0),
            ((360.0, 90.0, 0.0), 0.0),
            ((360.0, 90.0, 0.5), 45.0),
            ((360.0, 90.0, 1.0), 90.0),
        ];
        for ((a, b, t), expected) in data {
            let v = interp_angle(a, b, t);
            assert_eq!(expected, v);
        }
    }

    #[cfg(feature = "rust-rgb")]
    #[test]
    fn test_convert_rust_rgb_to_color() {
        let rgb = RGB::new(0.0, 0.5, 1.0);
        assert_eq!(Color::new(0.0, 0.5, 1.0, 1.0), Color::from(rgb));

        let rgba = RGBA::new(1.0, 0.5, 0.0, 0.5);
        assert_eq!(Color::new(1.0, 0.5, 0.0, 0.5), Color::from(rgba));
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_serde_serialize_to_hex() {
        let color = Color::new(1.0, 1.0, 0.5, 0.5);
        serde_test::assert_ser_tokens(&color, &[serde_test::Token::Str("#ffff8080")]);
    }

    #[cfg(all(feature = "serde", feature = "named-colors"))]
    #[test]
    fn test_serde_deserialize_from_string() {
        let named = Color::new(1.0, 1.0, 0.0, 1.0);
        serde_test::assert_de_tokens(&named, &[serde_test::Token::Str("yellow")]);

        let hex = Color::new(0.0, 1.0, 0.0, 1.0);
        serde_test::assert_de_tokens(&hex, &[serde_test::Token::Str("#00ff00ff")]);

        let rgb = Color::new(0.0, 1.0, 0.0, 1.0);
        serde_test::assert_de_tokens(&rgb, &[serde_test::Token::Str("rgba(0,255,0,1)")]);
    }
}
