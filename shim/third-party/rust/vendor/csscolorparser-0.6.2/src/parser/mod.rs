use std::{error, fmt};

use crate::Color;

#[cfg(feature = "named-colors")]
mod named_colors;

#[cfg(feature = "named-colors")]
pub(crate) use named_colors::NAMED_COLORS;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParseColorError {
    InvalidHex,
    InvalidRgb,
    InvalidHsl,
    InvalidHwb,
    InvalidHsv,
    #[cfg(feature = "lab")]
    InvalidLab,
    #[cfg(feature = "lab")]
    InvalidLch,
    InvalidFunction,
    InvalidUnknown,
}

impl fmt::Display for ParseColorError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::InvalidHex => f.write_str("invalid hex format"),
            Self::InvalidRgb => f.write_str("invalid rgb format"),
            Self::InvalidHsl => f.write_str("invalid hsl format"),
            Self::InvalidHwb => f.write_str("invalid hwb format"),
            Self::InvalidHsv => f.write_str("invalid hsv format"),
            #[cfg(feature = "lab")]
            Self::InvalidLab => f.write_str("invalid lab format"),
            #[cfg(feature = "lab")]
            Self::InvalidLch => f.write_str("invalid lch format"),
            Self::InvalidFunction => f.write_str("invalid color function"),
            Self::InvalidUnknown => f.write_str("invalid unknown format"),
        }
    }
}

impl error::Error for ParseColorError {}

/// Parse CSS color string
///
/// # Examples
///
/// ```
/// # use std::error::Error;
/// # fn main() -> Result<(), Box<dyn Error>> {
/// let c = csscolorparser::parse("#ff0")?;
///
/// assert_eq!(c.to_array(), [1.0, 1.0, 0.0, 1.0]);
/// assert_eq!(c.to_rgba8(), [255, 255, 0, 255]);
/// assert_eq!(c.to_hex_string(), "#ffff00");
/// assert_eq!(c.to_rgb_string(), "rgb(255,255,0)");
/// # Ok(())
/// # }
/// ```
///
/// ```
/// # use std::error::Error;
/// # fn main() -> Result<(), Box<dyn Error>> {
/// let c = csscolorparser::parse("hsl(360deg,100%,50%)")?;
///
/// assert_eq!(c.to_array(), [1.0, 0.0, 0.0, 1.0]);
/// assert_eq!(c.to_rgba8(), [255, 0, 0, 255]);
/// assert_eq!(c.to_hex_string(), "#ff0000");
/// assert_eq!(c.to_rgb_string(), "rgb(255,0,0)");
/// # Ok(())
/// # }
/// ```
pub fn parse(s: &str) -> Result<Color, ParseColorError> {
    let s = s.trim().to_lowercase();

    if s == "transparent" {
        return Ok(Color::new(0.0, 0.0, 0.0, 0.0));
    }

    // Named colors
    #[cfg(feature = "named-colors")]
    if let Some([r, g, b]) = NAMED_COLORS.get(&*s) {
        return Ok(Color::from_rgba8(*r, *g, *b, 255));
    }

    // Hex format
    if let Some(s) = s.strip_prefix('#') {
        return parse_hex(s);
    }

    if let (Some(i), Some(s)) = (s.find('('), s.strip_suffix(')')) {
        let fname = &s[..i].trim_end();
        let s = &s[i + 1..].replace(',', " ").replace('/', " ");
        let params = s.split_whitespace().collect::<Vec<&str>>();
        let p_len = params.len();

        match *fname {
            "rgb" | "rgba" => {
                if p_len != 3 && p_len != 4 {
                    return Err(ParseColorError::InvalidRgb);
                }

                let r = parse_percent_or_255(params[0]);
                let g = parse_percent_or_255(params[1]);
                let b = parse_percent_or_255(params[2]);

                let a = if p_len == 4 {
                    parse_percent_or_float(params[3])
                } else {
                    Some((1.0, true))
                };

                if let (Some((r, r_fmt)), Some((g, g_fmt)), Some((b, b_fmt)), Some((a, _))) =
                    (r, g, b, a)
                {
                    if r_fmt == g_fmt && g_fmt == b_fmt {
                        return Ok(Color {
                            r: r.clamp(0.0, 1.0),
                            g: g.clamp(0.0, 1.0),
                            b: b.clamp(0.0, 1.0),
                            a: a.clamp(0.0, 1.0),
                        });
                    }
                }

                return Err(ParseColorError::InvalidRgb);
            }
            "hsl" | "hsla" => {
                if p_len != 3 && p_len != 4 {
                    return Err(ParseColorError::InvalidHsl);
                }

                let h = parse_angle(params[0]);
                let s = parse_percent_or_float(params[1]);
                let l = parse_percent_or_float(params[2]);

                let a = if p_len == 4 {
                    parse_percent_or_float(params[3])
                } else {
                    Some((1.0, true))
                };

                if let (Some(h), Some((s, s_fmt)), Some((l, l_fmt)), Some((a, _))) = (h, s, l, a) {
                    if s_fmt == l_fmt {
                        return Ok(Color::from_hsla(h, s, l, a));
                    }
                }

                return Err(ParseColorError::InvalidHsl);
            }
            "hwb" | "hwba" => {
                if p_len != 3 && p_len != 4 {
                    return Err(ParseColorError::InvalidHwb);
                }

                let h = parse_angle(params[0]);
                let w = parse_percent_or_float(params[1]);
                let b = parse_percent_or_float(params[2]);

                let a = if p_len == 4 {
                    parse_percent_or_float(params[3])
                } else {
                    Some((1.0, true))
                };

                if let (Some(h), Some((w, w_fmt)), Some((b, b_fmt)), Some((a, _))) = (h, w, b, a) {
                    if w_fmt == b_fmt {
                        return Ok(Color::from_hwba(h, w, b, a));
                    }
                }

                return Err(ParseColorError::InvalidHwb);
            }
            "hsv" | "hsva" => {
                if p_len != 3 && p_len != 4 {
                    return Err(ParseColorError::InvalidHsv);
                }

                let h = parse_angle(params[0]);
                let s = parse_percent_or_float(params[1]);
                let v = parse_percent_or_float(params[2]);

                let a = if p_len == 4 {
                    parse_percent_or_float(params[3])
                } else {
                    Some((1.0, true))
                };

                if let (Some(h), Some((s, s_fmt)), Some((v, v_fmt)), Some((a, _))) = (h, s, v, a) {
                    if s_fmt == v_fmt {
                        return Ok(Color::from_hsva(h, s, v, a));
                    }
                }

                return Err(ParseColorError::InvalidHsv);
            }
            #[cfg(feature = "lab")]
            "lab" => {
                if p_len != 3 && p_len != 4 {
                    return Err(ParseColorError::InvalidLab);
                }

                let l = parse_percent_or_float(params[0]);
                let a = parse_percent_or_float(params[1]);
                let b = parse_percent_or_float(params[2]);

                let alpha = if p_len == 4 {
                    parse_percent_or_float(params[3])
                } else {
                    Some((1.0, true))
                };

                if let (Some((l, _)), Some((a, a_fmt)), Some((b, b_fmt)), Some((alpha, _))) =
                    (l, a, b, alpha)
                {
                    let a = if a_fmt {
                        remap(a, -1.0, 1.0, -125.0, 125.0)
                    } else {
                        a
                    };
                    let b = if b_fmt {
                        remap(b, -1.0, 1.0, -125.0, 125.0)
                    } else {
                        b
                    };
                    return Ok(Color::from_lab(l.max(0.0) * 100.0, a, b, alpha));
                }

                return Err(ParseColorError::InvalidLab);
            }
            #[cfg(feature = "lab")]
            "lch" => {
                if p_len != 3 && p_len != 4 {
                    return Err(ParseColorError::InvalidLch);
                }

                let l = parse_percent_or_float(params[0]);
                let c = parse_percent_or_float(params[1]);
                let h = parse_angle(params[2]);

                let alpha = if p_len == 4 {
                    parse_percent_or_float(params[3])
                } else {
                    Some((1.0, true))
                };

                if let (Some((l, _)), Some((c, c_fmt)), Some(h), Some((alpha, _))) =
                    (l, c, h, alpha)
                {
                    let c = if c_fmt { c * 150.0 } else { c };
                    return Ok(Color::from_lch(
                        l * 100.0,
                        c.max(0.0),
                        h.to_radians(),
                        alpha,
                    ));
                }

                return Err(ParseColorError::InvalidLch);
            }
            _ => {
                return Err(ParseColorError::InvalidFunction);
            }
        }
    }

    // Hex format without prefix '#'
    if let Ok(c) = parse_hex(&s) {
        return Ok(c);
    }

    Err(ParseColorError::InvalidUnknown)
}

fn parse_hex(s: &str) -> Result<Color, ParseColorError> {
    if !s.is_ascii() {
        return Err(ParseColorError::InvalidHex);
    }

    let n = s.len();

    if n == 3 || n == 4 {
        let r =
            u8::from_str_radix(&s[0..1].repeat(2), 16).map_err(|_| ParseColorError::InvalidHex)?;
        let g =
            u8::from_str_radix(&s[1..2].repeat(2), 16).map_err(|_| ParseColorError::InvalidHex)?;
        let b =
            u8::from_str_radix(&s[2..3].repeat(2), 16).map_err(|_| ParseColorError::InvalidHex)?;

        let a = if n == 4 {
            u8::from_str_radix(&s[3..4].repeat(2), 16).map_err(|_| ParseColorError::InvalidHex)?
        } else {
            255
        };

        Ok(Color::from_rgba8(r, g, b, a))
    } else if n == 6 || n == 8 {
        let r = u8::from_str_radix(&s[0..2], 16).map_err(|_| ParseColorError::InvalidHex)?;
        let g = u8::from_str_radix(&s[2..4], 16).map_err(|_| ParseColorError::InvalidHex)?;
        let b = u8::from_str_radix(&s[4..6], 16).map_err(|_| ParseColorError::InvalidHex)?;

        let a = if n == 8 {
            u8::from_str_radix(&s[6..8], 16).map_err(|_| ParseColorError::InvalidHex)?
        } else {
            255
        };

        Ok(Color::from_rgba8(r, g, b, a))
    } else {
        Err(ParseColorError::InvalidHex)
    }
}

fn parse_percent_or_float(s: &str) -> Option<(f64, bool)> {
    s.strip_suffix('%')
        .and_then(|s| s.parse().ok().map(|t: f64| (t / 100.0, true)))
        .or_else(|| s.parse().ok().map(|t| (t, false)))
}

fn parse_percent_or_255(s: &str) -> Option<(f64, bool)> {
    s.strip_suffix('%')
        .and_then(|s| s.parse().ok().map(|t: f64| (t / 100.0, true)))
        .or_else(|| s.parse().ok().map(|t: f64| (t / 255.0, false)))
}

fn parse_angle(s: &str) -> Option<f64> {
    s.strip_suffix("deg")
        .and_then(|s| s.parse().ok())
        .or_else(|| {
            s.strip_suffix("grad")
                .and_then(|s| s.parse().ok())
                .map(|t: f64| t * 360.0 / 400.0)
        })
        .or_else(|| {
            s.strip_suffix("rad")
                .and_then(|s| s.parse().ok())
                .map(|t: f64| t.to_degrees())
        })
        .or_else(|| {
            s.strip_suffix("turn")
                .and_then(|s| s.parse().ok())
                .map(|t: f64| t * 360.0)
        })
        .or_else(|| s.parse().ok())
}

#[cfg(feature = "lab")]
// Map t from range [a, b] to range [c, d]
fn remap(t: f64, a: f64, b: f64, c: f64, d: f64) -> f64 {
    (t - a) * ((d - c) / (b - a)) + c
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_percent_or_float() {
        let test_data = [
            ("0%", Some((0.0, true))),
            ("100%", Some((1.0, true))),
            ("50%", Some((0.5, true))),
            ("0", Some((0.0, false))),
            ("1", Some((1.0, false))),
            ("0.5", Some((0.5, false))),
            ("100.0", Some((100.0, false))),
            ("-23.7", Some((-23.7, false))),
            ("%", None),
            ("1x", None),
        ];
        for (s, expected) in test_data {
            assert_eq!(parse_percent_or_float(s), expected);
        }
    }

    #[test]
    fn test_parse_percent_or_255() {
        let test_data = [
            ("0%", Some((0.0, true))),
            ("100%", Some((1.0, true))),
            ("50%", Some((0.5, true))),
            ("-100%", Some((-1.0, true))),
            ("0", Some((0.0, false))),
            ("255", Some((1.0, false))),
            ("127.5", Some((0.5, false))),
            ("%", None),
            ("255x", None),
        ];
        for (s, expected) in test_data {
            assert_eq!(parse_percent_or_255(s), expected);
        }
    }

    #[test]
    fn test_parse_angle() {
        let test_data = [
            ("360", Some(360.0)),
            ("127.356", Some(127.356)),
            ("+120deg", Some(120.0)),
            ("90deg", Some(90.0)),
            ("-127deg", Some(-127.0)),
            ("100grad", Some(90.0)),
            ("1.5707963267948966rad", Some(90.0)),
            ("0.25turn", Some(90.0)),
            ("-0.25turn", Some(-90.0)),
            ("O", None),
            ("Odeg", None),
            ("rad", None),
        ];
        for (s, expected) in test_data {
            assert_eq!(parse_angle(s), expected);
        }
    }

    #[cfg(feature = "named-colors")]
    #[test]
    fn test_named_colors() {
        for (&name, &rgb) in NAMED_COLORS.entries() {
            assert_eq!(parse(name).unwrap().to_rgba8()[0..3], rgb);
        }

        let skip_list = ["aqua", "cyan", "fuchsia", "magenta"];

        for &name in NAMED_COLORS.keys() {
            if skip_list.contains(&name) || name.contains("gray") || name.contains("grey") {
                continue;
            }
            assert_eq!(parse(name).unwrap().name(), Some(name));
        }
    }
}
