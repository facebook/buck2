use super::*;
use std::convert::TryFrom;
use std::str::FromStr;

// To Lab /////////////////////////////////////////////////////////////////////
impl From<LchValue> for LabValue {
    fn from(lch: LchValue) -> LabValue {
        LabValue {
            l: lch.l,
            a: lch.c * lch.h.to_radians().cos(),
            b: lch.c * lch.h.to_radians().sin(),
        }
    }
}

impl From<&LchValue> for LabValue {
    fn from(lch: &LchValue) -> LabValue {
        LabValue::from(*lch)
    }
}

impl From<&LabValue> for LabValue {
    fn from(lab: &LabValue) -> LabValue {
        *lab
    }
}

impl From<XyzValue> for LabValue {
    fn from(xyz: XyzValue) -> LabValue {
        let x = xyz_to_lab_map(xyz.x / 0.9642);
        let y = xyz_to_lab_map(xyz.y);
        let z = xyz_to_lab_map(xyz.z / 0.8251);

        LabValue {
            l: (116.0 * y) - 16.0,
            a: 500.0 * (x - y),
            b: 200.0 * (y - z),
        }
    }
}

impl From<&XyzValue> for LabValue {
    fn from(xyz: &XyzValue) -> LabValue {
        LabValue::from(*xyz)
    }
}

impl TryFrom<&[f32; 3]> for LabValue {
    type Error = ValueError;
    fn try_from(slice: &[f32; 3]) -> ValueResult<LabValue> {
        LabValue {
            l: slice[0],
            a: slice[1],
            b: slice[2]
        }.validate()
    }
}

impl TryFrom<(f32, f32, f32)> for LabValue {
    type Error = ValueError;
    fn try_from(tuple: (f32, f32, f32)) -> ValueResult<LabValue> {
        LabValue {
            l: tuple.0,
            a: tuple.1,
            b: tuple.2,
        }.validate()
    }
}

impl TryFrom<&(f32, f32, f32)> for LabValue {
    type Error = ValueError;
    fn try_from(tuple: &(f32, f32, f32)) -> ValueResult<LabValue> {
        LabValue {
            l: tuple.0,
            a: tuple.1,
            b: tuple.2,
        }.validate()
    }
}

// To Lch /////////////////////////////////////////////////////////////////////
impl From<LabValue> for LchValue {
    fn from(lab: LabValue) -> LchValue {
        LchValue {
            l: lab.l,
            c: ( lab.a.powi(2) + lab.b.powi(2) ).sqrt(),
            h: get_h_prime(lab.a, lab.b),
        }
    }
}

impl From<&LabValue> for LchValue {
    fn from(lab: &LabValue) -> LchValue {
        LchValue::from(*lab)
    }
}

impl From<XyzValue> for LchValue {
    fn from(xyz: XyzValue) -> LchValue {
        LchValue::from(LabValue::from(xyz))
    }
}

impl From<&XyzValue> for LchValue {
    fn from(xyz: &XyzValue) -> LchValue {
        LchValue::from(*xyz)
    }
}

impl TryFrom<&[f32; 3]> for LchValue {
    type Error = ValueError;
    fn try_from(slice: &[f32; 3]) -> ValueResult<LchValue> {
        LchValue {
            l: slice[0],
            c: slice[1],
            h: slice[2]
        }.validate()
    }
}

impl TryFrom<(f32, f32, f32)> for LchValue {
    type Error = ValueError;
    fn try_from(tuple: (f32, f32, f32)) -> ValueResult<LchValue> {
        LchValue {
            l: tuple.0,
            c: tuple.1,
            h: tuple.2,
        }.validate()
    }
}

impl TryFrom<&(f32, f32, f32)> for LchValue {
    type Error = ValueError;
    fn try_from(tuple: &(f32, f32, f32)) -> ValueResult<LchValue> {
        LchValue {
            l: tuple.0,
            c: tuple.1,
            h: tuple.2,
        }.validate()
    }
}

// To Xyz /////////////////////////////////////////////////////////////////////
impl From<LabValue> for XyzValue {
    fn from(lab: LabValue) -> XyzValue {
        let fy = (lab.l + 16.0) / 116.0;
        let fx = (lab.a / 500.0) + fy;
        let fz = fy - (lab.b / 200.0);
        let xr = if fx > CBRT_EPSILON as f32 {
            fx.powi(3)
        } else {
            ((fx * 116.0) - 16.0) / KAPPA
        };
        let yr = if lab.l > EPSILON * KAPPA {
            fy.powi(3)
        } else {
            lab.l / KAPPA
        };
        let zr = if fz > CBRT_EPSILON as f32 {
            fz.powi(3)
        } else {
            ((fz * 116.0) - 16.0) / KAPPA
        };

        XyzValue {
            x: xr * 0.9642,
            y: yr,
            z: zr * 0.8251,
        }
    }
}

impl From<&LabValue> for XyzValue {
    fn from(lab: &LabValue) -> XyzValue {
        XyzValue::from(*lab)
    }
}

impl From<LchValue> for XyzValue {
    fn from(lch: LchValue) -> XyzValue {
        XyzValue::from(LabValue::from(lch))
    }
}

impl From<&LchValue> for XyzValue {
    fn from(lch: &LchValue) -> XyzValue {
        XyzValue::from(*lch)
    }
}

impl TryFrom<&[f32; 3]> for XyzValue {
    type Error = ValueError;
    fn try_from(slice: &[f32; 3]) -> ValueResult<XyzValue> {
        XyzValue {
            x: slice[0],
            y: slice[1],
            z: slice[2]
        }.validate()
    }
}

impl TryFrom<(f32, f32, f32)> for XyzValue {
    type Error = ValueError;
    fn try_from(tuple: (f32, f32, f32)) -> ValueResult<XyzValue> {
        XyzValue {
            x: tuple.0,
            y: tuple.1,
            z: tuple.2,
        }.validate()
    }
}

impl TryFrom<&(f32, f32, f32)> for XyzValue {
    type Error = ValueError;
    fn try_from(tuple: &(f32, f32, f32)) -> ValueResult<XyzValue> {
        XyzValue {
            x: tuple.0,
            y: tuple.1,
            z: tuple.2,
        }.validate()
    }
}

// FromStr ////////////////////////////////////////////////////////////////////
impl FromStr for DEMethod {
    type Err = std::io::Error;
    fn from_str(s: &str) -> Result<DEMethod, Self::Err> {
        match s.to_lowercase().trim() {
            "de2000"  | "de00"  | "2000"  | "00"  => Ok(DEMethod::DE2000),
            "de1976"  | "de76"  | "1976"  | "76"  => Ok(DEMethod::DE1976),
            "de1994"  | "de94"  | "1994"  | "94" |
            "de1994g" | "de94g" | "1994g" | "94g" => Ok(DEMethod::DE1994G),
            "de1994t" | "de94t" | "1994t" | "94t" => Ok(DEMethod::DE1994T),
            "decmc"   | "decmc1"| "cmc1"  | "cmc" => Ok(DEMethod::DECMC(1.0, 1.0)),
            "decmc2"  | "cmc2"                    => Ok(DEMethod::DECMC(2.0, 1.0)),
            _ => Err(io::Error::from(io::ErrorKind::InvalidInput)),
        }
    }
}

impl FromStr for LabValue {
    type Err = ValueError;
    fn from_str(s: &str) -> ValueResult<LabValue> {
        let split = parse_str_to_vecf32(s, 3)?;

        LabValue {
            l: split[0],
            a: split[1],
            b: split[2],
        }.validate()
    }
}

impl FromStr for LchValue {
    type Err = ValueError;
    fn from_str(s: &str) -> ValueResult<LchValue> {
        let split = parse_str_to_vecf32(s, 3)?;

        LchValue {
            l: split[0],
            c: split[1],
            h: split[2],
        }.validate()
    }
}

impl FromStr for XyzValue {
    type Err = ValueError;
    fn from_str(s: &str) -> ValueResult<XyzValue> {
        let split = parse_str_to_vecf32(s, 3)?;

        XyzValue {
            x: split[0],
            y: split[1],
            z: split[2],
        }.validate()
    }

}

// Helper Functions ////////////////////////////////////////////////////////////
const KAPPA: f32 = 24389.0 / 27.0; // CIE Standard: 903.3
const EPSILON: f32 = 216.0 / 24389.0; // CIE Standard: 0.008856
const CBRT_EPSILON: f64 = 0.20689655172413796;

pub fn get_h_prime(a: f32, b: f32) -> f32 {
    let h_prime = b.atan2(a).to_degrees();
    if h_prime < 0.0 {
        h_prime + 360.0
    } else {
        h_prime
    }
}

// Validate and convert strings to `LabValue`.
// Split string by comma (92.5,33.5,-18.8).
fn parse_str_to_vecf32(s: &str, length: usize) -> ValueResult<Vec<f32>> {
    let collection: Vec<&str> = s.split(',').collect();

    // Allow extraneous whitespace ("92.5, 33.5, -18.8")
    let mut v: Vec<&str> = Vec::new();
    for item in collection.iter() {
        if !item.is_empty() {
            v.push(item.trim());
        }
    }
    // Parse the f32's into a Vec
    let split: Vec<f32> = v.iter().filter_map(|s| s.parse().ok()).collect();

    // Check if it's the right number of items
    if v.len() != length || split.len() != length {
        return Err(ValueError::BadFormat);
    }

    Ok(split)
}

#[inline]
fn xyz_to_lab_map(c: f32) -> f32 {
    if c > EPSILON {
        c.powf(1.0/3.0)
    } else {
        (KAPPA * c + 16.0) / 116.0
    }
}
