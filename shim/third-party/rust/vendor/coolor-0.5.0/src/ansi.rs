use crate::*;

/// 8-bit Ansi Color Code
///
/// See <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit>
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AnsiColor {
    pub code: u8,
}

impl AnsiColor {
    pub const fn new(code: u8) -> Self {
        Self { code }
    }
    pub const fn to_hsl(self) -> Hsl {
        ANSI_TO_HSL[self.code as usize]
    }
    pub const fn to_rgb(self) -> Rgb {
        ANSI_TO_RGB[self.code as usize]
    }
    pub fn with_luminosity_change(self, delta_luminosity: f32) -> Self {
        let mut hsl = self.to_hsl();
        hsl.l = (hsl.l + delta_luminosity).min(1.0).max(0.0);
        hsl.to_ansi()
    }
    pub fn with_luminosity(self, l: f32) -> Self {
        let mut hsl = self.to_hsl();
        hsl.l = l;
        hsl.to_ansi()
    }
    pub fn with_saturation_change(self, delta_saturation: f32) -> Self {
        let mut hsl = self.to_hsl();
        hsl.s = (hsl.s + delta_saturation).min(1.0).max(0.0);
        hsl.to_ansi()
    }
    pub fn with_saturation(self, s: f32) -> Self {
        let mut hsl = self.to_hsl();
        hsl.s = s;
        hsl.to_ansi()
    }
}

pub const ANSI_TO_RGB: &[Rgb] = &[
    Rgb {
        r: 0x00,
        g: 0x00,
        b: 0x00,
    },
    Rgb {
        r: 0x80,
        g: 0x00,
        b: 0x00,
    },
    Rgb {
        r: 0x00,
        g: 0x80,
        b: 0x00,
    },
    Rgb {
        r: 0x80,
        g: 0x80,
        b: 0x00,
    },
    Rgb {
        r: 0x00,
        g: 0x00,
        b: 0x80,
    },
    Rgb {
        r: 0x80,
        g: 0x00,
        b: 0x80,
    },
    Rgb {
        r: 0x00,
        g: 0x80,
        b: 0x80,
    },
    Rgb {
        r: 0xC0,
        g: 0xC0,
        b: 0xC0,
    },
    Rgb {
        r: 0x80,
        g: 0x80,
        b: 0x80,
    },
    Rgb {
        r: 0xFF,
        g: 0x00,
        b: 0x00,
    },
    Rgb {
        r: 0x00,
        g: 0xFF,
        b: 0x00,
    },
    Rgb {
        r: 0xFF,
        g: 0xFF,
        b: 0x00,
    },
    Rgb {
        r: 0x00,
        g: 0x00,
        b: 0xFF,
    },
    Rgb {
        r: 0xFF,
        g: 0x00,
        b: 0xFF,
    },
    Rgb {
        r: 0x00,
        g: 0xFF,
        b: 0xFF,
    },
    Rgb {
        r: 0xFF,
        g: 0xFF,
        b: 0xFF,
    },
    Rgb {
        r: 0x00,
        g: 0x00,
        b: 0x00,
    },
    Rgb {
        r: 0x00,
        g: 0x00,
        b: 0x5F,
    },
    Rgb {
        r: 0x00,
        g: 0x00,
        b: 0x87,
    },
    Rgb {
        r: 0x00,
        g: 0x00,
        b: 0xAF,
    },
    Rgb {
        r: 0x00,
        g: 0x00,
        b: 0xD7,
    },
    Rgb {
        r: 0x00,
        g: 0x00,
        b: 0xFF,
    },
    Rgb {
        r: 0x00,
        g: 0x5F,
        b: 0x00,
    },
    Rgb {
        r: 0x00,
        g: 0x5F,
        b: 0x5F,
    },
    Rgb {
        r: 0x00,
        g: 0x5F,
        b: 0x87,
    },
    Rgb {
        r: 0x00,
        g: 0x5F,
        b: 0xAF,
    },
    Rgb {
        r: 0x00,
        g: 0x5F,
        b: 0xD7,
    },
    Rgb {
        r: 0x00,
        g: 0x5F,
        b: 0xFF,
    },
    Rgb {
        r: 0x00,
        g: 0x87,
        b: 0x00,
    },
    Rgb {
        r: 0x00,
        g: 0x87,
        b: 0x5F,
    },
    Rgb {
        r: 0x00,
        g: 0x87,
        b: 0x87,
    },
    Rgb {
        r: 0x00,
        g: 0x87,
        b: 0xAF,
    },
    Rgb {
        r: 0x00,
        g: 0x87,
        b: 0xD7,
    },
    Rgb {
        r: 0x00,
        g: 0x87,
        b: 0xFF,
    },
    Rgb {
        r: 0x00,
        g: 0xAF,
        b: 0x00,
    },
    Rgb {
        r: 0x00,
        g: 0xAF,
        b: 0x5F,
    },
    Rgb {
        r: 0x00,
        g: 0xAF,
        b: 0x87,
    },
    Rgb {
        r: 0x00,
        g: 0xAF,
        b: 0xAF,
    },
    Rgb {
        r: 0x00,
        g: 0xAF,
        b: 0xD7,
    },
    Rgb {
        r: 0x00,
        g: 0xAF,
        b: 0xFF,
    },
    Rgb {
        r: 0x00,
        g: 0xD7,
        b: 0x00,
    },
    Rgb {
        r: 0x00,
        g: 0xD7,
        b: 0x5F,
    },
    Rgb {
        r: 0x00,
        g: 0xD7,
        b: 0x87,
    },
    Rgb {
        r: 0x00,
        g: 0xD7,
        b: 0xAF,
    },
    Rgb {
        r: 0x00,
        g: 0xD7,
        b: 0xD7,
    },
    Rgb {
        r: 0x00,
        g: 0xD7,
        b: 0xFF,
    },
    Rgb {
        r: 0x00,
        g: 0xFF,
        b: 0x00,
    },
    Rgb {
        r: 0x00,
        g: 0xFF,
        b: 0x5F,
    },
    Rgb {
        r: 0x00,
        g: 0xFF,
        b: 0x87,
    },
    Rgb {
        r: 0x00,
        g: 0xFF,
        b: 0xAF,
    },
    Rgb {
        r: 0x00,
        g: 0xFF,
        b: 0xD7,
    },
    Rgb {
        r: 0x00,
        g: 0xFF,
        b: 0xFF,
    },
    Rgb {
        r: 0x5F,
        g: 0x00,
        b: 0x00,
    },
    Rgb {
        r: 0x5F,
        g: 0x00,
        b: 0x5F,
    },
    Rgb {
        r: 0x5F,
        g: 0x00,
        b: 0x87,
    },
    Rgb {
        r: 0x5F,
        g: 0x00,
        b: 0xAF,
    },
    Rgb {
        r: 0x5F,
        g: 0x00,
        b: 0xD7,
    },
    Rgb {
        r: 0x5F,
        g: 0x00,
        b: 0xFF,
    },
    Rgb {
        r: 0x5F,
        g: 0x5F,
        b: 0x00,
    },
    Rgb {
        r: 0x5F,
        g: 0x5F,
        b: 0x5F,
    },
    Rgb {
        r: 0x5F,
        g: 0x5F,
        b: 0x87,
    },
    Rgb {
        r: 0x5F,
        g: 0x5F,
        b: 0xAF,
    },
    Rgb {
        r: 0x5F,
        g: 0x5F,
        b: 0xD7,
    },
    Rgb {
        r: 0x5F,
        g: 0x5F,
        b: 0xFF,
    },
    Rgb {
        r: 0x5F,
        g: 0x87,
        b: 0x00,
    },
    Rgb {
        r: 0x5F,
        g: 0x87,
        b: 0x5F,
    },
    Rgb {
        r: 0x5F,
        g: 0x87,
        b: 0x87,
    },
    Rgb {
        r: 0x5F,
        g: 0x87,
        b: 0xAF,
    },
    Rgb {
        r: 0x5F,
        g: 0x87,
        b: 0xD7,
    },
    Rgb {
        r: 0x5F,
        g: 0x87,
        b: 0xFF,
    },
    Rgb {
        r: 0x5F,
        g: 0xAF,
        b: 0x00,
    },
    Rgb {
        r: 0x5F,
        g: 0xAF,
        b: 0x5F,
    },
    Rgb {
        r: 0x5F,
        g: 0xAF,
        b: 0x87,
    },
    Rgb {
        r: 0x5F,
        g: 0xAF,
        b: 0xAF,
    },
    Rgb {
        r: 0x5F,
        g: 0xAF,
        b: 0xD7,
    },
    Rgb {
        r: 0x5F,
        g: 0xAF,
        b: 0xFF,
    },
    Rgb {
        r: 0x5F,
        g: 0xD7,
        b: 0x00,
    },
    Rgb {
        r: 0x5F,
        g: 0xD7,
        b: 0x5F,
    },
    Rgb {
        r: 0x5F,
        g: 0xD7,
        b: 0x87,
    },
    Rgb {
        r: 0x5F,
        g: 0xD7,
        b: 0xAF,
    },
    Rgb {
        r: 0x5F,
        g: 0xD7,
        b: 0xD7,
    },
    Rgb {
        r: 0x5F,
        g: 0xD7,
        b: 0xFF,
    },
    Rgb {
        r: 0x5F,
        g: 0xFF,
        b: 0x00,
    },
    Rgb {
        r: 0x5F,
        g: 0xFF,
        b: 0x5F,
    },
    Rgb {
        r: 0x5F,
        g: 0xFF,
        b: 0x87,
    },
    Rgb {
        r: 0x5F,
        g: 0xFF,
        b: 0xAF,
    },
    Rgb {
        r: 0x5F,
        g: 0xFF,
        b: 0xD7,
    },
    Rgb {
        r: 0x5F,
        g: 0xFF,
        b: 0xFF,
    },
    Rgb {
        r: 0x87,
        g: 0x00,
        b: 0x00,
    },
    Rgb {
        r: 0x87,
        g: 0x00,
        b: 0x5F,
    },
    Rgb {
        r: 0x87,
        g: 0x00,
        b: 0x87,
    },
    Rgb {
        r: 0x87,
        g: 0x00,
        b: 0xAF,
    },
    Rgb {
        r: 0x87,
        g: 0x00,
        b: 0xD7,
    },
    Rgb {
        r: 0x87,
        g: 0x00,
        b: 0xFF,
    },
    Rgb {
        r: 0x87,
        g: 0x5F,
        b: 0x00,
    },
    Rgb {
        r: 0x87,
        g: 0x5F,
        b: 0x5F,
    },
    Rgb {
        r: 0x87,
        g: 0x5F,
        b: 0x87,
    },
    Rgb {
        r: 0x87,
        g: 0x5F,
        b: 0xAF,
    },
    Rgb {
        r: 0x87,
        g: 0x5F,
        b: 0xD7,
    },
    Rgb {
        r: 0x87,
        g: 0x5F,
        b: 0xFF,
    },
    Rgb {
        r: 0x87,
        g: 0x87,
        b: 0x00,
    },
    Rgb {
        r: 0x87,
        g: 0x87,
        b: 0x5F,
    },
    Rgb {
        r: 0x87,
        g: 0x87,
        b: 0x87,
    },
    Rgb {
        r: 0x87,
        g: 0x87,
        b: 0xAF,
    },
    Rgb {
        r: 0x87,
        g: 0x87,
        b: 0xD7,
    },
    Rgb {
        r: 0x87,
        g: 0x87,
        b: 0xFF,
    },
    Rgb {
        r: 0x87,
        g: 0xAF,
        b: 0x00,
    },
    Rgb {
        r: 0x87,
        g: 0xAF,
        b: 0x5F,
    },
    Rgb {
        r: 0x87,
        g: 0xAF,
        b: 0x87,
    },
    Rgb {
        r: 0x87,
        g: 0xAF,
        b: 0xAF,
    },
    Rgb {
        r: 0x87,
        g: 0xAF,
        b: 0xD7,
    },
    Rgb {
        r: 0x87,
        g: 0xAF,
        b: 0xFF,
    },
    Rgb {
        r: 0x87,
        g: 0xD7,
        b: 0x00,
    },
    Rgb {
        r: 0x87,
        g: 0xD7,
        b: 0x5F,
    },
    Rgb {
        r: 0x87,
        g: 0xD7,
        b: 0x87,
    },
    Rgb {
        r: 0x87,
        g: 0xD7,
        b: 0xAF,
    },
    Rgb {
        r: 0x87,
        g: 0xD7,
        b: 0xD7,
    },
    Rgb {
        r: 0x87,
        g: 0xD7,
        b: 0xFF,
    },
    Rgb {
        r: 0x87,
        g: 0xFF,
        b: 0x00,
    },
    Rgb {
        r: 0x87,
        g: 0xFF,
        b: 0x5F,
    },
    Rgb {
        r: 0x87,
        g: 0xFF,
        b: 0x87,
    },
    Rgb {
        r: 0x87,
        g: 0xFF,
        b: 0xAF,
    },
    Rgb {
        r: 0x87,
        g: 0xFF,
        b: 0xD7,
    },
    Rgb {
        r: 0x87,
        g: 0xFF,
        b: 0xFF,
    },
    Rgb {
        r: 0xAF,
        g: 0x00,
        b: 0x00,
    },
    Rgb {
        r: 0xAF,
        g: 0x00,
        b: 0x5F,
    },
    Rgb {
        r: 0xAF,
        g: 0x00,
        b: 0x87,
    },
    Rgb {
        r: 0xAF,
        g: 0x00,
        b: 0xAF,
    },
    Rgb {
        r: 0xAF,
        g: 0x00,
        b: 0xD7,
    },
    Rgb {
        r: 0xAF,
        g: 0x00,
        b: 0xFF,
    },
    Rgb {
        r: 0xAF,
        g: 0x5F,
        b: 0x00,
    },
    Rgb {
        r: 0xAF,
        g: 0x5F,
        b: 0x5F,
    },
    Rgb {
        r: 0xAF,
        g: 0x5F,
        b: 0x87,
    },
    Rgb {
        r: 0xAF,
        g: 0x5F,
        b: 0xAF,
    },
    Rgb {
        r: 0xAF,
        g: 0x5F,
        b: 0xD7,
    },
    Rgb {
        r: 0xAF,
        g: 0x5F,
        b: 0xFF,
    },
    Rgb {
        r: 0xAF,
        g: 0x87,
        b: 0x00,
    },
    Rgb {
        r: 0xAF,
        g: 0x87,
        b: 0x5F,
    },
    Rgb {
        r: 0xAF,
        g: 0x87,
        b: 0x87,
    },
    Rgb {
        r: 0xAF,
        g: 0x87,
        b: 0xAF,
    },
    Rgb {
        r: 0xAF,
        g: 0x87,
        b: 0xD7,
    },
    Rgb {
        r: 0xAF,
        g: 0x87,
        b: 0xFF,
    },
    Rgb {
        r: 0xAF,
        g: 0xAF,
        b: 0x00,
    },
    Rgb {
        r: 0xAF,
        g: 0xAF,
        b: 0x5F,
    },
    Rgb {
        r: 0xAF,
        g: 0xAF,
        b: 0x87,
    },
    Rgb {
        r: 0xAF,
        g: 0xAF,
        b: 0xAF,
    },
    Rgb {
        r: 0xAF,
        g: 0xAF,
        b: 0xD7,
    },
    Rgb {
        r: 0xAF,
        g: 0xAF,
        b: 0xFF,
    },
    Rgb {
        r: 0xAF,
        g: 0xD7,
        b: 0x00,
    },
    Rgb {
        r: 0xAF,
        g: 0xD7,
        b: 0x5F,
    },
    Rgb {
        r: 0xAF,
        g: 0xD7,
        b: 0x87,
    },
    Rgb {
        r: 0xAF,
        g: 0xD7,
        b: 0xAF,
    },
    Rgb {
        r: 0xAF,
        g: 0xD7,
        b: 0xD7,
    },
    Rgb {
        r: 0xAF,
        g: 0xD7,
        b: 0xFF,
    },
    Rgb {
        r: 0xAF,
        g: 0xFF,
        b: 0x00,
    },
    Rgb {
        r: 0xAF,
        g: 0xFF,
        b: 0x5F,
    },
    Rgb {
        r: 0xAF,
        g: 0xFF,
        b: 0x87,
    },
    Rgb {
        r: 0xAF,
        g: 0xFF,
        b: 0xAF,
    },
    Rgb {
        r: 0xAF,
        g: 0xFF,
        b: 0xD7,
    },
    Rgb {
        r: 0xAF,
        g: 0xFF,
        b: 0xFF,
    },
    Rgb {
        r: 0xD7,
        g: 0x00,
        b: 0x00,
    },
    Rgb {
        r: 0xD7,
        g: 0x00,
        b: 0x5F,
    },
    Rgb {
        r: 0xD7,
        g: 0x00,
        b: 0x87,
    },
    Rgb {
        r: 0xD7,
        g: 0x00,
        b: 0xAF,
    },
    Rgb {
        r: 0xD7,
        g: 0x00,
        b: 0xD7,
    },
    Rgb {
        r: 0xD7,
        g: 0x00,
        b: 0xFF,
    },
    Rgb {
        r: 0xD7,
        g: 0x5F,
        b: 0x00,
    },
    Rgb {
        r: 0xD7,
        g: 0x5F,
        b: 0x5F,
    },
    Rgb {
        r: 0xD7,
        g: 0x5F,
        b: 0x87,
    },
    Rgb {
        r: 0xD7,
        g: 0x5F,
        b: 0xAF,
    },
    Rgb {
        r: 0xD7,
        g: 0x5F,
        b: 0xD7,
    },
    Rgb {
        r: 0xD7,
        g: 0x5F,
        b: 0xFF,
    },
    Rgb {
        r: 0xD7,
        g: 0x87,
        b: 0x00,
    },
    Rgb {
        r: 0xD7,
        g: 0x87,
        b: 0x5F,
    },
    Rgb {
        r: 0xD7,
        g: 0x87,
        b: 0x87,
    },
    Rgb {
        r: 0xD7,
        g: 0x87,
        b: 0xAF,
    },
    Rgb {
        r: 0xD7,
        g: 0x87,
        b: 0xD7,
    },
    Rgb {
        r: 0xD7,
        g: 0x87,
        b: 0xFF,
    },
    Rgb {
        r: 0xD7,
        g: 0xAF,
        b: 0x00,
    },
    Rgb {
        r: 0xD7,
        g: 0xAF,
        b: 0x5F,
    },
    Rgb {
        r: 0xD7,
        g: 0xAF,
        b: 0x87,
    },
    Rgb {
        r: 0xD7,
        g: 0xAF,
        b: 0xAF,
    },
    Rgb {
        r: 0xD7,
        g: 0xAF,
        b: 0xD7,
    },
    Rgb {
        r: 0xD7,
        g: 0xAF,
        b: 0xFF,
    },
    Rgb {
        r: 0xD7,
        g: 0xD7,
        b: 0x00,
    },
    Rgb {
        r: 0xD7,
        g: 0xD7,
        b: 0x5F,
    },
    Rgb {
        r: 0xD7,
        g: 0xD7,
        b: 0x87,
    },
    Rgb {
        r: 0xD7,
        g: 0xD7,
        b: 0xAF,
    },
    Rgb {
        r: 0xD7,
        g: 0xD7,
        b: 0xD7,
    },
    Rgb {
        r: 0xD7,
        g: 0xD7,
        b: 0xFF,
    },
    Rgb {
        r: 0xD7,
        g: 0xFF,
        b: 0x00,
    },
    Rgb {
        r: 0xD7,
        g: 0xFF,
        b: 0x5F,
    },
    Rgb {
        r: 0xD7,
        g: 0xFF,
        b: 0x87,
    },
    Rgb {
        r: 0xD7,
        g: 0xFF,
        b: 0xAF,
    },
    Rgb {
        r: 0xD7,
        g: 0xFF,
        b: 0xD7,
    },
    Rgb {
        r: 0xD7,
        g: 0xFF,
        b: 0xFF,
    },
    Rgb {
        r: 0xFF,
        g: 0x00,
        b: 0x00,
    },
    Rgb {
        r: 0xFF,
        g: 0x00,
        b: 0x5F,
    },
    Rgb {
        r: 0xFF,
        g: 0x00,
        b: 0x87,
    },
    Rgb {
        r: 0xFF,
        g: 0x00,
        b: 0xAF,
    },
    Rgb {
        r: 0xFF,
        g: 0x00,
        b: 0xD7,
    },
    Rgb {
        r: 0xFF,
        g: 0x00,
        b: 0xFF,
    },
    Rgb {
        r: 0xFF,
        g: 0x5F,
        b: 0x00,
    },
    Rgb {
        r: 0xFF,
        g: 0x5F,
        b: 0x5F,
    },
    Rgb {
        r: 0xFF,
        g: 0x5F,
        b: 0x87,
    },
    Rgb {
        r: 0xFF,
        g: 0x5F,
        b: 0xAF,
    },
    Rgb {
        r: 0xFF,
        g: 0x5F,
        b: 0xD7,
    },
    Rgb {
        r: 0xFF,
        g: 0x5F,
        b: 0xFF,
    },
    Rgb {
        r: 0xFF,
        g: 0x87,
        b: 0x00,
    },
    Rgb {
        r: 0xFF,
        g: 0x87,
        b: 0x5F,
    },
    Rgb {
        r: 0xFF,
        g: 0x87,
        b: 0x87,
    },
    Rgb {
        r: 0xFF,
        g: 0x87,
        b: 0xAF,
    },
    Rgb {
        r: 0xFF,
        g: 0x87,
        b: 0xD7,
    },
    Rgb {
        r: 0xFF,
        g: 0x87,
        b: 0xFF,
    },
    Rgb {
        r: 0xFF,
        g: 0xAF,
        b: 0x00,
    },
    Rgb {
        r: 0xFF,
        g: 0xAF,
        b: 0x5F,
    },
    Rgb {
        r: 0xFF,
        g: 0xAF,
        b: 0x87,
    },
    Rgb {
        r: 0xFF,
        g: 0xAF,
        b: 0xAF,
    },
    Rgb {
        r: 0xFF,
        g: 0xAF,
        b: 0xD7,
    },
    Rgb {
        r: 0xFF,
        g: 0xAF,
        b: 0xFF,
    },
    Rgb {
        r: 0xFF,
        g: 0xD7,
        b: 0x00,
    },
    Rgb {
        r: 0xFF,
        g: 0xD7,
        b: 0x5F,
    },
    Rgb {
        r: 0xFF,
        g: 0xD7,
        b: 0x87,
    },
    Rgb {
        r: 0xFF,
        g: 0xD7,
        b: 0xAF,
    },
    Rgb {
        r: 0xFF,
        g: 0xD7,
        b: 0xD7,
    },
    Rgb {
        r: 0xFF,
        g: 0xD7,
        b: 0xFF,
    },
    Rgb {
        r: 0xFF,
        g: 0xFF,
        b: 0x00,
    },
    Rgb {
        r: 0xFF,
        g: 0xFF,
        b: 0x5F,
    },
    Rgb {
        r: 0xFF,
        g: 0xFF,
        b: 0x87,
    },
    Rgb {
        r: 0xFF,
        g: 0xFF,
        b: 0xAF,
    },
    Rgb {
        r: 0xFF,
        g: 0xFF,
        b: 0xD7,
    },
    Rgb {
        r: 0xFF,
        g: 0xFF,
        b: 0xFF,
    },
    Rgb {
        r: 0x08,
        g: 0x08,
        b: 0x08,
    },
    Rgb {
        r: 0x12,
        g: 0x12,
        b: 0x12,
    },
    Rgb {
        r: 0x1C,
        g: 0x1C,
        b: 0x1C,
    },
    Rgb {
        r: 0x26,
        g: 0x26,
        b: 0x26,
    },
    Rgb {
        r: 0x30,
        g: 0x30,
        b: 0x30,
    },
    Rgb {
        r: 0x3A,
        g: 0x3A,
        b: 0x3A,
    },
    Rgb {
        r: 0x44,
        g: 0x44,
        b: 0x44,
    },
    Rgb {
        r: 0x4E,
        g: 0x4E,
        b: 0x4E,
    },
    Rgb {
        r: 0x58,
        g: 0x58,
        b: 0x58,
    },
    Rgb {
        r: 0x60,
        g: 0x60,
        b: 0x60,
    },
    Rgb {
        r: 0x66,
        g: 0x66,
        b: 0x66,
    },
    Rgb {
        r: 0x76,
        g: 0x76,
        b: 0x76,
    },
    Rgb {
        r: 0x80,
        g: 0x80,
        b: 0x80,
    },
    Rgb {
        r: 0x8A,
        g: 0x8A,
        b: 0x8A,
    },
    Rgb {
        r: 0x94,
        g: 0x94,
        b: 0x94,
    },
    Rgb {
        r: 0x9E,
        g: 0x9E,
        b: 0x9E,
    },
    Rgb {
        r: 0xA8,
        g: 0xA8,
        b: 0xA8,
    },
    Rgb {
        r: 0xB2,
        g: 0xB2,
        b: 0xB2,
    },
    Rgb {
        r: 0xBC,
        g: 0xBC,
        b: 0xBC,
    },
    Rgb {
        r: 0xC6,
        g: 0xC6,
        b: 0xC6,
    },
    Rgb {
        r: 0xD0,
        g: 0xD0,
        b: 0xD0,
    },
    Rgb {
        r: 0xDA,
        g: 0xDA,
        b: 0xDA,
    },
    Rgb {
        r: 0xE4,
        g: 0xE4,
        b: 0xE4,
    },
    Rgb {
        r: 0xEE,
        g: 0xEE,
        b: 0xEE,
    },
];

pub const ANSI_TO_HSL: &[Hsl] = &[
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.0,
    },
    Hsl {
        h: 0.0,
        s: 1.0,
        l: 0.25,
    },
    Hsl {
        h: 120.0,
        s: 1.0,
        l: 0.25,
    },
    Hsl {
        h: 60.0,
        s: 1.0,
        l: 0.25,
    },
    Hsl {
        h: 240.0,
        s: 1.0,
        l: 0.25,
    },
    Hsl {
        h: 300.0,
        s: 1.0,
        l: 0.25,
    },
    Hsl {
        h: 180.0,
        s: 1.0,
        l: 0.25,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.75,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.5,
    },
    Hsl {
        h: 0.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 120.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 60.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 240.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 300.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 180.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.99609375,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.0,
    },
    Hsl {
        h: 240.0,
        s: 1.0,
        l: 0.18554688,
    },
    Hsl {
        h: 240.0,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 240.0,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 240.0,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 240.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 120.0,
        s: 1.0,
        l: 0.18554688,
    },
    Hsl {
        h: 180.0,
        s: 1.0,
        l: 0.18554688,
    },
    Hsl {
        h: 197.77777,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 207.42859,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 213.4884,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 217.6471,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 120.0,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 162.22223,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 180.0,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 193.7143,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 202.32556,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 208.23529,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 120.0,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 152.57141,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 166.2857,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 180.0,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 191.16278,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 198.82355,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 120.0,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 146.51163,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 157.67444,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 168.83722,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 180.0,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 189.41174,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 120.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 142.35294,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 151.76471,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 161.17645,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 170.58826,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 180.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 0.0,
        s: 1.0,
        l: 0.18554688,
    },
    Hsl {
        h: 300.0,
        s: 1.0,
        l: 0.18554688,
    },
    Hsl {
        h: 282.22223,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 272.5714,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 266.5116,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 262.3529,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 60.0,
        s: 1.0,
        l: 0.18554688,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.37109375,
    },
    Hsl {
        h: 240.0,
        s: 0.17391305,
        l: 0.44921875,
    },
    Hsl {
        h: 240.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 240.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 240.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 77.77777,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 120.0,
        s: 0.17391305,
        l: 0.44921875,
    },
    Hsl {
        h: 180.0,
        s: 0.17391305,
        l: 0.44921875,
    },
    Hsl {
        h: 210.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 220.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 225.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 87.42859,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 120.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 150.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 180.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 200.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 210.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 93.48837,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 120.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 140.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 160.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 180.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 195.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 97.647064,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 120.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 135.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 150.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 165.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 180.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 0.0,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 317.77777,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 300.0,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 286.2857,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 277.67444,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 271.7647,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 42.22223,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 0.0,
        s: 0.17391305,
        l: 0.44921875,
    },
    Hsl {
        h: 300.0,
        s: 0.17391305,
        l: 0.44921875,
    },
    Hsl {
        h: 270.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 260.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 255.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 60.0,
        s: 1.0,
        l: 0.26367188,
    },
    Hsl {
        h: 60.0,
        s: 0.17391305,
        l: 0.44921875,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.52734375,
    },
    Hsl {
        h: 240.0,
        s: 0.1980198,
        l: 0.60546875,
    },
    Hsl {
        h: 240.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 240.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 73.714294,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 90.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 120.0,
        s: 0.1980198,
        l: 0.60546875,
    },
    Hsl {
        h: 180.0,
        s: 0.1980198,
        l: 0.60546875,
    },
    Hsl {
        h: 210.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 220.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 82.32556,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 100.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 120.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 150.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 180.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 200.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 88.23529,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 105.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 120.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 140.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 160.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 180.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 0.0,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 327.42856,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 313.7143,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 300.0,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 288.83722,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 281.1765,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 32.57144,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 0.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 330.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 300.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 280.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 270.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 46.285706,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 30.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 0.0,
        s: 0.1980198,
        l: 0.60546875,
    },
    Hsl {
        h: 300.0,
        s: 0.1980198,
        l: 0.60546875,
    },
    Hsl {
        h: 270.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 260.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 60.0,
        s: 1.0,
        l: 0.34179688,
    },
    Hsl {
        h: 60.0,
        s: 0.3305785,
        l: 0.52734375,
    },
    Hsl {
        h: 60.0,
        s: 0.1980198,
        l: 0.60546875,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.68359375,
    },
    Hsl {
        h: 240.0,
        s: 0.32786885,
        l: 0.76171875,
    },
    Hsl {
        h: 240.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 71.16278,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 80.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 90.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 120.0,
        s: 0.32786885,
        l: 0.76171875,
    },
    Hsl {
        h: 180.0,
        s: 0.32786885,
        l: 0.76171875,
    },
    Hsl {
        h: 210.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 78.82355,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 90.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 100.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 120.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 150.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 180.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 0.0,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 333.48837,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 322.3256,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 311.16278,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 300.0,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 290.58826,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 26.511627,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 0.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 340.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 320.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 300.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 285.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 37.674408,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 20.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 0.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 330.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 300.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 280.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 48.83722,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 40.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 30.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 0.0,
        s: 0.32786885,
        l: 0.76171875,
    },
    Hsl {
        h: 300.0,
        s: 0.32786885,
        l: 0.76171875,
    },
    Hsl {
        h: 270.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 60.0,
        s: 1.0,
        l: 0.41992188,
    },
    Hsl {
        h: 60.0,
        s: 0.5940594,
        l: 0.60546875,
    },
    Hsl {
        h: 60.0,
        s: 0.49382716,
        l: 0.68359375,
    },
    Hsl {
        h: 60.0,
        s: 0.32786885,
        l: 0.76171875,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.83984375,
    },
    Hsl {
        h: 240.0,
        s: 0.95238096,
        l: 0.91796875,
    },
    Hsl {
        h: 69.41177,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 75.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 80.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 90.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 120.0,
        s: 0.95238096,
        l: 0.91796875,
    },
    Hsl {
        h: 180.0,
        s: 0.95238096,
        l: 0.91796875,
    },
    Hsl {
        h: 0.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 337.64706,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 328.2353,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 318.82352,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 309.41177,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 300.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 22.352936,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 0.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 345.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 330.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 315.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 300.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 31.76471,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 15.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 0.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 340.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 320.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 300.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 41.176483,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 30.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 20.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 0.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 330.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 300.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 50.588226,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 45.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 40.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 30.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 0.0,
        s: 0.95238096,
        l: 0.91796875,
    },
    Hsl {
        h: 300.0,
        s: 0.95238096,
        l: 0.91796875,
    },
    Hsl {
        h: 60.0,
        s: 1.0,
        l: 0.49804688,
    },
    Hsl {
        h: 60.0,
        s: 0.9876543,
        l: 0.68359375,
    },
    Hsl {
        h: 60.0,
        s: 0.9836066,
        l: 0.76171875,
    },
    Hsl {
        h: 60.0,
        s: 0.9756098,
        l: 0.83984375,
    },
    Hsl {
        h: 60.0,
        s: 0.95238096,
        l: 0.91796875,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.99609375,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.03125,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.0703125,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.109375,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.1484375,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.1875,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.2265625,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.265625,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.3046875,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.34375,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.375,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.3984375,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.4609375,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.5,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.5390625,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.578125,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.6171875,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.65625,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.6953125,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.734375,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.7734375,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.8125,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.8515625,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.890625,
    },
    Hsl {
        h: 0.0,
        s: 0.0,
        l: 0.9296875,
    },
];

#[test]
fn check_array_lengths() {
    // just check I properly generated the tables ^^
    assert_eq!(ANSI_TO_RGB.len(), 256);
    assert_eq!(ANSI_TO_HSL.len(), 256);
}
