use crossterm::style::Color;

/// Build a RGB color
///
/// ```
/// let gold = termimad::rgb(255, 187, 0);
/// ```
pub const fn rgb(r: u8, g: u8, b: u8) -> Color {
    Color::Rgb { r, g, b }
}

/// Build a gray-level color, from 0 (mostly dark) to 23 (light).
pub fn gray(level: u8) -> Color {
    assert!(level < 24, "invalid gray level (must be in 0..24)");
    Color::AnsiValue(0xE8 + level)
}

/// Build an [ANSI color](https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit)
pub const fn ansi(level: u8) -> Color {
    Color::AnsiValue(level)
}

#[cfg(test)]
mod color_tests {

    use crate::color::*;

    /// check the color range is correctly checked and a meaningful
    /// error is raised
    #[test]
    #[should_panic(expected = "invalid gray level (must be in 0..24)")]
    fn check_gray_panic() {
        let _ = gray(24);
    }
}
