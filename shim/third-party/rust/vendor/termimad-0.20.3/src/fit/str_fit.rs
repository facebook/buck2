use {
    unicode_width::UnicodeWidthChar,
    std::borrow::Cow,
};

pub static TAB_REPLACEMENT: &str = "  ";

/// Information about the fitting of a string into a given
/// width in cols.
///
/// The implementation here properly takes into account
/// the width of special characters.
///
/// Backspaces are considered as having a width of -1.
///
/// This implementation is based on a replacement of the
/// tab character.
#[derive(Debug, Clone, Copy)]
pub struct StrFit {
    bytes_count: usize,
    cols_count: usize,
    has_tab: bool,
}

impl StrFit {
    pub fn from(s: &str, cols_max: usize) -> Self {
        let mut bytes_count = 0;
        let mut cols_count: i32 = 0;
        let mut has_tab = false;
        for (idx, c) in s.char_indices() {
            let char_width: i32 = match c {
                '\t' => { // tab
                    has_tab = true;
                    TAB_REPLACEMENT.len() as i32
                }
                '\x08' => { // backspace
                    -1
                }
                _ => UnicodeWidthChar::width(c).map(|w| w as i32).unwrap_or(0),
            };
            let next_str_width = cols_count + char_width;
            if next_str_width > 0 && next_str_width as usize > cols_max {
                break;
            }
            cols_count = next_str_width;
            bytes_count = idx + c.len_utf8();
        }
        Self {
            bytes_count,
            cols_count: cols_count.max(0) as usize,
            has_tab,
        }
    }

    /// return the counts in bytes and columns of the longest substring
    /// fitting the given number of columns
    pub fn count_fitting(s: &str, cols_max: usize) -> (usize, usize) {
        let fit = StrFit::from(s, cols_max);
        (fit.bytes_count, fit.cols_count)
    }

    /// return both the longest fitting string and the number of cols
    /// it takes on screen.
    ///
    /// We don't build a string around the whole str, which could be costly
    /// if it's very big
    pub fn make_string(s: &str, cols_max: usize) -> (String, usize) {
        let fit = StrFit::from(s, cols_max);
        if fit.has_tab {
            let string = (&s[0..fit.bytes_count]).replace('\t', TAB_REPLACEMENT);
            (string, fit.cols_count)
        } else {
            (s[0..fit.bytes_count].to_string(), fit.cols_count)
        }
    }
    /// return both the longest fitting string and the number of cols
    /// it takes on screen.
    ///
    /// We don't build a string around the whole str, which could be costly
    /// if it's very big
    /// In case there's no tab in the input string, we can return a pointer over
    /// part of the original str)
    pub fn make_cow(s: &str, cols_max: usize) -> (Cow<str>, usize) {
        let fit = StrFit::from(s, cols_max);
        if fit.has_tab {
            // we can't just borrow, as we insert chars
            let string = (&s[0..fit.bytes_count]).replace('\t', TAB_REPLACEMENT);
            (Cow::Owned(string), fit.cols_count)
        } else {
            (Cow::Borrowed(&s[0..fit.bytes_count]), fit.cols_count)
        }
    }
}

#[cfg(test)]
mod fitting_count_tests {
    use super::*;

    #[test]
    fn test_count_fitting() {
        assert_eq!(StrFit::count_fitting("test", 3), (3, 3));
        assert_eq!(StrFit::count_fitting("test", 5), (4, 4));
        let c12 = "Comunicações"; // normalized string (12 characters, 14 bytes)
        assert_eq!(c12.len(), 14);
        assert_eq!(c12.chars().count(), 12);
        assert_eq!(StrFit::count_fitting(c12, 12), (14, 12));
        assert_eq!(StrFit::count_fitting(c12, 10), (12, 10));
        assert_eq!(StrFit::count_fitting(c12, 11), (13, 11));
        let c14 = "Comunicações"; // unnormalized string (14 characters, 16 bytes)
        assert_eq!(c14.len(), 16);
        assert_eq!(c14.chars().count(), 14);
        assert_eq!(StrFit::count_fitting(c14, 12), (16, 12));
        let ja = "概要"; // each char takes 3 bytes and 2 columns
        assert_eq!(ja.len(), 6);
        assert_eq!(ja.chars().count(), 2);
        assert_eq!(StrFit::count_fitting(ja, 1), (0, 0));
        assert_eq!(StrFit::count_fitting(ja, 2), (3, 2));
        assert_eq!(StrFit::count_fitting(ja, 3), (3, 2));
        assert_eq!(StrFit::count_fitting(ja, 4), (6, 4));
        assert_eq!(StrFit::count_fitting(ja, 5), (6, 4));
    }
}

