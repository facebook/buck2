use std::fmt::{self, Display, Write};

use crate::{Colour, Style};

impl Style {
    /// Write any bytes that go *before* a piece of text to the given writer.
    pub fn write_prefix(&self, f: &mut fmt::Formatter) -> Result<bool, fmt::Error> {
        let mut written_anything = false;
        macro_rules! write_anything {
            () => {
                if written_anything {
                    f.write_char(';')?;
                } else {
                    // Write the codes’ prefix, then write numbers, separated by
                    // semicolons, for each text style we want to apply.
                    f.write_str("\x1B[")?;
                    written_anything = true;
                }
            };
        }
        macro_rules! write_char {
            ($cond:ident, $c:expr) => {
                if self.$cond {
                    write_anything!();
                    f.write_char($c)?;
                }
            };
        }
        macro_rules! write_chars {
            ($cond:ident => $c:expr) => { write_char!($cond, $c); };
            ($cond:ident => $c:expr, $($t:tt)+) => {
                write_char!($cond, $c);
                write_chars!($($t)+);
            };
        }

        write_chars!(
            is_bold => '1',
            is_dimmed => '2',
            is_italic => '3',
            is_underline => '4',
            is_blink => '5',
            is_reverse => '7',
            is_hidden => '8',
            is_strikethrough => '9'
        );

        // The foreground and background colours, if specified, need to be
        // handled specially because the number codes are more complicated.
        // (see `write_background_code` and `write_foreground_code`)
        if let Some(bg) = self.background {
            write_anything!();
            bg.write_background_code(f)?;
        }

        if let Some(fg) = self.foreground {
            write_anything!();
            fg.write_foreground_code(f)?;
        }

        if written_anything {
            // All the codes end with an `m`, because reasons.
            f.write_char('m')?;
        }

        Ok(written_anything)
    }

    /// Write any bytes that go *after* a piece of text to the given writer.
    #[inline]
    pub fn write_reset(f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(RESET)
    }
}

impl Colour {
    /// Write any bytes that go *before* a piece of text to the given writer.
    #[inline]
    pub fn write_prefix(self, f: &mut fmt::Formatter) -> Result<bool, fmt::Error> {
        self.normal().write_prefix(f)
    }
}

/// The code to send to reset all styles and return to `Style::default()`.
pub static RESET: &str = "\x1B[0m";

macro_rules! write_color {
    ($_self:ident, $f:ident =>
        $black:expr, $red:expr, $green:expr, $yellow:expr, $blue:expr,
        $purple:expr, $cyan:expr, $white:expr, $fixed:expr, $rgb:expr) => {{
        use Colour::*;
        match $_self {
            Black => $f.write_str($black),
            Red => $f.write_str($red),
            Green => $f.write_str($green),
            Yellow => $f.write_str($yellow),
            Blue => $f.write_str($blue),
            Purple => $f.write_str($purple),
            Cyan => $f.write_str($cyan),
            White => $f.write_str($white),
            Fixed(num) => {
                $f.write_str($fixed)?;
                num.fmt($f)
            }
            RGB(r, g, b) => {
                $f.write_str($rgb)?;
                r.fmt($f)?;
                $f.write_char(';')?;
                g.fmt($f)?;
                $f.write_char(';')?;
                b.fmt($f)
            }
        }
    }};
}

impl Colour {
    #[inline]
    fn write_foreground_code(self, f: &mut fmt::Formatter) -> fmt::Result {
        write_color!(self, f => "30", "31", "32", "33", "34", "35", "36", "37", "38;5;", "38;2;")
    }

    #[inline]
    fn write_background_code(self, f: &mut fmt::Formatter) -> fmt::Result {
        write_color!(self, f => "40", "41", "42", "43", "44", "45", "46", "47", "48;5;", "48;2;")
    }
}

#[cfg(test)]
mod test {
    use crate::{Colour::*, Style};

    macro_rules! test {
        ($name: ident: $style: expr; $input: expr => $result: expr) => {
            #[test]
            fn $name() {
                assert_eq!($style.paint($input).to_string(), $result.to_string());
            }
        };
    }

    test!(plain:                 Style::default();                  "text/plain" => "text/plain");
    test!(red:                   Red;                               "hi" => "\x1B[31mhi\x1B[0m");
    test!(black:                 Black.normal();                    "hi" => "\x1B[30mhi\x1B[0m");
    test!(yellow_bold:           Yellow.bold();                     "hi" => "\x1B[1;33mhi\x1B[0m");
    test!(yellow_bold_2:         Yellow.normal().bold();            "hi" => "\x1B[1;33mhi\x1B[0m");
    test!(blue_underline:        Blue.underline();                  "hi" => "\x1B[4;34mhi\x1B[0m");
    test!(green_bold_ul:         Green.bold().underline();          "hi" => "\x1B[1;4;32mhi\x1B[0m");
    test!(green_bold_ul_2:       Green.underline().bold();          "hi" => "\x1B[1;4;32mhi\x1B[0m");
    test!(purple_on_white:       Purple.on(White);                  "hi" => "\x1B[47;35mhi\x1B[0m");
    test!(purple_on_white_2:     Purple.normal().on(White);         "hi" => "\x1B[47;35mhi\x1B[0m");
    test!(yellow_on_blue:        Style::new().on(Blue).fg(Yellow);  "hi" => "\x1B[44;33mhi\x1B[0m");
    test!(yellow_on_blue_2:      Cyan.on(Blue).fg(Yellow);          "hi" => "\x1B[44;33mhi\x1B[0m");
    test!(cyan_bold_on_white:    Cyan.bold().on(White);             "hi" => "\x1B[1;47;36mhi\x1B[0m");
    test!(cyan_ul_on_white:      Cyan.underline().on(White);        "hi" => "\x1B[4;47;36mhi\x1B[0m");
    test!(cyan_bold_ul_on_white: Cyan.bold().underline().on(White); "hi" => "\x1B[1;4;47;36mhi\x1B[0m");
    test!(cyan_ul_bold_on_white: Cyan.underline().bold().on(White); "hi" => "\x1B[1;4;47;36mhi\x1B[0m");
    test!(fixed:                 Fixed(100);                        "hi" => "\x1B[38;5;100mhi\x1B[0m");
    test!(fixed_on_purple:       Fixed(100).on(Purple);             "hi" => "\x1B[45;38;5;100mhi\x1B[0m");
    test!(fixed_on_fixed:        Fixed(100).on(Fixed(200));         "hi" => "\x1B[48;5;200;38;5;100mhi\x1B[0m");
    test!(rgb:                   RGB(70,130,180);                   "hi" => "\x1B[38;2;70;130;180mhi\x1B[0m");
    test!(rgb_on_blue:           RGB(70,130,180).on(Blue);          "hi" => "\x1B[44;38;2;70;130;180mhi\x1B[0m");
    test!(blue_on_rgb:           Blue.on(RGB(70,130,180));          "hi" => "\x1B[48;2;70;130;180;34mhi\x1B[0m");
    test!(rgb_on_rgb:            RGB(70,130,180).on(RGB(5,10,15));  "hi" => "\x1B[48;2;5;10;15;38;2;70;130;180mhi\x1B[0m");
    test!(bold:                  Style::new().bold();               "hi" => "\x1B[1mhi\x1B[0m");
    test!(underline:             Style::new().underline();          "hi" => "\x1B[4mhi\x1B[0m");
    test!(bunderline:            Style::new().bold().underline();   "hi" => "\x1B[1;4mhi\x1B[0m");
    test!(dimmed:                Style::new().dimmed();             "hi" => "\x1B[2mhi\x1B[0m");
    test!(italic:                Style::new().italic();             "hi" => "\x1B[3mhi\x1B[0m");
    test!(blink:                 Style::new().blink();              "hi" => "\x1B[5mhi\x1B[0m");
    test!(reverse:               Style::new().reverse();            "hi" => "\x1B[7mhi\x1B[0m");
    test!(hidden:                Style::new().hidden();             "hi" => "\x1B[8mhi\x1B[0m");
    test!(stricken:              Style::new().strikethrough();      "hi" => "\x1B[9mhi\x1B[0m");

    macro_rules! test_fn {
        ($name:ident: $style:expr; $result:expr) => {
            #[test]
            fn $name() {
                let string = String::from("hi");
                let string: &str = &string;
                assert_eq!(
                    $style.paint_fn(|f| f.write_str(string)).to_string(),
                    $result.to_string()
                );
            }
        };
    }

    test_fn!(plain_fn:                 Style::default();                  "hi");
    test_fn!(red_fn:                   Red;                               "\x1B[31mhi\x1B[0m");
    test_fn!(black_fn:                 Black.normal();                    "\x1B[30mhi\x1B[0m");
    test_fn!(yellow_bold_fn:           Yellow.bold();                     "\x1B[1;33mhi\x1B[0m");
    test_fn!(yellow_bold_2_fn:         Yellow.normal().bold();            "\x1B[1;33mhi\x1B[0m");
    test_fn!(blue_underline_fn:        Blue.underline();                  "\x1B[4;34mhi\x1B[0m");
    test_fn!(green_bold_ul_fn:         Green.bold().underline();          "\x1B[1;4;32mhi\x1B[0m");
    test_fn!(green_bold_ul_2_fn:       Green.underline().bold();          "\x1B[1;4;32mhi\x1B[0m");
    test_fn!(purple_on_white_fn:       Purple.on(White);                  "\x1B[47;35mhi\x1B[0m");
    test_fn!(purple_on_white_2_fn:     Purple.normal().on(White);         "\x1B[47;35mhi\x1B[0m");
    test_fn!(yellow_on_blue_fn:        Style::new().on(Blue).fg(Yellow);  "\x1B[44;33mhi\x1B[0m");
    test_fn!(yellow_on_blue_2_fn:      Cyan.on(Blue).fg(Yellow);          "\x1B[44;33mhi\x1B[0m");
    test_fn!(cyan_bold_on_white_fn:    Cyan.bold().on(White);             "\x1B[1;47;36mhi\x1B[0m");
    test_fn!(cyan_ul_on_white_fn:      Cyan.underline().on(White);        "\x1B[4;47;36mhi\x1B[0m");
    test_fn!(cyan_bold_ul_on_white_fn: Cyan.bold().underline().on(White); "\x1B[1;4;47;36mhi\x1B[0m");
    test_fn!(cyan_ul_bold_on_white_fn: Cyan.underline().bold().on(White); "\x1B[1;4;47;36mhi\x1B[0m");
    test_fn!(fixed_fn:                 Fixed(100);                        "\x1B[38;5;100mhi\x1B[0m");
    test_fn!(fixed_on_purple_fn:       Fixed(100).on(Purple);             "\x1B[45;38;5;100mhi\x1B[0m");
    test_fn!(fixed_on_fixed_fn:        Fixed(100).on(Fixed(200));         "\x1B[48;5;200;38;5;100mhi\x1B[0m");
    test_fn!(rgb_fn:                   RGB(70,130,180);                   "\x1B[38;2;70;130;180mhi\x1B[0m");
    test_fn!(rgb_on_blue_fn:           RGB(70,130,180).on(Blue);          "\x1B[44;38;2;70;130;180mhi\x1B[0m");
    test_fn!(blue_on_rgb_fn:           Blue.on(RGB(70,130,180));          "\x1B[48;2;70;130;180;34mhi\x1B[0m");
    test_fn!(rgb_on_rgb_fn:            RGB(70,130,180).on(RGB(5,10,15));  "\x1B[48;2;5;10;15;38;2;70;130;180mhi\x1B[0m");
    test_fn!(bold_fn:                  Style::new().bold();               "\x1B[1mhi\x1B[0m");
    test_fn!(underline_fn:             Style::new().underline();          "\x1B[4mhi\x1B[0m");
    test_fn!(bunderline_fn:            Style::new().bold().underline();   "\x1B[1;4mhi\x1B[0m");
    test_fn!(dimmed_fn:                Style::new().dimmed();             "\x1B[2mhi\x1B[0m");
    test_fn!(italic_fn:                Style::new().italic();             "\x1B[3mhi\x1B[0m");
    test_fn!(blink_fn:                 Style::new().blink();              "\x1B[5mhi\x1B[0m");
    test_fn!(reverse_fn:               Style::new().reverse();            "\x1B[7mhi\x1B[0m");
    test_fn!(hidden_fn:                Style::new().hidden();             "\x1B[8mhi\x1B[0m");
    test_fn!(stricken_fn:              Style::new().strikethrough();      "\x1B[9mhi\x1B[0m");

    #[test]
    fn test_move() {
        let string = String::from("hi");
        assert_eq!(
            Style::default()
                .paint_fn(|f| f.write_str(&string))
                .to_string(),
            "hi"
        );
    }

    #[test]
    fn test_ref() {
        let string = &String::from("hi");
        assert_eq!(
            Style::default()
                .paint_fn(|f| f.write_str(string))
                .to_string(),
            "hi"
        );
    }

    #[test]
    fn test_debug() {
        let a = vec![1, 2, 3];
        assert_eq!(
            Style::default()
                .paint_fn(|f| std::fmt::Debug::fmt(&a, f))
                .to_string(),
            "[1, 2, 3]"
        );
        assert_eq!(
            Style::default()
                .bold()
                .paint_fn(|f| std::fmt::Debug::fmt(&a, f))
                .to_string(),
            "\x1B[1m[1, 2, 3]\x1B[0m"
        );
    }

    #[test]
    fn test_write() {
        assert_eq!(
            Style::default()
                .paint_fn(|f| write!(f, "{:.5}", 1.0))
                .to_string(),
            "1.00000"
        );
        assert_eq!(
            Style::default()
                .bold()
                .paint_fn(|f| write!(f, "{:.5}", 1.0))
                .to_string(),
            "\x1B[1m1.00000\x1B[0m"
        );
    }

    /// Can not write the same `impl Display` two or more times
    /// else return error
    #[test]
    fn test_error() {
        use std::fmt::Write;
        let a = Style::default().paint("foo");
        let _ = a.to_string();
        let mut b = String::new();

        assert!(write!(b, "{}", a).is_err());
    }
}
