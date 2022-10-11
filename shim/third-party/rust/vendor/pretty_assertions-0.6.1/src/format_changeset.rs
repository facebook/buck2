use ansi_term::Colour::{Fixed, Green, Red};
use ansi_term::Style;
use difference::{Changeset, Difference};
use std::fmt;

macro_rules! paint {
    ($f:ident, $colour:expr, $fmt:expr, $($args:tt)*) => (
        write!($f, "{}", $colour.paint(format!($fmt, $($args)*)))
    )
}

const SIGN_RIGHT: char = '>'; // + > →
const SIGN_LEFT: char = '<'; // - < ←

// Adapted from:
// https://github.com/johannhof/difference.rs/blob/c5749ad7d82aa3d480c15cb61af9f6baa08f116f/examples/github-style.rs
// Credits johannhof (MIT License)

pub fn format_changeset(f: &mut fmt::Formatter, changeset: &Changeset) -> fmt::Result {
    let ref diffs = changeset.diffs;

    writeln!(
        f,
        "{} {} / {} :",
        Style::new().bold().paint("Diff"),
        Red.paint(format!("{} left", SIGN_LEFT)),
        Green.paint(format!("right {}", SIGN_RIGHT))
    )?;
    for i in 0..diffs.len() {
        match diffs[i] {
            Difference::Same(ref same) => {
                // Have to split line by line in order to have the extra whitespace
                // at the beginning.
                for line in same.split('\n') {
                    writeln!(f, " {}", line)?;
                }
            }
            Difference::Add(ref added) => {
                let prev = i.checked_sub(1).and_then(|x| diffs.get(x));
                match prev {
                    Some(&Difference::Rem(ref removed)) => {
                        // The addition is preceded by an removal.
                        //
                        // Let's highlight the character-differences in this replaced
                        // chunk. Note that this chunk can span over multiple lines.
                        format_replacement(f, added, removed)?;
                    }
                    _ => {
                        for line in added.split('\n') {
                            paint!(f, Green, "{}{}\n", SIGN_RIGHT, line)?;
                        }
                    }
                };
            }
            Difference::Rem(ref removed) => {
                let next = i.checked_add(1).and_then(|x| diffs.get(x));
                match next {
                    Some(&Difference::Add(_)) => {
                        // The removal is followed by an addition.
                        //
                        // ... we'll handle both in the next iteration.
                    }
                    _ => {
                        for line in removed.split('\n') {
                            paint!(f, Red, "{}{}\n", SIGN_LEFT, line)?;
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

macro_rules! join {
    (
        $elem:ident in ($iter:expr) {
            $( $body:tt )*
        } seperated by {
            $( $separator:tt )*
        }
    ) => (
        let mut iter = $iter;

        if let Some($elem) = iter.next() {
            $( $body )*
        }

        for $elem in iter {
            $( $separator )*
            $( $body )*
        }
    )
}

pub fn format_replacement(f: &mut fmt::Write, added: &str, removed: &str) -> fmt::Result {
    let Changeset { diffs, .. } = Changeset::new(removed, added, "");

    // LEFT side (==what's been)
    paint!(f, Red, "{}", SIGN_LEFT)?;
    for c in &diffs {
        match *c {
            Difference::Same(ref word_diff) => {
                join!(chunk in (word_diff.split('\n')) {
                    paint!(f, Red, "{}", chunk)?;
                } seperated by {
                    writeln!(f)?;
                    paint!(f, Red, "{}", SIGN_LEFT)?;
                });
            }
            Difference::Rem(ref word_diff) => {
                join!(chunk in (word_diff.split('\n')) {
                    paint!(f, Red.on(Fixed(52)).bold(), "{}", chunk)?;
                } seperated by {
                    writeln!(f)?;
                    paint!(f, Red.bold(), "{}", SIGN_LEFT)?;
                });
            }
            _ => (),
        }
    }
    writeln!(f, "")?;

    // RIGHT side (==what's new)
    paint!(f, Green, "{}", SIGN_RIGHT)?;
    for c in &diffs {
        match *c {
            Difference::Same(ref word_diff) => {
                join!(chunk in (word_diff.split('\n')) {
                    paint!(f, Green, "{}", chunk)?;
                } seperated by {
                    writeln!(f)?;
                    paint!(f, Green, "{}", SIGN_RIGHT)?;
                });
            }
            Difference::Add(ref word_diff) => {
                join!(chunk in (word_diff.split('\n')) {
                    paint!(f, Green.on(Fixed(22)).bold(), "{}", chunk)?;
                } seperated by {
                    writeln!(f)?;
                    paint!(f, Green.bold(), "{}", SIGN_RIGHT)?;
                });
            }
            _ => (),
        }
    }

    writeln!(f, "")
}

#[test]
fn test_format_replacement() {
    let added = "    84,\
                 \n    248,";
    let removed = "    0,\
                   \n    0,\
                   \n    128,";

    let mut buf = String::new();
    let _ = format_replacement(&mut buf, added, removed);

    println!(
        "## removed ##\
         \n{}\
         \n## added ##\
         \n{}\
         \n## diff ##\
         \n{}",
        removed, added, buf
    );

    assert_eq!(
        buf,
        "\u{1b}[31m<\u{1b}[0m\u{1b}[31m    \u{1b}[0m\u{1b}[1;48;5;52;31m0\u{1b}[0m\u{1b}[31m,\u{1b}[0m\n\u{1b}[31m<\u{1b}[0m\u{1b}[31m    \u{1b}[0m\u{1b}[1;48;5;52;31m0,\u{1b}[0m\n\u{1b}[1;31m<\u{1b}[0m\u{1b}[1;48;5;52;31m    1\u{1b}[0m\u{1b}[31m2\u{1b}[0m\u{1b}[31m8,\u{1b}[0m\n\u{1b}[32m>\u{1b}[0m\u{1b}[32m    \u{1b}[0m\u{1b}[1;48;5;22;32m84\u{1b}[0m\u{1b}[32m,\u{1b}[0m\n\u{1b}[32m>\u{1b}[0m\u{1b}[32m    \u{1b}[0m\u{1b}[32m2\u{1b}[0m\u{1b}[1;48;5;22;32m4\u{1b}[0m\u{1b}[32m8,\u{1b}[0m\n"
    );
}
