extern crate term;
extern crate difference;
use difference::{Difference, Changeset};
use std::io::Write;

// Screenshot:
// https://raw.githubusercontent.com/johannhof/difference.rs/master/assets/github-style.png

#[allow(unused_must_use)]
fn main() {
    let text1 = "Roses are red, violets are blue,\n\
                 I wrote this library here,\n\
                 just for you.\n\
                 (It's true).";

    let text2 = "Roses are red, violets are blue,\n\
                 I wrote this documentation here,\n\
                 just for you.\n\
                 (It's quite true).";

    let Changeset { diffs, .. } = Changeset::new(text1, text2, "\n");

    let mut t = term::stdout().unwrap();

    for i in 0..diffs.len() {
        match diffs[i] {
            Difference::Same(ref x) => {
                t.reset().unwrap();
                writeln!(t, " {}", x);
            }
            Difference::Add(ref x) => {
                match diffs[i - 1] {
                    Difference::Rem(ref y) => {
                        t.fg(term::color::GREEN).unwrap();
                        write!(t, "+");
                        let Changeset { diffs, .. } = Changeset::new(y, x, " ");
                        for c in diffs {
                            match c {
                                Difference::Same(ref z) => {
                                    t.fg(term::color::GREEN).unwrap();
                                    write!(t, "{}", z);
                                    write!(t, " ");
                                }
                                Difference::Add(ref z) => {
                                    t.fg(term::color::WHITE).unwrap();
                                    t.bg(term::color::GREEN).unwrap();
                                    write!(t, "{}", z);
                                    t.reset().unwrap();
                                    write!(t, " ");
                                }
                                _ => (),
                            }
                        }
                        writeln!(t, "");
                    }
                    _ => {
                        t.fg(term::color::BRIGHT_GREEN).unwrap();
                        writeln!(t, "+{}", x);
                    }
                };
            }
            Difference::Rem(ref x) => {
                t.fg(term::color::RED).unwrap();
                writeln!(t, "-{}", x);
            }
        }
    }
    t.reset().unwrap();
    t.flush().unwrap();
}
