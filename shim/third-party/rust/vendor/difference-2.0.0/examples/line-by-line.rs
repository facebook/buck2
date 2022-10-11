extern crate term;
extern crate difference;
use difference::{Difference, Changeset};
use std::io::Write;

/*
 * The only thing to do here is to create a diff based on line
 * splits (passing the newline character as a split symbol)
 * and iterate over the results, matching and formatting them based
 * on the type of `Difference`.
 *
 * Screenshot:
 * https://raw.githubusercontent.com/johannhof/difference.rs/master/assets/git-style.png
 */

#[allow(unused_must_use)]
#[cfg_attr(feature = "cargo-clippy", allow(needless_range_loop))]
fn main() {
    let text1 = "Roses are red, violets are blue,\n\
               I wrote this library here,\n\
               just for you.\n\
               (It's true).";

    let text2 = "Roses are red, violets are blue,\n\
               I wrote this documentation here,\n\
               just for you.\n\
               (It's quite true).";

    // Compare both texts, the third parameter defines the split level.
    let Changeset { diffs, .. } = Changeset::new(text1, text2, "\n");

    let mut t = term::stdout().unwrap();

    for i in 0..diffs.len() {
        match diffs[i] {
            Difference::Same(ref x) => {
                t.reset().unwrap();
                writeln!(t, " {}", x);
            }
            Difference::Add(ref x) => {
                t.fg(term::color::GREEN).unwrap();
                writeln!(t, "+{}", x);
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
