extern crate difference;
extern crate quickcheck;

use difference::{Changeset, Difference};
use quickcheck::{TestResult, quickcheck, QuickCheck};
use std::fmt;

const DEBUG: bool = false;

struct Check<'a> {
    old: &'a str,
    new: &'a str,
    changeset: Changeset,
}

impl<'a> fmt::Display for Check<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Changeset::new({:?}, {:?}, {:?}) -> [",
            self.old,
            self.new,
            self.changeset.split
        )?;

        let mut iter = self.changeset.diffs.iter();
        if let Some(d) = iter.next() {
            write!(f, "{:?}", d)?;
        }
        for d in iter {
            write!(f, " {:?}", d)?;
        }
        write!(f, "]")
    }
}

fn check_changeset(old: &str, new: &str, split: &str) -> TestResult {
    Check::new(old, new, split).check()
}

impl<'a> Check<'a> {
    fn new(old: &'a str, new: &'a str, split: &'a str) -> Check<'a> {
        Check {
            old: old,
            new: new,
            changeset: Changeset::new(old, new, split),
        }
    }

    fn check(&self) -> TestResult {
        let split = &self.changeset.split;

        let mut old: Vec<&str> = Vec::new();
        let mut new: Vec<&str> = Vec::new();

        for d in &self.changeset.diffs {
            if DEBUG {
                println!("assert `{:?}` (old: {:?}, new: {:?})", d, old, new);
            }

            match *d {
                Difference::Same(ref x) => {
                    old.push(x);
                    new.push(x);
                }
                Difference::Add(ref x) => {
                    new.push(x);
                }
                Difference::Rem(ref x) => {
                    old.push(x);
                }
            }
        }
        let got_old = old.join(split);
        let got_new = new.join(split);
        if got_old != self.old {
            return TestResult::error(format!("Diff output implies old=`{:?}`, not `{:?}` in {}",
                        got_old, self.old, self,
                ));
        }
        if got_new != self.new {
            return TestResult::error(format!("Diff output implies new=`{:?}`, not `{:?}` in {}",
                        got_new, self.new, self,
                ));
        }

        TestResult::passed()
    }
}

#[test]
fn simple() {
    quickcheck(check_changeset("a", "a a", " "));
}

#[test]
fn issue_19() {
    // https://github.com/johannhof/difference.rs/issues/19
    quickcheck(check_changeset("a b : g", "b a : b b : g g", " "));
}

#[test]
#[allow(needless_pass_by_value)]
fn fuzzy() {
    fn prop(old: Vec<usize>, new: Vec<usize>, words: Vec<char>) -> TestResult {
        if words.is_empty() {
            return TestResult::discard();
        }

        fn map_to_words(input: &[usize], words: &[char]) -> String {
            input.iter().enumerate().fold(
                String::new(),
                |mut acc, (i, x)| {
                    if i > 0 {
                        acc.push(' ');
                    }
                    acc.push(words[x % words.len()]);
                    acc
                },
            )
        }
        let old = map_to_words(&old, &words);
        let new = map_to_words(&new, &words);

        check_changeset(&old, &new, " ")
    }

    QuickCheck::new()
        .tests(100) // max successful tests
        .max_tests(10_000) // max attempts
        .quickcheck(prop as fn(Vec<usize>, Vec<usize>, Vec<char>) -> TestResult);
}
