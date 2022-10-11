

use super::{Changeset, Difference};
use std::fmt;

impl fmt::Display for Changeset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for d in &self.diffs {
            match *d {
                Difference::Same(ref x) => {
                    try!(write!(f, "{}{}", x, self.split));
                }
                Difference::Add(ref x) => {
                    try!(write!(f, "\x1b[92m{}\x1b[0m{}", x, self.split));
                }
                Difference::Rem(ref x) => {
                    try!(write!(f, "\x1b[91m{}\x1b[0m{}", x, self.split));
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::super::Changeset;
    use std::io::Write;
    use std::iter::FromIterator;
    use std::thread;
    use std::time;

    /// convert slice to vector for assert_eq
    fn vb(b: &'static [u8]) -> Vec<u8> {
        Vec::from_iter(b.iter().cloned())
    }

    /// if the format changes, you can use this to help create the test for color
    /// just pass it in and copy-paste (validating that it looks right first of course...)
    #[allow(dead_code)]
    fn debug_bytes(result: &[u8], expected: &[u8]) {
        // sleep for a bit so stderr passes us
        thread::sleep(time::Duration::new(0, 2e8 as u32));
        println!("Debug Result:");
        for b in result {
            print!("{}", *b as char);
        }
        println!("Repr Result:");
        repr_bytes(result);
        println!("");
        println!("--Result Repr DONE");

        println!("Debug Expected:");
        for b in expected {
            print!("{}", *b as char);
        }
        println!("Repr Expected:");
        repr_bytes(expected);
        println!("");
        println!("--Expected Repr DONE");
    }

    /// for helping debugging what the actual bytes are
    /// for writing user tests
    fn repr_bytes(bytes: &[u8]) {
        for b in bytes {
            match *b {
                // 9 => print!("{}", *b as char), // TAB
                b'\n' => print!("\\n"),
                b'\r' => print!("\\r"),
                32...126 => print!("{}", *b as char), // visible ASCII
                _ => print!(r"\x{:0>2x}", b),

            }
        }
    }

    #[test]
    fn test_display() {
        let text1 = "Roses are red, violets are blue,\n\
                     I wrote this library,\n\
                     just for you.\n\
                     (It's true).";

        let text2 = "Roses are red, violets are blue,\n\
                     I wrote this documentation,\n\
                     just for you.\n\
                     (It's quite true).";
        let expected = b"Roses are red, violets are blue,\n\x1b[91mI wrote this library,\x1b\
            [0m\n\x1b[92mI wrote this documentation,\x1b[0m\njust for you.\n\x1b\
            [91m(It's true).\x1b[0m\n\x1b[92m(It's quite true).\x1b[0m\n";

        let ch = Changeset::new(text1, text2, "\n");
        let mut result: Vec<u8> = Vec::new();
        write!(result, "{}", ch).unwrap();
        debug_bytes(&result, expected);
        assert_eq!(result, vb(expected));

    }
}
