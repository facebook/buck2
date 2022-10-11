//! This module provides two interfaces for accessing clusters from an underlying string. The
//! `GraphemeCluster` trait extends the `Peekable` iterators over `Chars` or `CharIndices`
//! to add a `next_cluster` method which returns `Option<String>` with the next
//! cluster if one exists. This is the best method for getting individual clusters from a stream which is normally
//! only getting `char`s but is not recommended if you wish to iterate over clusters.
//! ```
//! # use crate::finl_unicode::grapheme_clusters::GraphemeCluster;
//! let mut char_iterator = "A\u{301}✋🏽🇦🇹!".chars().peekable();
//! assert_eq!(char_iterator.next_cluster(), Some("A\u{301}".to_string()));
//! assert_eq!(char_iterator.next_cluster(), Some("✋🏽".to_string()));
//! assert_eq!(char_iterator.next_cluster(), Some("🇦🇹".to_string()));
//! assert_eq!(char_iterator.next_cluster(), Some("!".to_string()));
//! assert_eq!(char_iterator.next_cluster(), None);
//! ```
//!
//! For the iterating over clusters case there is a struct `Graphemes` which implements `iterator`
//! and can be constructed from a `&str`. This returns references to substrings of the original
//! `&str` and is more performant for that case than the extended iterator provided through
//! `GraphemeCluster` which allocates a new `String` for each cluster found.
//! ```
//! # use crate::finl_unicode::grapheme_clusters::Graphemes;
//! let graphemes = Graphemes::new("A\u{301}✋🏽🇦🇹!");
//! assert_eq!(graphemes.collect::<Vec<&str>>(), ["A\u{301}", "✋🏽", "🇦🇹", "!"])
//! ```

use std::iter::Peekable;
use std::str::CharIndices;
use std::str::Chars;
use crate::data::grapheme_property::{GP_PAGES,GP_TABLE};


/// `Graphemes` provides an iterator over the grapheme clusters of a string.
pub struct Graphemes<'a> {
    input: &'a str,
    iter: Peekable<CharIndices<'a>>,
}

impl<'a> Graphemes<'a> {
    /// A new instance of graphemes can be constructed from a string using `Graphemes::new`
    /// ```
    /// # use crate::finl_unicode::grapheme_clusters::Graphemes;
    /// let graphemes = Graphemes::new("some string");
    /// ```
    pub fn new(input: &'a str) -> Graphemes<'a> {
        let iter = input.char_indices().peekable();
        Graphemes {
            input,
            iter
        }
    }
}

impl<'a> Iterator for Graphemes<'a> {
    type Item = &'a str;
    #[inline]
    /// Return a slice of the underlying
    /// string corresponding to the next cluster if one exists, or `None` if the end of the string
    /// has been reached.
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(&(start, _)) = self.iter.peek() {
            let mut cluster_machine = ClusterMachine::new();
            loop {
                if let Some(&(curr_loc, ch)) = self.iter.peek() {
                    match cluster_machine.find_cluster(ch) {
                        Break::None => { self.iter.next(); }
                        Break::Before => {
                            return Some(&self.input[start..curr_loc]);
                        }
                        Break::After => {
                            self.iter.next();
                            return Some(
                                if let Some(&(curr_loc, _)) = self.iter.peek() {
                                    &self.input[start..curr_loc]
                                } else {
                                    &self.input[start..]
                                });
                        }
                    }
                }
                else {
                    return Some(&self.input[start..]);
                }
            }
        } else {
            None
        }
    }
}

/// Get the next grapheme cluster from a stream of characters or char indices
/// This trait is implemented for `Peekable<CharIndices>` and `Peekable<Chars>`.
pub trait GraphemeCluster<T> {
    fn next_cluster(&mut self) -> Option<String>;
}

impl<T> GraphemeCluster<T> for T where T: PeekChar {
    /// Returns the next cluster if there is one in an `Option<String>`. Since this has a heap allocation
    /// it is *not* recommended for iterating over all the clusters in a string. In that case, use
    /// `Graphemes` instead.
    #[inline]
    fn next_cluster(&mut self) -> Option<String> {
        if self.has_next() {
            let mut cluster_machine = ClusterMachine::new();
            let mut rv = String::new();
            loop {
                if let Some(&ch) = self.peek_char() {
                    let state = cluster_machine.find_cluster(ch);
                    match state {
                        Break::None => {
                            rv.push(ch);
                            self.next();
                        }
                        Break::Before => { return Some(rv); }
                        Break::After => {
                            rv.push(ch);
                            self.next();
                            return Some(rv);
                        }
                    }
                } else {
                    break;
                }
            }
            Some(rv)
        } else {
            None
        }
    }

}

/// This trait exists primarily to allow a single implementation to be used for both `Peekable<Chars>`
/// and `Peekable<CharIndices>`. You could implement this for some other iterator if you like as
/// long as you can implement the two methods below.
pub trait PeekChar: Iterator {
    /// Returns the next character (if it exists) or `None` otherwise.
    fn peek_char(&mut self) -> Option<&char>;
    /// Returns `true` if there is another character available on the iterator, `false` otherwise.
    fn has_next(&mut self) -> bool;
}


impl PeekChar for Peekable<Chars<'_>> {
    #[inline]
    fn peek_char(&mut self) -> Option<&char> {
        self.peek()
    }

    #[inline]
    fn has_next(&mut self) -> bool {
        self.peek().is_some()
    }

}

impl PeekChar for Peekable<CharIndices<'_>> {
    #[inline]
    fn peek_char(&mut self) -> Option<&char> {
        self.peek().map(|(_, ch)|  ch)
    }

    #[inline]
    fn has_next(&mut self) -> bool {
        self.peek().is_some()
    }
}

// ------------------------
// Private implementation details follow

#[derive(PartialEq)]
enum ClusterMachineState {
    Start,
    Precore,
    CcsBase,
    CrLf,
    HangulSyllableL,
    HangulSyllableV,
    HangulSyllableT,
    CcsExtend,
    Flag,
    Emoji,
    EmojiZWJ,
    Other,
}

#[derive(Debug, PartialEq)]
enum Break {
    None,
    Before,
    After,
}

struct ClusterMachine {
    state: ClusterMachineState,
}

impl ClusterMachine {
    #[inline]
    pub fn new() -> ClusterMachine {
        ClusterMachine {
            state: ClusterMachineState::Start,
        }
    }

    /// If we have a cluster, we return the cluster in a `String` in an `Option` long with a `bool`
    /// If the `bool` is true, it means that we are also consuming the character  in the cluster.
    #[inline]
    pub fn find_cluster(&mut self, c: char) -> Break {
        if self.state == ClusterMachineState::Start {
            return self.first_character(c);
        }
        let property = get_property(c);

        if property == GraphemeProperty::CONTROL {
            return if self.state == ClusterMachineState::CrLf && c == '\n' {
                self.state = ClusterMachineState::Start;
                Break::After
            } else {
                if c == '\r' {
                    self.state = ClusterMachineState::CrLf;
                } else {
                    self.state = ClusterMachineState::Start;
                }
                Break::Before
            }
        }

        match self.state {
            ClusterMachineState::Start => self.first_character(c),
            ClusterMachineState::Precore => {
                self.first_character(c);
                Break::None
            }

            ClusterMachineState::HangulSyllableL => {
                match property {
                    GraphemeProperty::L => Break::None,
                    GraphemeProperty::V | GraphemeProperty::LV => {
                        self.state = ClusterMachineState::HangulSyllableV;
                        Break::None
                    }
                    GraphemeProperty::LVT => {
                        self.state = ClusterMachineState::HangulSyllableT;
                        Break::None
                    }
                    GraphemeProperty::EXTEND | GraphemeProperty::SPACING_MARK | GraphemeProperty::ZWJ => {
                        self.state = ClusterMachineState::CcsBase;
                        Break::None
                    }
                    _ => {
                        self.first_character(c);
                        Break::Before
                    }
                }
            }
            ClusterMachineState::HangulSyllableV => {
                match property {
                    GraphemeProperty::V => Break::None,
                    GraphemeProperty::T => {
                        self.state = ClusterMachineState::HangulSyllableT;
                        Break::None
                    }
                    GraphemeProperty::EXTEND | GraphemeProperty::SPACING_MARK | GraphemeProperty::ZWJ => {
                        self.state = ClusterMachineState::CcsBase;
                        Break::None
                    }
                    _ => {
                        self.first_character(c);
                        Break::Before
                    }
                }
            }
            ClusterMachineState::HangulSyllableT => {
                match property {
                    GraphemeProperty::T => Break::None,
                    GraphemeProperty::EXTEND | GraphemeProperty::SPACING_MARK | GraphemeProperty::ZWJ => {
                        self.state = ClusterMachineState::CcsBase;
                        Break::None
                    }
                    _ => {
                        self.first_character(c);
                        Break::Before
                    }
                }
            }
            ClusterMachineState::CcsExtend => {
                match property {
                    GraphemeProperty::EXTEND
                    | GraphemeProperty::SPACING_MARK
                    | GraphemeProperty::ZWJ => Break::None,
                    _ => Break::Before
                }
            }
            ClusterMachineState::Flag => {
                self.state = ClusterMachineState::Start;
                match property {
                    GraphemeProperty::REGIONAL_INDICATOR => {
                        self.state = ClusterMachineState::Other;
                        Break::None
                    }
                    GraphemeProperty::EXTEND
                    | GraphemeProperty::SPACING_MARK
                    | GraphemeProperty::ZWJ => {
                        self.state = ClusterMachineState::CcsExtend;
                        Break::None
                    }
                    _ => {
                        self.first_character(c);
                        Break::Before
                    }
                }
            }
            ClusterMachineState::Emoji => {
                match property {
                    GraphemeProperty::ZWJ => {
                        self.state = ClusterMachineState::EmojiZWJ;
                        Break::None
                    }
                    GraphemeProperty::EXTEND | GraphemeProperty::SPACING_MARK => {
                        self.state = ClusterMachineState::Emoji;
                        Break::None
                    }
                    _ => {
                        self.first_character(c);
                        Break::Before
                    }
                }
            }
            ClusterMachineState::EmojiZWJ => {
                if property == GraphemeProperty::EXTENDED_GRAPHEME {
                    self.state = ClusterMachineState::Emoji;
                    Break::None
                } else {
                    Break::Before
                }
            }
            ClusterMachineState::CrLf => Break::Before,
            _ => {
                if is_continuation(property) {
                    Break::None
                } else {
                    self.first_character(c);
                    Break::Before
                }
            }
        }
    }
    #[inline]
    fn first_character(&mut self, c: char) -> Break {
        if c == '\r' {
            self.state = ClusterMachineState::CrLf;
            return Break::None;
        }
        let property = get_property(c);
        if property == GraphemeProperty::CONTROL {
            self.state = ClusterMachineState::Start;
            return Break::After;
        }
        match property {
            GraphemeProperty::PREPEND => {
                self.state = ClusterMachineState::Precore;
            }
            GraphemeProperty::EXTEND => {
                self.state = ClusterMachineState::CcsExtend;
            }
            GraphemeProperty::SPACING_MARK => {
                self.state = ClusterMachineState::CcsExtend;
            }
            GraphemeProperty::L => {
                self.state = ClusterMachineState::HangulSyllableL;
            }
            GraphemeProperty::V => {
                self.state = ClusterMachineState::HangulSyllableV;
            }
            GraphemeProperty::T => {
                self.state = ClusterMachineState::HangulSyllableT;
            }
            GraphemeProperty::LV => {
                self.state = ClusterMachineState::HangulSyllableV;
            }
            GraphemeProperty::LVT => {
                self.state = ClusterMachineState::HangulSyllableT;
            }
            GraphemeProperty::EXTENDED_GRAPHEME => {
                self.state = ClusterMachineState::Emoji;
            }
            GraphemeProperty::REGIONAL_INDICATOR => {
                self.state = ClusterMachineState::Flag;
            }
            _ => {
                self.state = ClusterMachineState::Other;
            }
        }
        Break::None
    }
}

#[inline]
fn is_continuation(property: u8) -> bool {
    property != 0 && property & 0xc == 0
}


// Symbolic names for properties in data tables
struct GraphemeProperty {}
impl GraphemeProperty {
    const EXTEND: u8 = 0x01;
    const SPACING_MARK: u8 = 0x02;
    const ZWJ: u8 = 0x03;
    const CONTROL: u8 = 0x04;
    const PREPEND: u8 = 0x05;
    const EXTENDED_GRAPHEME: u8 = 0x06;
    const REGIONAL_INDICATOR: u8 = 0x07;
    const L: u8 = 0x0c;
    const V: u8 = 0x08;
    const T: u8 = 0x09;
    const LV: u8 = 0x0d;
    const LVT: u8 = 0x0e;
}


#[inline]
fn get_property(c: char) -> u8 {
    GP_PAGES[usize::from(GP_TABLE[(c as usize) >> 8])][(c as usize) & 0xff]
}



#[cfg(test)]
pub (crate) mod tests {
    use crate::grapheme_clusters::*;

    #[test]
    fn low_level_interface_test() {
        let mut machine = ClusterMachine::new();
        assert_eq!(machine.find_cluster('\r'), Break::None);
        assert_eq!(machine.find_cluster('a'), Break::Before);
        assert_eq!(machine.find_cluster('\r'), Break::Before);
        assert_eq!(machine.find_cluster('\n'), Break::After);
    }

    #[test]
    fn can_get_clusters() {
        let mut peekable_index = "\r\ne\u{301}f".char_indices().peekable();
        assert_eq!(Some("\r\n".to_string()), peekable_index.next_cluster());
        assert_eq!(Some("e\u{301}".to_string()), peekable_index.next_cluster());
        assert_eq!(Some("f".to_string()), peekable_index.next_cluster());
    }

    pub (crate) fn grapheme_test(input: &str, expected_output: &[&str], message: &str) {
        let mut iter = input.char_indices().peekable();
        let mut clusters = vec!();
        while let Some(cluster) = iter.next_cluster() {
            clusters.push(cluster);
        }
        assert_eq!(clusters.len(), expected_output.len(), "Lengths did not match on Grapheme Cluster\n\t{message}\n\tOutput: {clusters:?}\n\tExpected: {expected_output:?}");
        clusters.iter().zip(expected_output.into_iter())
            .for_each(|(actual, &expected)| assert_eq!(actual.as_str(), expected, "GraphemeCluster mismatch: {message}"));

        let iter = Graphemes::new(input);
        let clusters = iter.collect::<Vec<&str>>();
        assert_eq!(clusters.len(), expected_output.len(), "Lengths did not match on Grapheme Cluster Indices\n\t{message}\n\tOutput: {clusters:?}\n\tExpected: {expected_output:?}");
        clusters.iter().zip(expected_output.into_iter())
            .for_each(|(actual, &expected)| assert_eq!(*actual, expected, "Grapheme cluster indices mismatch: {message}\n{} ≠ {}", actual.escape_unicode(), expected.escape_unicode()));
    }

}