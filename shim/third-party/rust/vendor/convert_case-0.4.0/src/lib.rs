//! Converts to and from various cases.
//!
//! # Command Line Utility `ccase`
//!
//! Since version "0.3.0" this crate is just a case conversion _library_.  The command line utility
//! that uses the tools in this library has been moved to the `ccase` crate.  You can read about it
//! at the [github repository](https://github.com/rutrum/convert-case/tree/master/ccase).
//!
//! # Rust Library
//!
//! Provides a [`Case`](enum.Case.html) enum which defines a variety of cases to convert into.
//! A `Case` can be used with an item that implements the [`Casing`](trait.Casing.html) trait,
//! which allows the item to be converted to a given case.
//!
//! You can convert a string or string slice into a case using the `to_case` method.
//! ```
//! use convert_case::{Case, Casing};
//!
//! assert_eq!("Ronnie James Dio", "ronnie james dio".to_case(Case::Title));
//! assert_eq!("ronnieJamesDio", "Ronnie_James_dio".to_case(Case::Camel));
//! assert_eq!("Ronnie-James-Dio", "RONNIE_JAMES_DIO".to_case(Case::Train));
//! ```
//!
//! By default, `to_case` will split along all word boundaries, that is
//! * space characters ` `,
//! * underscores `_`,
//! * hyphens `-`,
//! * and changes in capitalization `aA`.
//!
//! For more accuracy, the `from_case` method splits based on the word boundaries
//! of a particular case.  For example, splitting from snake case will only treat
//! underscores as word boundaries.
//! ```
//! use convert_case::{Case, Casing};
//!
//! assert_eq!(
//!     "2020 04 16 My Cat Cali",
//!     "2020-04-16_my_cat_cali".to_case(Case::Title)
//! );
//! assert_eq!(
//!     "2020-04-16 My Cat Cali",
//!     "2020-04-16_my_cat_cali".from_case(Case::Snake).to_case(Case::Title)
//! );
//! ```
//!
//! By default (and when converting from camel case or similar cases) `convert_case`
//! will detect acronyms.  It also ignores any leading, trailing, or deplicate delimeters.
//! ```
//! use convert_case::{Case, Casing};
//!
//! assert_eq!("io_stream", "IOStream".to_case(Case::Snake));
//! assert_eq!("my_json_parser", "myJSONParser".to_case(Case::Snake));
//!
//! assert_eq!("weird_var_name", "__weird--var _name-".to_case(Case::Snake));
//! ```
//!
//! It also works non-ascii characters.  However, no inferences on the language itself is made.
//! For instance, the diagraph `ij` in dutch will not be capitalized, because it is represented
//! as two distinct unicode characters.  However, `æ` would be capitalized.
//! ```
//! use convert_case::{Case, Casing};
//!
//! assert_eq!("granat-äpfel", "GranatÄpfel".to_case(Case::Kebab));
//!
//! // The example from str::to_lowercase documentation
//! let odysseus = "ὈΔΥΣΣΕΎΣ";
//! assert_eq!("ὀδυσσεύς", odysseus.to_case(Case::Lower));
//! ```
//! 
//! For the purposes of case conversion, characters followed by numerics and vice-versa are 
//! considered word boundaries.  In addition, any special ascii characters (besides `_` and `-`) 
//! are ignored.
//! ```
//! use convert_case::{Case, Casing};
//! 
//! assert_eq!("e_5150", "E5150".to_case(Case::Snake));
//! assert_eq!("10,000_days", "10,000Days".to_case(Case::Snake));
//! assert_eq!("HELLO, WORLD!", "Hello, world!".to_case(Case::Upper));
//! assert_eq!("One\ntwo\nthree", "ONE\nTWO\nTHREE".to_case(Case::Title));
//! ```
//!
//! # Note on Accuracy
//!
//! The `Casing` methods `from_case` and `to_case` do not fail.  Conversion to a case will always
//! succeed.  However, the results can still be unexpected.  Failure to detect any word boundaries
//! for a particular case means the entire string will be considered a single word.
//! ```
//! use convert_case::{Case, Casing};
//!
//! // Mistakenly parsing using Case::Snake
//! assert_eq!("My-kebab-var", "my-kebab-var".from_case(Case::Snake).to_case(Case::Title));
//!
//! // Converts using an unexpected method
//! assert_eq!("my_kebab_like_variable", "myKebab-like-variable".to_case(Case::Snake));
//! ```
//!
//! # Random Feature
//!
//! To ensure this library had zero dependencies, randomness was moved to the _random_ feature,
//! which requires the `rand` crate. You can enable this feature by including the 
//! following in your `Cargo.toml`.
//! ```{toml}
//! [dependencies]
//! convert_case = { version = "^0.3, features = ["random"] }
//! ```
//! This will add two additional cases: Random and PseudoRandom.  You can read about their
//! construction in the [Case enum](enum.Case.html).

mod case;
mod words;
pub use case::Case;
use words::Words;

/// Describes items that can be converted into a case.
///
/// Implemented for string slices `&str` and owned strings `String`.
pub trait Casing {
    /// References `self` and converts to the given case.
    fn to_case(&self, case: Case) -> String;

    /// Creates a `FromCasing` struct, which saves information about
    /// how to parse `self` before converting to a case.
    fn from_case(&self, case: Case) -> FromCasing;
}

impl Casing for str {
    fn to_case(&self, case: Case) -> String {
        Words::new(self).into_case(case)
    }

    fn from_case(&self, case: Case) -> FromCasing {
        FromCasing::new(self.to_string(), case)
    }
}

impl Casing for String {
    fn to_case(&self, case: Case) -> String {
        Words::new(self).into_case(case)
    }

    fn from_case(&self, case: Case) -> FromCasing {
        FromCasing::new(self.to_string(), case)
    }
}

/// Holds information about parsing before converting into a case.
///
/// This struct is used when invoking the `from_case` method on
/// `Casing`.  `FromCasing` also implements `Casing`.
/// ```
/// use convert_case::{Case, Casing};
///
/// let title = "ninety-nine_problems".from_case(Case::Snake).to_case(Case::Title);
/// assert_eq!("Ninety-nine Problems", title);
/// ```
pub struct FromCasing {
    name: String,
    case: Case,
}

impl FromCasing {
    const fn new(name: String, case: Case) -> Self {
        Self { name, case }
    }
}

impl Casing for FromCasing {
    fn to_case(&self, case: Case) -> String {
        Words::from_casing(&self.name, self.case).into_case(case)
    }

    fn from_case(&self, case: Case) -> Self {
        Self::new(self.name.to_string(), case)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use strum::IntoEnumIterator;

    #[test]
    fn lossless_against_lossless() {
        let examples = vec![
            (Case::Lower, "my variable 22 name"),
            (Case::Upper, "MY VARIABLE 22 NAME"),
            (Case::Title, "My Variable 22 Name"),
            (Case::Camel, "myVariable22Name"),
            (Case::Pascal, "MyVariable22Name"),
            (Case::Snake, "my_variable_22_name"),
            (Case::ScreamingSnake, "MY_VARIABLE_22_NAME"),
            (Case::Kebab, "my-variable-22-name"),
            (Case::Cobol, "MY-VARIABLE-22-NAME"),
            (Case::Toggle, "mY vARIABLE 22 nAME"),
            (Case::Train, "My-Variable-22-Name"),
            (Case::Alternating, "mY vArIaBlE 22 nAmE"),
        ];

        for (case_a, str_a) in examples.iter() {
            for (case_b, str_b) in examples.iter() {
                assert_eq!(*str_a, str_b.from_case(*case_b).to_case(*case_a))
            }
        }
    }

    #[test]
    fn obvious_default_parsing() {
        let examples = vec![
            "SuperMario64Game",
            "super-mario64-game",
            "superMario64 game",
            "Super Mario 64_game",
            "SUPERMario 64-game",
            "super_mario-64 game",
        ];

        for example in examples {
            assert_eq!("super_mario_64_game", example.to_case(Case::Snake));
        }
    }

    #[test]
    fn multiline_strings() {
        assert_eq!(
            "One\ntwo\nthree",
            "one\ntwo\nthree".to_case(Case::Title)
        );
    }

    #[test]
    fn camel_case_acroynms() {
        assert_eq!(
            "xml_http_request",
            "XMLHttpRequest".from_case(Case::Camel).to_case(Case::Snake)
        );
        assert_eq!(
            "xml_http_request",
            "XMLHttpRequest"
                .from_case(Case::UpperCamel)
                .to_case(Case::Snake)
        );
        assert_eq!(
            "xml_http_request",
            "XMLHttpRequest"
                .from_case(Case::Pascal)
                .to_case(Case::Snake)
        );
    }

    #[test]
    fn leading_tailing_delimeters() {
        assert_eq!(
            "leading_underscore",
            "_leading_underscore"
                .from_case(Case::Snake)
                .to_case(Case::Snake)
        );
        assert_eq!(
            "tailing_underscore",
            "tailing_underscore_"
                .from_case(Case::Snake)
                .to_case(Case::Snake)
        );
        assert_eq!(
            "leading_hyphen",
            "-leading-hyphen"
                .from_case(Case::Kebab)
                .to_case(Case::Snake)
        );
        assert_eq!(
            "tailing_hyphen",
            "tailing-hyphen-"
                .from_case(Case::Kebab)
                .to_case(Case::Snake)
        );
    }

    #[test]
    fn double_delimeters() {
        assert_eq!(
            "many_underscores",
            "many___underscores"
                .from_case(Case::Snake)
                .to_case(Case::Snake)
        );
        assert_eq!(
            "many-underscores",
            "many---underscores"
                .from_case(Case::Kebab)
                .to_case(Case::Kebab)
        );
    }

    #[test]
    fn early_word_boundaries() {
        assert_eq!(
            "a_bagel",
            "aBagel".from_case(Case::Camel).to_case(Case::Snake)
        );
    }

    #[test]
    fn late_word_boundaries() {
        assert_eq!(
            "team_a",
            "teamA".from_case(Case::Camel).to_case(Case::Snake)
        );
    }

    #[test]
    fn empty_string() {
        for (case_a, case_b) in Case::iter().zip(Case::iter()) {
            assert_eq!("", "".from_case(case_a).to_case(case_b));
        }
    }

    #[test]
    fn owned_string() {
        assert_eq!(
            "test_variable",
            String::from("TestVariable").to_case(Case::Snake)
        )
    }

    #[test]
    fn default_all_boundaries() {
        assert_eq!(
            "abc_abc_abc_abc_abc_abc",
            "ABC-abc_abcAbc ABCAbc".to_case(Case::Snake)
        );
    }

    #[test]
    fn alternating_ignore_symbols() {
        assert_eq!("tHaT's", "that's".to_case(Case::Alternating));
    }
}
