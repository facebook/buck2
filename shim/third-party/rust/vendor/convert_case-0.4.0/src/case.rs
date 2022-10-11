#[cfg(test)]
use strum_macros::EnumIter;

/// Defines the type of casing a string can be.
///
/// ```
/// use convert_case::{Case, Casing};
///
/// let super_mario_title: String = "super_mario_64".to_case(Case::Title);
/// assert_eq!("Super Mario 64", super_mario_title);
/// ```
#[cfg_attr(test, derive(EnumIter))]
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum Case {

    /// Uppercase strings are delimited by spaces and all characters are uppercase.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("MY VARIABLE NAME", "My variable NAME".to_case(Case::Upper))
    /// ```
    Upper,

    /// Lowercase strings are delimited by spaces and all characters are lowercase.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("my variable name", "My variable NAME".to_case(Case::Lower))
    /// ```
    Lower,

    /// Title case strings are delimited by spaces. Only the leading character of
    /// each word is uppercase.  No inferences are made about language, so words
    /// like "as", "to", and "for" will still be capitalized.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("My Variable Name", "My variable NAME".to_case(Case::Title))
    /// ```
    Title,

    /// Toggle case strings are delimited by spaces.  All characters are uppercase except
    /// for the leading character of each word, which is lowercase.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("mY vARIABLE nAME", "My variable NAME".to_case(Case::Toggle))
    /// ```
    Toggle,

    /// Camel case strings are lowercase, but for every word _except the first_ the
    /// first letter is capitalized.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("myVariableName", "My variable NAME".to_case(Case::Camel))
    /// ```
    Camel,

    /// Pascal case strings are lowercase, but for every word the
    /// first letter is capitalized.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("MyVariableName", "My variable NAME".to_case(Case::Pascal))
    /// ```
    Pascal,

    /// Upper camel case is an alternative name for Pascal case.
    UpperCamel,

    /// Snake case strings are delimited by underscores `_` and are all lowercase.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("my_variable_name", "My variable NAME".to_case(Case::Snake))
    /// ```
    Snake,

    /// Upper snake case strings are delimited by underscores `_` and are all uppercase.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("MY_VARIABLE_NAME", "My variable NAME".to_case(Case::UpperSnake))
    /// ```
    UpperSnake,
    
    /// Screaming snake case is an alternative name for upper snake case.
    ScreamingSnake,

    /// Kebab case strings are delimited by hyphens `-` and are all lowercase.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("my-variable-name", "My variable NAME".to_case(Case::Kebab))
    /// ```
    Kebab,

    /// Cobol case strings are delimited by hyphens `-` and are all uppercase.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("MY-VARIABLE-NAME", "My variable NAME".to_case(Case::Cobol))
    /// ```
    Cobol,

    /// Train case strings are delimited by hyphens `-`.  All characters are lowercase
    /// except for the leading character of each word.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("My-Variable-Name", "My variable NAME".to_case(Case::Train))
    /// ```
    Train,

    /// Flat case strings are all lowercase, with no delimiter.  Converting to
    /// this case is **lossy**.  That is, word boundaries are lost.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("myvariablename", "My variable NAME".to_case(Case::Flat))
    /// ```
    Flat,

    /// Upper flat case strings are all uppercase, with no delimiter.  Converting to
    /// this case is **lossy**.  That is, word boundaries are lost.
    ///
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("MYVARIABLENAME", "My variable NAME".to_case(Case::UpperFlat))
    /// ```
    UpperFlat,

    /// Alternating case strings are delimited by spaces.  Characters alternate between uppercase
    /// and lowercase.
    /// ```
    /// use convert_case::{Case, Casing};
    /// assert_eq!("mY vArIaBlE nAmE", "My variable NAME".to_case(Case::Alternating));
    /// ```
    Alternating,

    /// Random case strings are delimited by spaces and characters are
    /// randomly upper case or lower case.  This uses the `rand` crate 
    /// and is only available with the "random" feature.
    /// ```
    /// use convert_case::{Case, Casing};
    /// let new = "My variable NAME".to_case(Case::Random);
    /// ```
    /// `new` could be "My vaRIAbLE nAme" for example.
    #[cfg(feature = "random")]
    Random,

    /// Pseudo-random case strings are delimited by spaces and characters are randomly
    /// upper case or lower case, but there will never more than two consecutive lower
    /// case or upper case letters in a row.  This uses the `rand` crate and is
    /// only available with the "random" feature.
    /// ```
    /// use convert_case::{Case, Casing};
    /// let new = "My variable NAME".to_case(Case::Random);
    /// ```
    /// `new` could be "mY vArIAblE NamE" for example.
    #[cfg(feature = "random")]
    PseudoRandom,
}

impl Case {
    // Created to avoid using the EnumIter trait from strum in
    // final library.  A test confirms that all cases are listed here.
    /// Returns a vector with all case enum variants.  This was
    /// created for use in the `ccase` binary.
    pub fn all_cases() -> Vec<Case> {
        use Case::*;
        vec![
            Upper,
            Lower,
            Title,
            Toggle,
            Camel,
            Pascal,
            UpperCamel,
            Snake,
            UpperSnake,
            ScreamingSnake,
            Kebab,
            Cobol,
            Train,
            Flat,
            UpperFlat,
            Alternating,

            #[cfg(feature = "random")]
            Random,
            #[cfg(feature = "random")]
            PseudoRandom,
        ]
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use strum::IntoEnumIterator;

    #[test]
    fn all_cases_in_iter() {
        let all = Case::all_cases();
        for case in Case::iter() {
            assert!(all.contains(&case));
        }
    }
}
