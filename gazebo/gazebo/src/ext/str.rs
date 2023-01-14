/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(feature = "str_pattern_extensions")]
use std::str::pattern::*;

/// Extension traits on [`str`](str).
///
/// Set the configuration option `str_pattern_extensions` to enable the associated methods.
/// The setting `str_pattern_extensions` requires the unstable features
/// `pattern` and `associated_type_bounds`, so only works with Rust nightly.
pub trait StrExt {
    /// Like `split`, but only separates off the first element. For example:
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// assert_eq!("test".split1('e'), ("t", "st"));
    /// assert_eq!("test".split1('t'), ("", "est"));
    /// assert_eq!("test".split1('x'), ("test", ""));
    /// assert_eq!("".split1('e'), ("", ""));
    /// ```
    #[cfg(feature = "str_pattern_extensions")]
    fn split1<'a, P>(&'a self, pat: P) -> (&'a Self, &'a Self)
    where
        P: Pattern<'a>;

    /// Like `split`, but only separates off the first element if there is a
    /// separator, otherwise returns `None`. For example:
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// assert_eq!("test".split1_opt('e'), Some(("t", "st")));
    /// assert_eq!("test".split1_opt('t'), Some(("", "est")));
    /// assert_eq!("test".split1_opt('x'), None);
    /// assert_eq!("test!".split1_opt('!'), Some(("test", "")));
    /// assert_eq!("test".split1_opt("es"), Some(("t", "t")));
    /// assert_eq!("".split1_opt('e'), None);
    /// ```
    ///
    /// In most cases you should use `split_once`, which is now available in `std`.
    #[cfg(feature = "str_pattern_extensions")]
    #[deprecated(since = "0.6.1", note = "use `split_once` available in `std`")]
    fn split1_opt<'a, P>(&'a self, pat: P) -> Option<(&'a Self, &'a Self)>
    where
        P: Pattern<'a>;

    /// Trim off the first match, or return the string unchanged if the pattern
    /// is not a prefix of the string. Like 'trim_start_matches', but at
    /// most one trim.
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// assert_eq!("test".trim_start_match("tes"), "t");
    /// assert_eq!("test".trim_start_match("x"), "test");
    /// assert_eq!("tttest".trim_start_match("t"), "ttest");
    /// ```
    #[cfg(feature = "str_pattern_extensions")]
    fn trim_start_match<'a, P>(&'a self, pat: P) -> &'a Self
    where
        P: Pattern<'a>;

    /// Trim off the first match and return 'Some', or return 'None' if the
    /// pattern is not a prefix of the string. Like 'trim_start_matches'.
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// assert_eq!("test".trim_start_match_opt("tes"), Some("t"));
    /// assert_eq!("test".trim_start_match_opt("x"), None);
    /// assert_eq!("tttest".trim_start_match_opt("t"), Some("ttest"));
    /// assert_eq!("est".trim_start_match_opt("t"), None);
    /// ```
    #[cfg(feature = "str_pattern_extensions")]
    #[deprecated(note = "Use str.strip_prefix instead")]
    fn trim_start_match_opt<'a, P>(&'a self, pat: P) -> Option<&'a Self>
    where
        P: Pattern<'a>;

    /// Trim off the first match, or return the string unchanged if the pattern
    /// is not a prefix of the string. Like 'trim_start_matches', but at
    /// most one trim.
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// assert_eq!("test".trim_end_match("est"), "t");
    /// assert_eq!("test".trim_end_match("x"), "test");
    /// assert_eq!("testtt".trim_end_match("t"), "testt");
    /// ```
    #[cfg(feature = "str_pattern_extensions")]
    fn trim_end_match<'a, P>(&'a self, pat: P) -> &'a Self
    where
        P: Pattern<'a, Searcher: ReverseSearcher<'a>>;

    /// Trim off the first match and return 'Some', or return 'None' if the
    /// pattern is not a prefix of the string. Like 'trim_start_matches'.
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// assert_eq!("test".trim_end_match_opt("est"), Some("t"));
    /// assert_eq!("test".trim_end_match_opt("x"), None);
    /// assert_eq!("testtt".trim_end_match_opt("t"), Some("testt"));
    /// assert_eq!("tes".trim_end_match_opt("t"), None);
    /// ```
    #[cfg(feature = "str_pattern_extensions")]
    #[deprecated(note = "Use str.strip_suffix instead")]
    fn trim_end_match_opt<'a, P>(&'a self, pat: P) -> Option<&'a Self>
    where
        P: Pattern<'a, Searcher: ReverseSearcher<'a>>;
}

impl StrExt for str {
    #[cfg(feature = "str_pattern_extensions")]
    fn split1_opt<'a, P>(&'a self, pat: P) -> Option<(&'a Self, &'a Self)>
    where
        P: Pattern<'a>,
    {
        self.split_once(pat)
    }

    #[cfg(feature = "str_pattern_extensions")]
    fn split1<'a, P>(&'a self, pat: P) -> (&'a Self, &'a Self)
    where
        P: Pattern<'a>,
    {
        self.split_once(pat).unwrap_or((self, ""))
    }

    #[cfg(feature = "str_pattern_extensions")]
    fn trim_start_match_opt<'a, P>(&'a self, pat: P) -> Option<&'a Self>
    where
        P: Pattern<'a>,
    {
        let mut matcher = pat.into_searcher(self);
        match matcher.next() {
            SearchStep::Match(0, n) => Some(&self[n..]),
            _ => None,
        }
    }

    #[cfg(feature = "str_pattern_extensions")]
    fn trim_start_match<'a, P>(&'a self, pat: P) -> &'a Self
    where
        P: Pattern<'a>,
    {
        #[allow(deprecated)]
        self.trim_start_match_opt(pat).unwrap_or(self)
    }

    #[cfg(feature = "str_pattern_extensions")]
    fn trim_end_match_opt<'a, P>(&'a self, pat: P) -> Option<&'a Self>
    where
        P: Pattern<'a, Searcher: ReverseSearcher<'a>>,
    {
        let mut matcher = pat.into_searcher(self);
        match matcher.next_back() {
            SearchStep::Match(n, _) => Some(&self[0..n]),
            _ => None,
        }
    }

    #[cfg(feature = "str_pattern_extensions")]
    fn trim_end_match<'a, P>(&'a self, pat: P) -> &'a Self
    where
        P: Pattern<'a, Searcher: ReverseSearcher<'a>>,
    {
        #[allow(deprecated)]
        self.trim_end_match_opt(pat).unwrap_or(self)
    }
}
