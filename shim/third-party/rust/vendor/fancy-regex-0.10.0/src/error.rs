use std::fmt;

/// Result type for this crate with specific error enum.
pub type Result<T> = ::std::result::Result<T, Error>;

/// An error for the result of compiling or running a regex.
#[derive(Debug)]
pub enum Error {
    // Compile time errors
    /// General parsing error
    ParseError,
    /// Opening parenthesis without closing parenthesis, e.g. `(a|b`
    UnclosedOpenParen,
    /// Invalid repeat syntax
    InvalidRepeat,
    /// Pattern too deeply nested
    RecursionExceeded,
    /// Look-behind assertion without constant size
    LookBehindNotConst,
    /// Backslash without following character
    TrailingBackslash,
    /// Invalid escape
    InvalidEscape(String),
    /// Unicode escape not closed
    UnclosedUnicodeName,
    /// Invalid hex escape
    InvalidHex,
    /// Invalid codepoint for hex or unicode escape
    InvalidCodepointValue,
    /// Invalid character class
    InvalidClass,
    /// Unknown group flag
    UnknownFlag(String),
    /// Disabling Unicode not supported
    NonUnicodeUnsupported,
    /// Invalid back reference
    InvalidBackref,
    /// Regex crate error
    InnerError(regex::Error),
    /// Couldn't parse group name
    InvalidGroupName,
    /// Invalid group id in escape sequence
    InvalidGroupNameBackref(String),
    /// Once named groups are used you cannot refer to groups by number
    NamedBackrefOnly,

    /// Quantifier on lookaround or other zero-width assertion
    TargetNotRepeatable,

    // Run time errors
    /// Max stack size exceeded for backtracking while executing regex.
    StackOverflow,
    /// Max limit for backtracking count exceeded while executing the regex.
    /// Configure using
    /// [`RegexBuilder::backtrack_limit`](struct.RegexBuilder.html#method.backtrack_limit).
    BacktrackLimitExceeded,

    /// This enum may grow additional variants, so this makes sure clients don't count on exhaustive
    /// matching. Otherwise, adding a new variant could break existing code.
    #[doc(hidden)]
    __Nonexhaustive,
}

impl ::std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // We should make these more helpful, e.g. by including the parts of the regex that lead to
        // the error.
        match self {
            Error::ParseError => write!(f, "General parsing error"),
            Error::UnclosedOpenParen => {
                write!(f, "Opening parenthesis without closing parenthesis")
            }
            Error::InvalidRepeat => write!(f, "Invalid repeat syntax"),
            Error::RecursionExceeded => write!(f, "Pattern too deeply nested"),
            Error::LookBehindNotConst => write!(f, "Look-behind assertion without constant size"),
            Error::TrailingBackslash => write!(f, "Backslash without following character"),
            Error::InvalidEscape(s) => write!(f, "Invalid escape: {}", s),
            Error::UnclosedUnicodeName => write!(f, "Unicode escape not closed"),
            Error::InvalidHex => write!(f, "Invalid hex escape"),
            Error::InvalidCodepointValue => {
                write!(f, "Invalid codepoint for hex or unicode escape")
            }
            Error::InvalidClass => write!(f, "Invalid character class"),
            Error::UnknownFlag(s) => write!(f, "Unknown group flag: {}", s),
            Error::NonUnicodeUnsupported => write!(f, "Disabling Unicode not supported"),
            Error::InvalidBackref => write!(f, "Invalid back reference"),
            Error::InnerError(e) => write!(f, "Regex error: {}", e),
            Error::StackOverflow => write!(f, "Max stack size exceeded for backtracking"),
            Error::BacktrackLimitExceeded => write!(f, "Max limit for backtracking count exceeded"),
            Error::__Nonexhaustive => unreachable!(),
            Error::InvalidGroupName => write!(f, "Could not parse group name"),
            Error::InvalidGroupNameBackref(s) => write!(f, "Invalid group name in back reference: {}", s),
            Error::TargetNotRepeatable => write!(f, "Target of repeat operator is invalid"),
            Error::NamedBackrefOnly => write!(f, "Numbered backref/call not allowed because named group was used, use a named backref instead"),
        }
    }
}
