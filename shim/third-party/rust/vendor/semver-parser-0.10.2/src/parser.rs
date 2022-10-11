// this is only for parsing versions now

use std::fmt;
use std::mem;

use self::Error::*;
use crate::lexer::{self, Lexer, Token};
use crate::version::{Identifier, Version};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Error<'input> {
    /// Needed more tokens for parsing, but none are available.
    UnexpectedEnd,
    /// Unexpected token.
    UnexpectedToken(Token<'input>),
    /// An error occurred in the lexer.
    Lexer(lexer::Error),
    /// More input available.
    MoreInput(Vec<Token<'input>>),
    /// Encountered empty predicate in a set of predicates.
    EmptyPredicate,
    /// Encountered an empty range.
    EmptyRange,
}

impl<'input> From<lexer::Error> for Error<'input> {
    fn from(value: lexer::Error) -> Self {
        Error::Lexer(value)
    }
}

impl<'input> fmt::Display for Error<'input> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;

        match *self {
            UnexpectedEnd => write!(fmt, "expected more input"),
            UnexpectedToken(ref token) => write!(fmt, "encountered unexpected token: {:?}", token),
            Lexer(ref error) => write!(fmt, "lexer error: {:?}", error),
            MoreInput(ref tokens) => write!(fmt, "expected end of input, but got: {:?}", tokens),
            EmptyPredicate => write!(fmt, "encountered empty predicate"),
            EmptyRange => write!(fmt, "encountered empty range"),
        }
    }
}

/// impl for backwards compatibility.
impl<'input> From<Error<'input>> for String {
    fn from(value: Error<'input>) -> Self {
        value.to_string()
    }
}

/// A recursive-descent parser for parsing version requirements.
pub struct Parser<'input> {
    /// Source of token.
    lexer: Lexer<'input>,
    /// Lookaehead.
    c1: Option<Token<'input>>,
}

impl<'input> Parser<'input> {
    /// Construct a new parser for the given input.
    pub fn new(input: &'input str) -> Result<Parser<'input>, Error<'input>> {
        let mut lexer = Lexer::new(input);

        let c1 = if let Some(c1) = lexer.next() {
            Some(c1?)
        } else {
            None
        };

        Ok(Parser { lexer, c1 })
    }

    /// Pop one token.
    #[inline(always)]
    fn pop(&mut self) -> Result<Token<'input>, Error<'input>> {
        let c1 = if let Some(c1) = self.lexer.next() {
            Some(c1?)
        } else {
            None
        };

        mem::replace(&mut self.c1, c1).ok_or_else(|| UnexpectedEnd)
    }

    /// Peek one token.
    #[inline(always)]
    fn peek(&mut self) -> Option<&Token<'input>> {
        self.c1.as_ref()
    }

    /// Skip whitespace if present.
    fn skip_whitespace(&mut self) -> Result<(), Error<'input>> {
        match self.peek() {
            Some(&Token::Whitespace(_, _)) => self.pop().map(|_| ()),
            _ => Ok(()),
        }
    }

    /// Parse a single component.
    ///
    /// Returns `None` if the component is a wildcard.
    pub fn component(&mut self) -> Result<Option<u64>, Error<'input>> {
        match self.pop()? {
            Token::Numeric(number) => Ok(Some(number)),
            ref t if t.is_wildcard() => Ok(None),
            tok => Err(UnexpectedToken(tok)),
        }
    }

    /// Parse a single numeric.
    pub fn numeric(&mut self) -> Result<u64, Error<'input>> {
        match self.pop()? {
            Token::Numeric(number) => Ok(number),
            tok => Err(UnexpectedToken(tok)),
        }
    }

    /// Optionally parse a dot, then a component.
    ///
    /// The second component of the tuple indicates if a wildcard has been encountered, and is
    /// always `false` if the first component is `Some`.
    ///
    /// If a dot is not encountered, `(None, false)` is returned.
    ///
    /// If a wildcard is encountered, `(None, true)` is returned.
    pub fn dot_component(&mut self) -> Result<(Option<u64>, bool), Error<'input>> {
        match self.peek() {
            Some(&Token::Dot) => {}
            _ => return Ok((None, false)),
        }

        // pop the peeked dot.
        self.pop()?;
        self.component().map(|n| (n, n.is_none()))
    }

    /// Parse a dot, then a numeric.
    pub fn dot_numeric(&mut self) -> Result<u64, Error<'input>> {
        match self.pop()? {
            Token::Dot => {}
            tok => return Err(UnexpectedToken(tok)),
        }

        self.numeric()
    }

    /// Parse an string identifier.
    ///
    /// Like, `foo`, or `bar`, or `beta-1`.
    pub fn identifier(&mut self) -> Result<Identifier, Error<'input>> {
        let identifier = match self.pop()? {
            Token::AlphaNumeric(identifier) => {
                // TODO: Borrow?
                Identifier::AlphaNumeric(identifier.to_string())
            }
            Token::Numeric(n) => Identifier::Numeric(n),
            tok => return Err(UnexpectedToken(tok)),
        };

        if let Some(&Token::Hyphen) = self.peek() {
            // pop the peeked hyphen
            self.pop()?;
            // concat with any following identifiers
            Ok(identifier
                .concat("-")
                .concat(&self.identifier()?.to_string()))
        } else {
            Ok(identifier)
        }
    }

    /// Parse all pre-release identifiers, separated by dots.
    ///
    /// Like, `abcdef.1234`.
    fn pre(&mut self) -> Result<Vec<Identifier>, Error<'input>> {
        match self.peek() {
            Some(&Token::Hyphen) => {}
            _ => return Ok(vec![]),
        }

        // pop the peeked hyphen.
        self.pop()?;
        self.parts()
    }

    /// Parse a dot-separated set of identifiers.
    fn parts(&mut self) -> Result<Vec<Identifier>, Error<'input>> {
        let mut parts = Vec::new();

        parts.push(self.identifier()?);

        while let Some(&Token::Dot) = self.peek() {
            self.pop()?;

            parts.push(self.identifier()?);
        }

        Ok(parts)
    }

    /// Parse optional build metadata.
    ///
    /// Like, `` (empty), or `+abcdef`.
    fn plus_build_metadata(&mut self) -> Result<Vec<Identifier>, Error<'input>> {
        match self.peek() {
            Some(&Token::Plus) => {}
            _ => return Ok(vec![]),
        }

        // pop the plus.
        self.pop()?;
        self.parts()
    }

    /// Parse a version.
    ///
    /// Like, `1.0.0` or `3.0.0-beta.1`.
    pub fn version(&mut self) -> Result<Version, Error<'input>> {
        self.skip_whitespace()?;

        let major = self.numeric()?;
        let minor = self.dot_numeric()?;
        let patch = self.dot_numeric()?;
        let pre = self.pre()?;
        let build = self.plus_build_metadata()?;

        self.skip_whitespace()?;

        Ok(Version {
            major,
            minor,
            patch,
            pre,
            build,
        })
    }

    /// Check if we have reached the end of input.
    pub fn is_eof(&mut self) -> bool {
        self.c1.is_none()
    }

    /// Get the rest of the tokens in the parser.
    ///
    /// Useful for debugging.
    pub fn tail(&mut self) -> Result<Vec<Token<'input>>, Error<'input>> {
        let mut out = Vec::new();

        if let Some(t) = self.c1.take() {
            out.push(t);
        }

        while let Some(t) = self.lexer.next() {
            out.push(t?);
        }

        Ok(out)
    }
}
