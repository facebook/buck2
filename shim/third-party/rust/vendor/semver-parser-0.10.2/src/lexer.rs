//! Lexer for semver ranges.
//!
//! Breaks a string of input into an iterator of tokens that can be used with a parser.
//!
//! This should be used with the [`parser`] module.
//!
//! [`parser`]: ../parser/index.html
//!
//! # Examples
//!
//! Example without errors:
//!
//! ```rust
//! use semver_parser::lexer::{Lexer, Token};
//!
//! let mut l = Lexer::new("foo 123 *");
//!
//! assert_eq!(Some(Ok(Token::AlphaNumeric("foo"))), l.next());
//! assert_eq!(Some(Ok(Token::Whitespace(3, 4))), l.next());
//! assert_eq!(Some(Ok(Token::Numeric(123))), l.next());
//! assert_eq!(Some(Ok(Token::Whitespace(7, 8))), l.next());
//! assert_eq!(Some(Ok(Token::Star)), l.next());
//! assert_eq!(None, l.next());
//! ```
//!
//! Example with error:
//!
//! ```rust
//! use semver_parser::lexer::{Lexer, Token, Error};
//!
//! let mut l = Lexer::new("foo / *");
//!
//! assert_eq!(Some(Ok(Token::AlphaNumeric("foo"))), l.next());
//! assert_eq!(Some(Ok(Token::Whitespace(3, 4))), l.next());
//! assert_eq!(Some(Err(Error::UnexpectedChar('/'))), l.next());
//! ```

use self::Error::*;
use self::Token::*;
use std::str;

macro_rules! scan_while {
    ($slf:expr, $start:expr, $first:pat $(| $rest:pat)*) => {{
        let mut __end = $start;

        loop {
            if let Some((idx, c)) = $slf.one() {
                __end = idx;

                match c {
                    $first $(| $rest)* => $slf.step(),
                    _ => break,
                }

                continue;
            } else {
                __end = $slf.input.len();
            }

            break;
        }

        __end
    }}
}

/// Semver tokens.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token<'input> {
    /// `=`
    Eq,
    /// `>`
    Gt,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `>=`
    GtEq,
    /// '^`
    Caret,
    /// '~`
    Tilde,
    /// '*`
    Star,
    /// `.`
    Dot,
    /// `,`
    Comma,
    /// `-`
    Hyphen,
    /// `+`
    Plus,
    /// '||'
    Or,
    /// any number of whitespace (`\t\r\n `) and its span.
    Whitespace(usize, usize),
    /// Numeric component, like `0` or `42`.
    Numeric(u64),
    /// Alphanumeric component, like `alpha1` or `79deadbe`.
    AlphaNumeric(&'input str),
}

impl<'input> Token<'input> {
    /// Check if the current token is a whitespace token.
    pub fn is_whitespace(&self) -> bool {
        match *self {
            Whitespace(..) => true,
            _ => false,
        }
    }

    /// Check if the current token is a wildcard token.
    pub fn is_wildcard(&self) -> bool {
        match *self {
            Star | AlphaNumeric("X") | AlphaNumeric("x") => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Error {
    /// Unexpected character.
    UnexpectedChar(char),
}

/// Lexer for semver tokens belonging to a range.
#[derive(Debug)]
pub struct Lexer<'input> {
    input: &'input str,
    chars: str::CharIndices<'input>,
    // lookahead
    c1: Option<(usize, char)>,
    c2: Option<(usize, char)>,
}

impl<'input> Lexer<'input> {
    /// Construct a new lexer for the given input.
    pub fn new(input: &str) -> Lexer {
        let mut chars = input.char_indices();
        let c1 = chars.next();
        let c2 = chars.next();

        Lexer {
            input,
            chars,
            c1,
            c2,
        }
    }

    /// Shift all lookahead storage by one.
    fn step(&mut self) {
        self.c1 = self.c2;
        self.c2 = self.chars.next();
    }

    fn step_n(&mut self, n: usize) {
        for _ in 0..n {
            self.step();
        }
    }

    /// Access the one character, or set it if it is not set.
    fn one(&mut self) -> Option<(usize, char)> {
        self.c1
    }

    /// Access two characters.
    fn two(&mut self) -> Option<(usize, char, char)> {
        self.c1
            .and_then(|(start, c1)| self.c2.map(|(_, c2)| (start, c1, c2)))
    }

    /// Consume a component.
    ///
    /// A component can either be an alphanumeric or numeric.
    /// Does not permit leading zeroes if numeric.
    fn component(&mut self, start: usize) -> Result<Token<'input>, Error> {
        let end = scan_while!(self, start, '0'..='9' | 'A'..='Z' | 'a'..='z');
        let input = &self.input[start..end];

        let mut it = input.chars();
        let (a, b) = (it.next(), it.next());

        // exactly zero
        if a == Some('0') && b.is_none() {
            return Ok(Numeric(0));
        }

        if a != Some('0') {
            if let Ok(numeric) = input.parse::<u64>() {
                return Ok(Numeric(numeric));
            }
        }

        Ok(AlphaNumeric(input))
    }

    /// Consume whitespace.
    fn whitespace(&mut self, start: usize) -> Result<Token<'input>, Error> {
        let end = scan_while!(self, start, ' ' | '\t' | '\n' | '\r');
        Ok(Whitespace(start, end))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Token<'input>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        #[allow(clippy::never_loop)]
        loop {
            // two subsequent char tokens.
            if let Some((_, a, b)) = self.two() {
                let two = match (a, b) {
                    ('<', '=') => Some(LtEq),
                    ('>', '=') => Some(GtEq),
                    ('|', '|') => Some(Or),
                    _ => None,
                };

                if let Some(two) = two {
                    self.step_n(2);
                    return Some(Ok(two));
                }
            }

            // single char and start of numeric tokens.
            if let Some((start, c)) = self.one() {
                let tok = match c {
                    ' ' | '\t' | '\n' | '\r' => {
                        self.step();
                        return Some(self.whitespace(start));
                    }
                    '=' => Eq,
                    '>' => Gt,
                    '<' => Lt,
                    '^' => Caret,
                    '~' => Tilde,
                    '*' => Star,
                    '.' => Dot,
                    ',' => Comma,
                    '-' => Hyphen,
                    '+' => Plus,
                    '0'..='9' | 'a'..='z' | 'A'..='Z' => {
                        self.step();
                        return Some(self.component(start));
                    }
                    c => return Some(Err(UnexpectedChar(c))),
                };

                self.step();
                return Some(Ok(tok));
            };

            return None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Vec<Token> {
        Lexer::new(input).map(Result::unwrap).collect::<Vec<_>>()
    }

    #[test]
    pub fn simple_tokens() {
        assert_eq!(
            lex("=><<=>=^~*.,-+||"),
            vec![Eq, Gt, Lt, LtEq, GtEq, Caret, Tilde, Star, Dot, Comma, Hyphen, Plus, Or,]
        );
    }

    #[test]
    pub fn whitespace() {
        assert_eq!(
            lex("  foo \t\n\rbar"),
            vec![
                Whitespace(0, 2),
                AlphaNumeric("foo"),
                Whitespace(5, 9),
                AlphaNumeric("bar"),
            ]
        );
    }

    #[test]
    pub fn components() {
        assert_eq!(lex("42"), vec![Numeric(42)]);
        assert_eq!(lex("0"), vec![Numeric(0)]);
        assert_eq!(lex("01"), vec![AlphaNumeric("01")]);
        assert_eq!(lex("01"), vec![AlphaNumeric("01")]);
        assert_eq!(lex("5885644aa"), vec![AlphaNumeric("5885644aa")]);
        assert_eq!(lex("beta2"), vec![AlphaNumeric("beta2")]);
        assert_eq!(lex("beta.2"), vec![AlphaNumeric("beta"), Dot, Numeric(2)]);
    }

    #[test]
    pub fn is_wildcard() {
        assert_eq!(Star.is_wildcard(), true);
        assert_eq!(AlphaNumeric("x").is_wildcard(), true);
        assert_eq!(AlphaNumeric("X").is_wildcard(), true);
        assert_eq!(AlphaNumeric("other").is_wildcard(), false);
    }

    #[test]
    pub fn empty() {
        assert_eq!(lex(""), vec![]);
    }

    #[test]
    pub fn numeric_all_numbers() {
        let expected: Vec<Token> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
            .into_iter()
            .map(Numeric)
            .collect::<Vec<_>>();

        let actual: Vec<_> = lex("0 1 2 3 4 5 6 7 8 9")
            .into_iter()
            .filter(|t| !t.is_whitespace())
            .collect();

        assert_eq!(actual, expected);
    }
}
