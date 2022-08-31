/*
 * Copyright 2018 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::char;
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Display;

use derive_more::Display;
use logos::Logos;
use num_bigint::BigInt;
use num_traits::Num;
use thiserror::Error;

use crate::codemap::CodeMap;
use crate::codemap::Pos;
use crate::codemap::Span;
use crate::errors::Diagnostic;
use crate::syntax::cursors::CursorBytes;
use crate::syntax::cursors::CursorChars;
use crate::syntax::dialect::Dialect;

#[derive(Error, Debug)]
pub(crate) enum LexemeError {
    #[error("Parse error: incorrect indentation")]
    Indentation,
    #[error("Parse error: invalid input `{0}`")]
    InvalidInput(String),
    #[error("Parse error: tabs are not allowed in the dialect")]
    InvalidTab,
    #[error("Parse error: unfinished string literal")]
    UnfinishedStringLiteral,
    #[error("Parse error: invalid string escape sequence `{0}`")]
    InvalidEscapeSequence(String),
    #[error("Parse error: cannot use reserved keyword `{0}`")]
    ReservedKeyword(String),
    #[error("Parse error: integer cannot have leading 0, got `{0}`")]
    StartsZero(String),
    #[error("Parse error: failed to parse integer: `{0}`")]
    IntParse(String),
}

type Lexeme = anyhow::Result<(usize, Token, usize)>;

pub(crate) struct Lexer<'a> {
    // Information for spans
    codemap: CodeMap,
    // Other info
    indent_levels: Vec<usize>,
    /// Lexemes that have been generated but not yet returned
    buffer: VecDeque<Lexeme>,
    parens: isize, // Number of parens we have seen
    lexer: logos::Lexer<'a, Token>,
    done: bool,
    dialect_allow_tabs: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, dialect: &Dialect, codemap: CodeMap) -> Self {
        let lexer = Token::lexer(input);
        let mut lexer2 = Self {
            codemap,
            // Aim to size all the buffers such that they never resize
            indent_levels: Vec::with_capacity(20),
            buffer: VecDeque::with_capacity(10),
            lexer,
            parens: 0,
            done: false,
            dialect_allow_tabs: dialect.enable_tabs,
        };
        if let Err(e) = lexer2.calculate_indent() {
            lexer2.buffer.push_back(Err(e));
        }
        lexer2
    }

    fn err_pos<T>(&self, msg: LexemeError, pos: usize) -> anyhow::Result<T> {
        self.err_span(msg, pos, pos)
    }

    fn err_span<T>(&self, msg: LexemeError, start: usize, end: usize) -> anyhow::Result<T> {
        Err(Diagnostic::new(
            msg,
            Span::new(Pos::new(start as u32), Pos::new(end as u32)),
            &self.codemap,
        ))
    }

    fn err_now<T>(&self, msg: fn(String) -> LexemeError) -> anyhow::Result<T> {
        self.err_span(
            msg(self.lexer.slice().to_owned()),
            self.lexer.span().start,
            self.lexer.span().end,
        )
    }

    /// We have just seen a newline, read how many indents we have
    /// and then set self.indent properly
    fn calculate_indent(&mut self) -> anyhow::Result<()> {
        // consume tabs and spaces, output the indentation levels
        let mut it = CursorBytes::new(self.lexer.remainder());
        let mut spaces = 0;
        let mut tabs = 0;
        let mut indent_start = self.lexer.span().end;
        loop {
            match it.next_char() {
                None => {
                    self.lexer.bump(it.pos());
                    return Ok(());
                }
                Some(' ') => {
                    spaces += 1;
                }
                Some('\t') => {
                    tabs += 1;
                }
                Some('\n') => {
                    // A line that is entirely blank gets emitted as a newline, and then
                    // we don't consume the subsequent newline character.
                    self.lexer.bump(it.pos() - 1);
                    return Ok(());
                }
                Some('\r') => {
                    // We just ignore these entirely
                }
                Some('#') => {
                    // A line that is all comments doesn't get emitted at all
                    // Skip until the next newline
                    // Remove skip now, so we can freely add it on later
                    spaces = 0;
                    tabs = 0;
                    loop {
                        match it.next_char() {
                            None => {
                                self.lexer.bump(it.pos());
                                return Ok(());
                            }
                            Some('\n') => break, // only the inner loop
                            Some(_) => {}
                        }
                    }
                    indent_start = self.lexer.span().end + it.pos();
                }
                _ => break,
            }
        }
        self.lexer.bump(it.pos() - 1); // last character broke us out the loop
        let indent = spaces + tabs * 8;
        if tabs > 0 && !self.dialect_allow_tabs {
            return self.err_pos(LexemeError::InvalidTab, self.lexer.span().start);
        }
        let now = self.indent_levels.last().copied().unwrap_or(0);

        if indent > now {
            self.indent_levels.push(indent);
            let span = self.lexer.span();
            self.buffer
                .push_back(Ok((indent_start, Token::Indent, span.end)));
        } else if indent < now {
            let mut dedents = 1;
            self.indent_levels.pop().unwrap();
            loop {
                let now = self.indent_levels.last().copied().unwrap_or(0);
                if now == indent {
                    break;
                } else if now > indent {
                    dedents += 1;
                    self.indent_levels.pop().unwrap();
                } else {
                    let pos = self.lexer.span();
                    return self.err_span(LexemeError::Indentation, pos.start, pos.end);
                }
            }
            for _ in 0..dedents {
                // We must declare each dedent is only a position, so multiple adjacent dedents don't overlap
                self.buffer
                    .push_back(Ok((indent_start, Token::Dedent, indent_start)))
            }
        }
        Ok(())
    }

    fn wrap(&mut self, token: Token) -> Option<Lexeme> {
        let span = self.lexer.span();
        Some(Ok((span.start, token, span.end)))
    }

    // We've potentially seen one character, now consume between min and max elements of iterator
    // and treat it as an int in base radix
    fn escape_char(it: &mut CursorChars, min: usize, max: usize, radix: u32) -> Result<char, ()> {
        let mut value = 0u32;
        let mut count = 0;
        while count < max {
            match it.next() {
                None => {
                    if count >= min {
                        break;
                    } else {
                        return Err(());
                    }
                }
                Some(c) => match c.to_digit(radix) {
                    None => {
                        if count >= min {
                            it.unnext(c);
                            break;
                        } else {
                            return Err(());
                        }
                    }
                    Some(v) => {
                        count += 1;
                        value = (value * radix) + v;
                    }
                },
            }
        }
        char::from_u32(value).ok_or(())
    }

    // We have seen a '\' character, now parse what comes next
    fn escape(it: &mut CursorChars, res: &mut String) -> Result<(), ()> {
        match it.next() {
            Some('n') => res.push('\n'),
            Some('r') => res.push('\r'),
            Some('t') => res.push('\t'),
            Some('a') => res.push('\x07'),
            Some('b') => res.push('\x08'),
            Some('f') => res.push('\x0C'),
            Some('v') => res.push('\x0B'),
            Some('\n') => {}
            Some('\r') => {
                // Windows newline incoming, we expect a \n next, which we can ignore
                if it.next() != Some('\n') {
                    // A random \r character happened, let's declare an error, but we're just confused here
                    return Err(());
                }
            }
            Some('x') => res.push(Self::escape_char(it, 2, 2, 16)?),
            Some('u') => res.push(Self::escape_char(it, 4, 4, 16)?),
            Some('U') => res.push(Self::escape_char(it, 8, 8, 16)?),
            Some(c) => match c {
                '0'..='7' => {
                    it.unnext(c);
                    res.push(Self::escape_char(it, 1, 3, 8)?)
                }
                '"' | '\'' | '\\' => res.push(c),
                _ => {
                    res.push('\\');
                    res.push(c);
                }
            },
            None => {
                return Err(());
            }
        };
        Ok(())
    }

    // String parsing is a hot-spot, so parameterise by a `stop` function which gets
    // specialised for each variant
    fn string(&mut self, triple: bool, raw: bool, mut stop: impl FnMut(char) -> bool) -> Lexeme {
        // We have seen an openning quote, which is either ' or "
        // If triple is true, it was a triple quote
        // stop lets us know when a string ends.

        // Before the first quote character
        let string_start = self.lexer.span().start;
        // After the first quote character, but before any contents or it tracked stuff
        let mut string_end = self.lexer.span().end;

        let mut it = CursorBytes::new(self.lexer.remainder());
        let it2;

        if triple {
            it.next();
            it.next();
        }
        let contents_start = it.pos();

        // Take the fast path as long as the result is a slice of the original, with no changes.
        let mut res;
        loop {
            match it.next_char() {
                None => {
                    return self.err_span(
                        LexemeError::UnfinishedStringLiteral,
                        string_start,
                        string_end + it.pos(),
                    );
                }
                Some(c) => {
                    if stop(c) {
                        let contents_end = it.pos() - if triple { 3 } else { 1 };
                        let contents = &self.lexer.remainder()[contents_start..contents_end];
                        self.lexer.bump(it.pos());
                        return Ok((
                            string_start,
                            Token::String(contents.to_owned()),
                            string_end + it.pos(),
                        ));
                    } else if c == '\\' || c == '\r' || (c == '\n' && !triple) {
                        res = String::with_capacity(it.pos() + 10);
                        res.push_str(&self.lexer.remainder()[contents_start..it.pos() - 1]);
                        it2 = CursorChars::new_offset(self.lexer.remainder(), it.pos() - 1);
                        break;
                    }
                }
            }
        }

        // We bailed out of the fast path, that means we now accumulate character by character,
        // might have an error or be dealing with escape characters.
        let mut it = it2;
        while let Some(c) = it.next() {
            if stop(c) {
                self.lexer.bump(it.pos());
                if triple {
                    res.truncate(res.len() - 2);
                }
                return Ok((string_start, Token::String(res), string_end + it.pos()));
            }
            match c {
                '\n' if !triple => {
                    // Will raise an error about out of chars.
                    // But don't include the final \n in the count.
                    string_end -= 1;
                    break;
                }
                '\r' => {
                    // We just ignore these in all modes
                }
                '\\' => {
                    if raw {
                        match it.next() {
                            Some(c) => {
                                if c != '\'' && c != '"' {
                                    res.push('\\');
                                }
                                res.push(c);
                            }
                            _ => break, // Out of chars
                        }
                    } else {
                        let pos = it.pos();
                        if Self::escape(&mut it, &mut res).is_err() {
                            return self.err_span(
                                LexemeError::InvalidEscapeSequence(
                                    self.lexer.remainder()[pos..it.pos()].to_owned(),
                                ),
                                string_end + pos - 1,
                                string_end + it.pos(),
                            );
                        }
                    }
                }
                c => res.push(c),
            }
        }

        // We ran out of characters
        self.err_span(
            LexemeError::UnfinishedStringLiteral,
            string_start,
            string_end + it.pos(),
        )
    }

    fn int(&self, s: &str, radix: u32) -> Lexeme {
        let span = self.lexer.span();
        match i32::from_str_radix(s, radix) {
            Ok(i) => Ok((span.start, Token::Int(TokenInt::I32(i)), span.end)),
            Err(_) => match BigInt::from_str_radix(s, radix) {
                Ok(i) => Ok((span.start, Token::Int(TokenInt::BigInt(i)), span.end)),
                Err(_) => {
                    // This code is unreachable, but safer to return an error than to panic.
                    self.err_now(LexemeError::IntParse)
                }
            },
        }
    }

    pub fn next(&mut self) -> Option<Lexeme> {
        loop {
            // Note that this function doesn't always return - a few branches use `continue`
            // to always go round the loop again.
            return if let Some(x) = self.buffer.pop_front() {
                Some(x)
            } else if self.done {
                None
            } else {
                match self.lexer.next() {
                    None => {
                        self.done = true;
                        let pos = self.lexer.span().end;
                        for _ in 0..self.indent_levels.len() {
                            self.buffer.push_back(Ok((pos, Token::Dedent, pos)))
                        }
                        self.indent_levels.clear();
                        self.wrap(Token::Newline)
                    }
                    Some(token) => match token {
                        Token::Tabs => {
                            if !self.dialect_allow_tabs {
                                self.buffer.push_back(
                                    self.err_pos(LexemeError::InvalidTab, self.lexer.span().start),
                                );
                            }
                            continue;
                        }
                        Token::Newline => {
                            if self.parens == 0 {
                                let span = self.lexer.span();
                                if let Err(e) = self.calculate_indent() {
                                    return Some(Err(e));
                                }
                                Some(Ok((span.start, Token::Newline, span.end)))
                            } else {
                                continue;
                            }
                        }
                        Token::Reserved => Some(self.err_now(LexemeError::ReservedKeyword)),
                        Token::Error => Some(self.err_now(LexemeError::InvalidInput)),
                        Token::RawDecInt => {
                            let s = self.lexer.slice();
                            if s.len() > 1 && &s[0..1] == "0" {
                                return Some(self.err_now(LexemeError::StartsZero));
                            }
                            Some(self.int(s, 10))
                        }
                        Token::RawOctInt => {
                            let s = self.lexer.slice();
                            assert!(s.starts_with("0o") || s.starts_with("0O"));
                            Some(self.int(&s[2..], 8))
                        }
                        Token::RawHexInt => {
                            let s = self.lexer.slice();
                            assert!(s.starts_with("0x") || s.starts_with("0X"));
                            Some(self.int(&s[2..], 16))
                        }
                        Token::RawBinInt => {
                            let s = self.lexer.slice();
                            assert!(s.starts_with("0b") || s.starts_with("0B"));
                            Some(self.int(&s[2..], 2))
                        }
                        Token::Int(..) => unreachable!("Lexer does not produce Int tokens"),
                        Token::RawDoubleQuote => {
                            let raw = self.lexer.span().len() == 2;
                            if self.lexer.remainder().starts_with("\"\"") {
                                let mut qs = 0;
                                Some(self.string(true, raw, |c| {
                                    if c == '\"' {
                                        qs += 1;
                                        qs == 3
                                    } else {
                                        qs = 0;
                                        false
                                    }
                                }))
                            } else {
                                Some(self.string(false, raw, |c| c == '\"'))
                            }
                        }
                        Token::RawSingleQuote => {
                            let raw = self.lexer.span().len() == 2;
                            if self.lexer.remainder().starts_with("''") {
                                let mut qs = 0;
                                Some(self.string(true, raw, |c| {
                                    if c == '\'' {
                                        qs += 1;
                                        qs == 3
                                    } else {
                                        qs = 0;
                                        false
                                    }
                                }))
                            } else {
                                Some(self.string(false, raw, |c| c == '\''))
                            }
                        }
                        Token::OpeningCurly | Token::OpeningRound | Token::OpeningSquare => {
                            self.parens += 1;
                            self.wrap(token)
                        }
                        Token::ClosingCurly | Token::ClosingRound | Token::ClosingSquare => {
                            self.parens -= 1;
                            self.wrap(token)
                        }
                        _ => self.wrap(token),
                    },
                }
            };
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Display)]
pub enum TokenInt {
    #[display(fmt = "{}", .0)]
    I32(i32),
    #[display(fmt = "{}", .0)]
    BigInt(BigInt),
}

/// All token that can be generated by the lexer
#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    #[regex(" +", logos::skip)] // Whitespace
    #[token("\\\n", logos::skip)] // Escaped newline
    #[token("\\\r\n", logos::skip)] // Escaped newline (Windows line ending)
    #[regex(r#"#[^\n]*"#, logos::skip)] // Comments
    #[error]
    Error,

    #[regex("\t+")] // Tabs (might be an error)
    Tabs,

    // Indentation block & meaningfull spaces
    Indent, // New indentation block
    Dedent, // Leaving an indentation block
    #[regex(r"(\r)?\n")]
    Newline, // Newline outside a string

    // Some things the lexer can't deal with well, so we step in and generate
    // things ourselves
    #[token("'")]
    #[token("r'")]
    RawSingleQuote,
    #[token("\"")]
    #[token("r\"")]
    RawDoubleQuote,

    #[regex(
        "as|import|is|class|nonlocal|del|raise|except|try|finally|while|from|with|global|yield"
    )]
    Reserved, // One of the reserved keywords

    #[regex(
        "[a-zA-Z_][a-zA-Z0-9_]*"
    , |lex| lex.slice().to_owned())]
    Identifier(String), // An identifier

    #[regex("[0-9]+")]
    RawDecInt,
    #[regex("0[xX][A-Fa-f0-9]+")]
    RawHexInt,
    #[regex("0[bB][01]+")]
    RawBinInt,
    #[regex("0[oO][0-7]+")]
    RawOctInt,

    Int(TokenInt), // An integer literal (123, 0x1, 0b1011, 0o755, ...)

    #[regex("[0-9]+\\.[0-9]*([eE][-+]?[0-9]+)?", |lex| lex.slice().parse::<f64>())]
    #[regex("[0-9]+[eE][-+]?[0-9]+", |lex| lex.slice().parse::<f64>())]
    #[regex("\\.[0-9]+([eE][-+]?[0-9]+)?", |lex| lex.slice().parse::<f64>())]
    Float(f64), // A float literal (3.14, .3, 1e6, 0.)

    String(String), // A string literal

    // Keywords
    #[token("and")]
    And,
    #[token("else")]
    Else,
    #[token("load")]
    Load,
    #[token("break")]
    Break,
    #[token("for")]
    For,
    #[token("not")]
    Not,
    #[token("continue")]
    Continue,
    #[token("if")]
    If,
    #[token("or")]
    Or,
    #[token("def")]
    Def,
    #[token("in")]
    In,
    #[token("pass")]
    Pass,
    #[token("elif")]
    Elif,
    #[token("return")]
    Return,
    #[token("lambda")]
    Lambda,
    // Symbols
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("+=")]
    PlusEqual,
    #[token("-=")]
    MinusEqual,
    #[token("*=")]
    StarEqual,
    #[token("/=")]
    SlashEqual,
    #[token("//=")]
    SlashSlashEqual,
    #[token("%=")]
    PercentEqual,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    BangEqual,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("**")]
    StarStar,
    #[token("->")]
    MinusGreater,
    #[token("=")]
    Equal,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token("*")]
    Star,
    #[token("%")]
    Percent,
    #[token("/")]
    Slash,
    #[token("//")]
    SlashSlash,
    #[token(".")]
    Dot,
    #[token("&")]
    Ampersand,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("<<")]
    LessLess,
    #[token(">>")]
    GreaterGreater,
    #[token("~")]
    Tilde,
    #[token("&=")]
    AmpersandEqual,
    #[token("|=")]
    PipeEqual,
    #[token("^=")]
    CaretEqual,
    #[token("<<=")]
    LessLessEqual,
    #[token(">>=")]
    GreaterGreaterEqual,

    // Brackets
    #[token("[")]
    OpeningSquare,
    #[token("{")]
    OpeningCurly,
    #[token("(")]
    OpeningRound,
    #[token("]")]
    ClosingSquare,
    #[token("}")]
    ClosingCurly,
    #[token(")")]
    ClosingRound,
}

impl Token {
    /// Used for testing
    pub(crate) fn unlex(&self) -> String {
        match self {
            Token::Indent => "\t".to_owned(),
            Token::Newline => "\n".to_owned(),
            Token::Dedent => "#dedent".to_owned(),
            Token::String(x) => {
                // The Rust {:?} is unstable, so changes between versions,
                // instead use the JSON standard for string escapes.
                // Reuse the StarlarkValue implementation since it's close to hand.
                serde_json::to_string(x).unwrap()
            }
            _ => {
                let s = self.to_string();
                // Out display is often: keyword 'lambda'
                // so strip out the bit in single quotes
                let first = s.find('\'');
                match first {
                    Some(first) if s.ends_with('\'') && first != s.len() - 1 => {
                        s[first + 1..s.len() - 1].to_owned()
                    }
                    _ => s,
                }
            }
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Error => write!(f, "lexical error"),
            Token::Indent => write!(f, "new indentation block"),
            Token::Dedent => write!(f, "end of indentation block"),
            Token::Newline => write!(f, "new line"),
            Token::And => write!(f, "keyword 'and'"),
            Token::Else => write!(f, "keyword 'else'"),
            Token::Load => write!(f, "keyword 'load'"),
            Token::Break => write!(f, "keyword 'break'"),
            Token::For => write!(f, "keyword 'for'"),
            Token::Not => write!(f, "keyword 'not'"),
            Token::Continue => write!(f, "keyword 'continue'"),
            Token::If => write!(f, "keyword 'if'"),
            Token::Or => write!(f, "keyword 'or'"),
            Token::Def => write!(f, "keyword 'def'"),
            Token::In => write!(f, "keyword 'in'"),
            Token::Pass => write!(f, "keyword 'pass'"),
            Token::Elif => write!(f, "keyword 'elif'"),
            Token::Return => write!(f, "keyword 'return'"),
            Token::Lambda => write!(f, "keyword 'lambda'"),
            Token::Comma => write!(f, "symbol ','"),
            Token::Semicolon => write!(f, "symbol ';'"),
            Token::Colon => write!(f, "symbol ':'"),
            Token::PlusEqual => write!(f, "symbol '+='"),
            Token::MinusEqual => write!(f, "symbol '-='"),
            Token::StarEqual => write!(f, "symbol '*='"),
            Token::SlashEqual => write!(f, "symbol '/='"),
            Token::SlashSlashEqual => write!(f, "symbol '//='"),
            Token::PercentEqual => write!(f, "symbol '%='"),
            Token::EqualEqual => write!(f, "symbol '=='"),
            Token::BangEqual => write!(f, "symbol '!='"),
            Token::LessEqual => write!(f, "symbol '<='"),
            Token::GreaterEqual => write!(f, "symbol '>='"),
            Token::StarStar => write!(f, "symbol '**'"),
            Token::MinusGreater => write!(f, "symbol '->'"),
            Token::Equal => write!(f, "symbol '='"),
            Token::LessThan => write!(f, "symbol '<'"),
            Token::GreaterThan => write!(f, "symbol '>'"),
            Token::Minus => write!(f, "symbol '-'"),
            Token::Plus => write!(f, "symbol '+'"),
            Token::Star => write!(f, "symbol '*'"),
            Token::Percent => write!(f, "symbol '%'"),
            Token::Slash => write!(f, "symbol '/'"),
            Token::SlashSlash => write!(f, "symbol '//'"),
            Token::Dot => write!(f, "symbol '.'"),
            Token::Ampersand => write!(f, "symbol '&'"),
            Token::Pipe => write!(f, "symbol '|'"),
            Token::Caret => write!(f, "symbol '^'"),
            Token::LessLess => write!(f, "symbol '<<'"),
            Token::GreaterGreater => write!(f, "symbol '>>'"),
            Token::Tilde => write!(f, "symbol '~'"),
            Token::AmpersandEqual => write!(f, "symbol '&='"),
            Token::PipeEqual => write!(f, "symbol '|='"),
            Token::CaretEqual => write!(f, "symbol '^='"),
            Token::LessLessEqual => write!(f, "symbol '<<='"),
            Token::GreaterGreaterEqual => write!(f, "symbol '>>='"),
            Token::OpeningSquare => write!(f, "symbol '['"),
            Token::OpeningCurly => write!(f, "symbol '{{'"),
            Token::OpeningRound => write!(f, "symbol '('"),
            Token::ClosingSquare => write!(f, "symbol ']'"),
            Token::ClosingCurly => write!(f, "symbol '}}'"),
            Token::ClosingRound => write!(f, "symbol ')'"),
            Token::Reserved => write!(f, "reserved keyword"),
            Token::Identifier(s) => write!(f, "identifier '{}'", s),
            Token::Int(i) => write!(f, "integer literal '{}'", i),
            Token::RawDecInt => write!(f, "decimal integer literal"),
            Token::RawHexInt => write!(f, "hexadecimal integer literal"),
            Token::RawOctInt => write!(f, "octal integer literal"),
            Token::RawBinInt => write!(f, "binary integer literal"),
            Token::Float(n) => write!(f, "float literal '{}'", n),
            Token::String(s) => write!(f, "string literal '{}'", s),
            Token::RawSingleQuote => write!(f, "starting '"),
            Token::RawDoubleQuote => write!(f, "starting \""),
            Token::Tabs => Ok(()),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Lexeme;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}
