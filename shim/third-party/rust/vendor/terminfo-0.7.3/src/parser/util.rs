//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//                    Version 2, December 2004
//
// Copyleft (ↄ) meh. <meh@schizofreni.co> | http://meh.schizofreni.co
//
// Everyone is permitted to copy and distribute verbatim or modified
// copies of this license document, and changing it is allowed as long
// as the name is changed.
//
//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//  0. You just DO WHAT THE FUCK YOU WANT TO.

use std::str;
use std::u8;
use std::borrow::Cow;
use nom::character::{streaming::line_ending as eol, is_digit};

macro_rules! take_until_or_eof (
  ($i:expr, $substr:expr) => (
    {
      use $crate::nom::InputLength;
      use $crate::nom::FindSubstring;
      use $crate::nom::Slice;

      let ret: $crate::nom::IResult<_, _> = if $substr.input_len() > $i.input_len() {
        Err($crate::nom::Err::Incomplete($crate::nom::Needed::Size($substr.input_len())))
      }
			else {
        match ($i).find_substring($substr) {
          None => {
            Ok(($i.slice(0..0), $i))
          },

          Some(index) => {
            Ok(($i.slice(index..), $i.slice(0..index)))
          },
        }
      };

      ret
    }
  );
);

const NONE:    u8 = 0b000000;
const PRINT:   u8 = 0b000001;
const SPACE:   u8 = 0b000010;
const CONTROL: u8 = 0b000100;
const PIPE:    u8 = 0b001000;
const COMMA:   u8 = 0b010000;
const EOL:     u8 = 0b100000;

// Ugly table of DOOM, gotta run and gun.
static ASCII: [u8; 256] = [
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, SPACE, EOL, NONE, NONE, EOL, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	PRINT | SPACE, PRINT, PRINT, PRINT | CONTROL, PRINT, PRINT, PRINT, PRINT,
	PRINT, PRINT, PRINT, PRINT, PRINT | COMMA | CONTROL, PRINT, PRINT, PRINT,
	PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
	PRINT, PRINT, PRINT, PRINT, PRINT, PRINT | CONTROL, PRINT, PRINT,
	PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
	PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
	PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
	PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
	PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
	PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
	PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT, PRINT,
	PRINT, PRINT, PRINT, PRINT, PRINT | PIPE, PRINT, PRINT, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
	NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
];

#[inline(always)]
pub fn is_ws(ch: u8) -> bool {
	unsafe { ASCII.get_unchecked(ch as usize) & SPACE == SPACE }
}

#[inline(always)]
pub fn is_eol(ch: u8) -> bool {
	unsafe { ASCII.get_unchecked(ch as usize) & EOL == EOL }
}

#[inline(always)]
pub fn is_printable_no_pipe(ch: u8) -> bool {
	unsafe { ASCII.get_unchecked(ch as usize) & (PRINT | PIPE) == PRINT }
}

#[inline(always)]
pub fn is_printable_no_comma(ch: u8) -> bool {
	unsafe { ASCII.get_unchecked(ch as usize) & (PRINT | COMMA) == PRINT }
}

#[inline(always)]
pub fn is_printable_no_control(ch: u8) -> bool {
	unsafe { ASCII.get_unchecked(ch as usize) & (PRINT | CONTROL) == PRINT }
}

named!(pub ws<char>,
	alt!(char!(' ') | char!('\t')));

named!(pub end,
	alt!(eof!() | eol));

#[inline]
pub fn number(i: &[u8]) -> i32 {
	let mut n: i32 = 0;

	for &ch in i {
		let d = (ch as i32).wrapping_sub(b'0' as i32);

		if d <= 9 {
			n = n.saturating_mul(10).saturating_add(d);
		}
	}

	n
}

pub fn unescape(i: &[u8]) -> Cow<[u8]> {
	fn escape<I: Iterator<Item = u8>>(output: &mut Vec<u8>, iter: &mut I) {
		match iter.next() {
			None => (),

			Some(b'a') =>
				output.push(0x07),

			Some(b'b') =>
				output.push(0x08),

			Some(b'E') | Some(b'e') =>
				output.push(0x1B),

			Some(b'f') =>
				output.push(0x0C),

			Some(b'l') | Some(b'n') =>
				output.push(b'\n'),

			Some(b'r') =>
				output.push(b'\r'),

			Some(b's') =>
				output.push(b' '),

			Some(b't') =>
				output.push(b'\t'),

			Some(b'^') =>
				output.push(b'^'),

			Some(b'\\') =>
				output.push(b'\\'),

			Some(b',') =>
				output.push(b','),

			Some(b':') =>
				output.push(b':'),

			Some(b'0') =>
				output.push(0x00),

			Some(a) if is_digit(a) =>
				match (iter.next(), iter.next()) {
					(Some(b), Some(c)) if is_digit(b) && is_digit(c) =>
						if let Ok(number) = u8::from_str_radix(unsafe { str::from_utf8_unchecked(&[a, b, c]) }, 8) {
							output.push(number);
						}
						else {
							output.extend(&[a, b, c]);
						},

					(Some(b), None) =>
						output.extend(&[b'\\', a, b]),

					(None, None) =>
						output.extend(&[b'\\', a]),

					_ =>
						unreachable!()
				},

			Some(ch) =>
				output.extend(&[b'\\', ch]),
		}
	}

	fn control<I: Iterator<Item = u8>>(output: &mut Vec<u8>, iter: &mut I) {
		match iter.next() {
			None => (),

			Some(ch) if ch >= b'A' && ch <= b'Z' =>
				output.push(ch - b'A' + 1),

			Some(ch) if ch >= b'a' && ch <= b'z' =>
				output.push(ch - b'a' + 1),

			Some(ch) =>
				output.extend(&[b'^', ch]),
		}
	}

	let mut chars  = i.iter().cloned();
	let mut offset = 0;

	while let Some(ch) = chars.next() {
		if ch == b'\\' || ch == b'^' {
			let mut output = (&i[.. offset]).to_vec();

			match ch {
				b'\\' =>
					escape(&mut output, &mut chars),

				b'^' =>
					control(&mut output, &mut chars),

				_ =>
					unreachable!()
			}

			while let Some(ch) = chars.next() {
				match ch {
					b'\\' =>
						escape(&mut output, &mut chars),

					b'^' =>
						control(&mut output, &mut chars),

					ch =>
						output.push(ch)
				}
			}

			return Cow::Owned(output);
		}

		offset += 1;
	}

	Cow::Borrowed(i)
}
