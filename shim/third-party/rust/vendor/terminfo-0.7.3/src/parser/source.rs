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

use std::borrow::Cow;
use std::str;
use nom::character::{is_digit, streaming::line_ending as eol};
use crate::parser::util::{is_printable_no_pipe, is_printable_no_comma, is_printable_no_control};
use crate::parser::util::{is_eol, is_ws, ws, end};
use crate::parser::util::unescape;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Item<'a> {
	Comment(&'a str),

	Definition {
		name:        &'a str,
		aliases:     Vec<&'a str>,
		description: &'a str,
	},

	True(&'a str),
	Number(&'a str, i16),
	String(&'a str, Cow<'a, [u8]>),
	Disable(&'a str),
}

named!(pub parse<Item>,
	alt!(comment | definition | disable | entry));

named!(comment<Item>,
	do_parse!(
		tag!("#") >>
		content: map_res!(terminated!(take_until!("\n"), tag!("\n")), str::from_utf8) >>
		opt!(complete!(take_while!(is_eol))) >>

		(Item::Comment(content.trim()))));

named!(definition<Item>,
	do_parse!(
		name: map!(take_while!(is_printable_no_pipe), |n|
			unsafe { str::from_utf8_unchecked(n) }) >>

		tag!("|") >>

		content: map!(take_while!(is_printable_no_comma), |n|
			unsafe { str::from_utf8_unchecked(n) }) >>

		tag!(",") >>
		take_while!(is_ws) >>
		eol >> opt!(complete!(take_while!(is_eol))) >>

		({
			let mut aliases = content.split(|c| c == '|').map(|n| n.trim()).collect::<Vec<_>>();

			Item::Definition {
				name:        name,
				description: aliases.pop().unwrap(),
				aliases:     aliases,
			}
		})));

named!(disable<Item>,
	do_parse!(
		ws >> take_while!(is_ws) >>
		tag!("@") >>

		name: map!(take_while!(is_printable_no_control), |n|
			unsafe { str::from_utf8_unchecked(n) }) >>

		tag!(",") >>
		take_while!(is_ws) >> end >> opt!(complete!(take_while!(is_eol))) >>

		(Item::Disable(name))));

named!(entry<Item>,
	do_parse!(
		ws >> take_while!(is_ws) >>

		name: map!(take_while!(is_printable_no_control), |n|
			unsafe { str::from_utf8_unchecked(n) }) >>

		value: switch!(take!(1),
			b"," => value!(
				Item::True(name)) |

			b"#" => do_parse!(
				value: map!(take_while!(is_digit), |n|
					unsafe { str::from_utf8_unchecked(n) }) >>

				tag!(",") >>

				(Item::Number(name, value.parse().unwrap()))) |

			b"=" => do_parse!(
				value: take_while!(is_printable_no_comma) >>

				tag!(",") >>

				(Item::String(name, unescape(value))))) >>

		take_while!(is_ws) >> end >> opt!(complete!(take_while!(is_eol))) >>

		(value)));

#[cfg(test)]
mod test {
	use super::*;

	use std::fs::File;
	use std::io::Read;

	#[test]
	fn parsing() {
		let mut file   = File::open("tests/xterm.terminfo").unwrap();
		let mut buffer = Vec::new();
		file.read_to_end(&mut buffer).unwrap();

		let mut input = &buffer[..];

		while !input.is_empty() {
			match parse(input) {
				Ok((rest, _)) =>
					input = rest,

				Err(::nom::Err::Incomplete(_)) =>
					panic!("incomplete"),

				Err(err) =>
					panic!("parsing: {:?}", err),
			}
		}
	}
}
