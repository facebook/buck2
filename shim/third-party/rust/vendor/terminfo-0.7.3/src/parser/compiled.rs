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
use nom::number::streaming::{le_i16, le_i32};

use crate::names;
use crate::capability::Value;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Database<'a> {
	names:    &'a [u8],
	standard: Standard<'a>,
	extended: Option<Extended<'a>>,
}

impl<'a> Into<crate::Database> for Database<'a> {
	fn into(self) -> crate::Database {
		let mut names = self.names.split(|&c| c == b'|')
			.map(|s| unsafe { str::from_utf8_unchecked(s) })
			.map(|s| s.trim())
			.collect::<Vec<_>>();

		let mut database = crate::Database::new();

		database
			.name(names.remove(0))
			.description(names.pop().unwrap())
			.aliases(names);

		for (index, _) in self.standard.booleans.iter().enumerate().filter(|&(_, &value)| value) {
			if let Some(&name) = names::BOOLEAN.get(&(index as u16)) {
				database.raw(name,
					Value::True);
			}
		}

		for (index, &value) in self.standard.numbers.iter().enumerate().filter(|&(_, &n)| n >= 0) {
			if let Some(&name) = names::NUMBER.get(&(index as u16)) {
				database.raw(name,
					Value::Number(value));
			}
		}

		for (index, &offset) in self.standard.strings.iter().enumerate().filter(|&(_, &n)| n >= 0) {
			if let Some(&name) = names::STRING.get(&(index as u16)) {
				let string = &self.standard.table[offset as usize ..];
				let edge   = string.iter().position(|&c| c == 0).unwrap();

				database.raw(name,
					Value::String(Vec::from(&string[.. edge])));
			}
		}

		if let Some(extended) = self.extended {
			let names = extended.table.split(|&c| c == 0)
				.skip(extended.strings.iter().cloned().filter(|&n| n >= 0).count())
				.map(|s| unsafe { str::from_utf8_unchecked(s) })
				.collect::<Vec<_>>();

			for (index, _) in extended.booleans.iter().enumerate().filter(|&(_, &value)| value)  {
				database.raw(names[index],
					Value::True);
			}

			for (index, &value) in extended.numbers.iter().enumerate().filter(|&(_, &n)| n >= 0) {
				database.raw(names[extended.booleans.len() + index],
					Value::Number(value));
			}

			for (index, &offset) in extended.strings.iter().enumerate().filter(|&(_, &n)| n >= 0) {
				let string = &extended.table[offset as usize ..];
				let edge   = string.iter().position(|&c| c == 0).unwrap();

				database.raw(names[extended.booleans.len() + extended.numbers.len() + index],
					Value::String(Vec::from(&string[.. edge])));
			}
		}

		database.build().unwrap()
	}
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Standard<'a> {
	booleans: Vec<bool>,
	numbers:  Vec<i32>,
	strings:  Vec<i32>,
	table:    &'a [u8],
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Extended<'a> {
	booleans: Vec<bool>,
	numbers:  Vec<i32>,
	strings:  Vec<i32>,
	names:    Vec<i32>,
	table:    &'a [u8],
}

fn bit_size(magic: &[u8]) -> usize {
	match magic[1] {
		0x01 => 16,
		0x02 => 32,

		_ =>
			unreachable!("unknown magic number")
	}
}

macro_rules! cond_reduce(
	($i:expr, $cond:expr, $submac:ident!( $($args:tt)* )) => (
		{
			use nom::lib::std::result::Result::*;
			use nom::{Err,error::ErrorKind,IResult};
			let default_err = Err(Err::convert(Err::Error(error_position!($i, ErrorKind::MapRes))));

			if $cond {
				let sub_res = $submac!($i, $($args)*);
				fn unify_types<I,O,E>(_: &IResult<I,O,E>, _: &IResult<I,O,E>) {}
				unify_types(&sub_res, &default_err);

				match sub_res {
					Ok((i,o)) => Ok((i, o)),
					Err(e)    => Err(e),
				}
			} else {
				default_err
			}
		}
	);
	($i:expr, $cond:expr, $f:expr) => (
		cond_reduce!($i, $cond, call!($f));
	);
);

named!(pub parse<Database>,
	do_parse!(
		magic: alt!(tag!([0x1A, 0x01]) | tag!([0x1E, 0x02])) >>

		name_size:    size >>
		bool_count:   size >>
		num_count:    size >>
		string_count: size >>
		table_size:   size >>

		names: flat_map!(take!(name_size),
			take_until!("\x00")) >>

		booleans: count!(boolean,
			bool_count) >>

		cond!((name_size + bool_count) % 2 != 0,
			take!(1)) >>

		numbers: count!(call!(capability, bit_size(magic)),
			num_count) >>

		strings: count!(call!(capability, 16),
			string_count) >>

		table: take!(table_size) >>

		extended: opt!(complete!(do_parse!(
			cond!(table_size % 2 != 0,
				take!(1)) >>

			ext_bool_count:    size >>
			ext_num_count:     size >>
			ext_string_count:  size >>
			_ext_offset_count: size >>
			ext_table_size:    size >>

			booleans: count!(boolean,
				ext_bool_count) >>

			cond!(ext_bool_count % 2 != 0,
				take!(1)) >>

			numbers: count!(call!(capability, bit_size(magic)),
				ext_num_count) >>

			strings: count!(call!(capability, 16),
				ext_string_count) >>

			names: count!(call!(capability, 16),
				ext_bool_count + ext_num_count + ext_string_count) >>

			table: take!(ext_table_size) >>

			(Extended {
				booleans: booleans,
				numbers:  numbers,
				strings:  strings,
				names:    names,
				table:    table,
			})))) >>

		(Database {
			names: names,

			standard: Standard {
				booleans: booleans,
				numbers:  numbers,
				strings:  strings,
				table:    table,
			},

			extended: extended,
		})));

named!(boolean<bool>,
	alt!(tag!([0]) => { |_| false } |
	     tag!([1]) => { |_| true }));

named!(size<usize>,
	map_opt!(le_i16, |n| match n {
		-1          => Some(0),
		n if n >= 0 => Some(n as usize),
		_           => None }));

named_args!(capability(bits: usize)<i32>,
	alt!(
		cond_reduce!(bits == 16,
			map_opt!(le_i16, |n| if n >= -2 { Some(n as i32) } else { None })) |

		cond_reduce!(bits == 32,
			map_opt!(le_i32, |n| if n >= -2 { Some(n) } else { None }))));

#[cfg(test)]
mod test {
	use std::fs::File;
	use std::io::Read;
	use std::path::Path;
	use super::*;
	use crate::capability as cap;

	fn load<P: AsRef<Path>, F: FnOnce(crate::Database)>(path: P, f: F) {
		let mut file   = File::open(path).unwrap();
		let mut buffer = Vec::new();
		file.read_to_end(&mut buffer).unwrap();

		f(parse(&buffer).unwrap().1.into())
	}

	#[test]
	fn name() {
		load("tests/cancer-256color", |db|
			assert_eq!("cancer-256color", db.name()));
	}

	#[test]
	fn aliases() {
		load("tests/st-256color", |db|
			assert_eq!(vec!["stterm-256color"], db.aliases()));
	}

	#[test]
	fn description() {
		load("tests/cancer-256color", |db|
			assert_eq!("terminal cancer with 256 colors", db.description()));
	}

	#[test]
	fn standard() {
		load("tests/st-256color", |db| {
			assert_eq!(Some(cap::Columns(80)), db.get::<cap::Columns>());
			assert_eq!(Some(cap::AutoRightMargin(true)), db.get::<cap::AutoRightMargin>());
			assert_eq!(Some(cap::AutoLeftMargin(false)), db.get::<cap::AutoLeftMargin>());
		});
	}

	#[test]
	fn extended() {
		load("tests/cancer-256color", |db| {
			assert_eq!(Some(&cap::Value::True), db.raw("Ts"));
			assert_eq!(Some(&cap::Value::True), db.raw("AX"));
			assert_eq!(Some(&cap::Value::String(b"\x1B[2 q".to_vec())), db.raw("Se"));
		});
	}

	#[test]
	fn bigger_numbers() {
		load("tests/xterm-256color", |db|
			assert_eq!("xterm-256color", db.name()));
	}
}
