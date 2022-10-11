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

use std::char;
use std::io::{BufWriter, Write};

use crate::error;
use crate::parser::expansion::*;

/// Trait for items that can be expanded.
pub trait Expand {
	fn expand<W: Write>(&self, output: W, parameters: &[Parameter], context: &mut Context) -> error::Result<()>;
}

/// An expansion parameter.
#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Parameter {
	/// A number.
	Number(i32),

	/// An ASCII string.
	String(Vec<u8>),
}

impl Default for Parameter {
	fn default() -> Self {
		Parameter::Number(0)
	}
}

macro_rules! from {
	(number $ty:ty) => (
		impl From<$ty> for Parameter {
			fn from(value: $ty) -> Self {
				Parameter::Number(value as i32)
			}
		}
	);

	(string ref $ty:ty) => (
		impl<'a> From<&'a $ty> for Parameter {
			fn from(value: &'a $ty) -> Self {
				Parameter::String(value.into())
			}
		}
	);

	(string $ty:ty) => (
		impl From<$ty> for Parameter {
			fn from(value: $ty) -> Self {
				Parameter::String(value.into())
			}
		}
	);
}

from!(number bool);
from!(number u8);
from!(number i8);
from!(number u16);
from!(number i16);
from!(number u32);
from!(number i32);

from!(string String);
from!(string ref str);
from!(string Vec<u8>);
from!(string ref [u8]);

/// The expansion context.
///
/// The same context should be passed around through every expansion for the
/// same `Database`.
#[derive(Eq, PartialEq, Default, Debug)]
pub struct Context {
	pub fixed:   [Parameter; 26],
	pub dynamic: [Parameter; 26],
}

/// Expand a parametrized string.
///
/// ## Examples
///
/// Write the expansion to `stdout`.
///
/// ```
/// # #[macro_use] extern crate terminfo;
///
/// use terminfo::{Database, capability as cap};
/// use std::io;
///
/// # fn main() {
///
/// let info = Database::from_env().unwrap();
///
/// // Move the cursor to X: 20, Y: 30
/// expand!(io::stdout(), info.get::<cap::CursorAddress>().unwrap().as_ref(); 20, 30).unwrap();
///
/// # }
/// ```
///
/// Load the expansion for later usage.
///
/// ```
/// # #[macro_use] extern crate terminfo;
///
/// use terminfo::{Database, capability as cap};
/// use std::io;
///
/// # fn main() {
///
/// let info = Database::from_env().unwrap();
///
/// // Set foreground color to red.
/// let red     = expand!(info.get::<cap::SetAForeground>().unwrap().as_ref(); 1).unwrap();
/// let on_blue = expand!(info.get::<cap::SetABackground>().unwrap().as_ref(); 4).unwrap();
///
/// # }
/// ```
#[macro_export]
macro_rules! expand {
	($value:expr) => (
		expand!($value;)
	);

	($value:expr => $context:expr) => (
		expand!($value => $context;)
	);

	($value:expr; $($item:expr),*) => (
		expand!($value => &mut Default::default(); $($item),*)
	);

	($value:expr => $context:expr; $($item:expr),*) => ({
		let mut output = Vec::new();

		match expand!(&mut output, $value => $context; $($item),*) {
			Ok(()) => Ok(output),
			Err(e) => Err(e)
		}
	});

	($output:expr, $value:expr) => (
		expand!($output, $value;)
	);

	($output:expr, $value:expr => $context:expr) => (
		expand!($output, $value => $context;)
	);

	($output:expr, $value:expr; $($item:expr),*) => (
		expand!($output, $value => &mut Default::default(); $($item),*)
	);

	($output:expr, $value:expr => $context:expr; $($item:expr),*) => ({
		use $crate::Expand;
		$value.expand($output, &[$($item.into()),*], $context)
	})
}

impl Expand for [u8] {
	fn expand<W: Write>(&self, output: W, parameters: &[Parameter], context: &mut Context) -> error::Result<()> {
		let mut output                 = BufWriter::new(output);
		let mut input                  = self;
		let mut params: [Parameter; 9] = Default::default();
		let mut stack                  = Vec::new();
		let mut conditional            = false;
		let mut incremented            = false;

		for (dest, source) in params.iter_mut().zip(parameters.iter()) {
			*dest = source.clone();
		}

		macro_rules! next {
			() => (
				match parse(input) {
					Ok((rest, item)) => {
						input = rest;
						item
					}

					Err(_) =>
						return Err(error::Expand::Invalid.into())
				}
			);
		}

		'main: while !input.is_empty() {
			match next!() {
				Item::Conditional(Conditional::If) => {
					conditional = true;
				}

				Item::Conditional(Conditional::End) if conditional => {
					conditional = false;
				}

				Item::Conditional(Conditional::Then) if conditional => {
					match stack.pop() {
						Some(Parameter::Number(0)) => {
							let mut level = 0;

							while !input.is_empty() {
								match next!() {
									Item::Conditional(Conditional::End) |
									Item::Conditional(Conditional::Else) if level == 0 =>
										continue 'main,

									Item::Conditional(Conditional::If) =>
										level += 1,

									Item::Conditional(Conditional::End) =>
										level -= 1,

									_ => (),
								}
							}

							return Err(error::Expand::Invalid.into());
						}

						Some(_) =>
							(),

						None =>
							return Err(error::Expand::StackUnderflow.into()),
					}
				}

				Item::Conditional(Conditional::Else) if conditional => {
					let mut level = 0;

					while !input.is_empty() {
						match next!() {
							Item::Conditional(Conditional::End) if level == 0 =>
								continue 'main,

							Item::Conditional(Conditional::If) =>
								level += 1,

							Item::Conditional(Conditional::End) =>
								level -= 1,

							_ => (),
						}
					}

					return Err(error::Expand::Invalid.into());
				}

				Item::Conditional(..) =>
					return Err(error::Expand::Invalid.into()),

				Item::String(value) =>
					output.write_all(value)?,

				Item::Constant(Constant::Character(ch)) => {
					stack.push(Parameter::Number(ch as i32));
				}

				Item::Constant(Constant::Integer(value)) => {
					stack.push(Parameter::Number(value));
				}

				Item::Variable(Variable::Length) => {
					match stack.pop() {
						Some(Parameter::String(ref value)) => {
							stack.push(Parameter::Number(value.len() as i32));
						}

						Some(_) => {
							return Err(error::Expand::TypeMismatch.into());
						}

						None => {
							return Err(error::Expand::StackUnderflow.into());
						}
					}
				}

				Item::Variable(Variable::Push(index)) => {
					stack.push(params[index as usize].clone());
				}

				Item::Variable(Variable::Set(dynamic, index)) => {
					if let Some(value) = stack.pop() {
						if dynamic {
							context.dynamic[index as usize] = value.clone();
						}
						else {
							context.fixed[index as usize] = value.clone();
						}
					}
					else {
						return Err(error::Expand::StackUnderflow.into());
					}
				}

				Item::Variable(Variable::Get(dynamic, index)) => {
					if dynamic {
						stack.push(context.dynamic[index as usize].clone());
					}
					else {
						stack.push(context.fixed[index as usize].clone());
					}
				}

				Item::Operation(Operation::Increment) if !incremented => {
					incremented = true;

					if let (&Parameter::Number(x), &Parameter::Number(y)) = (&params[0], &params[1]) {
						params[0] = Parameter::Number(x + 1);
						params[1] = Parameter::Number(y + 1);
					}
					else {
						return Err(error::Expand::TypeMismatch.into());
					}
				}

				Item::Operation(Operation::Increment) => (),

				Item::Operation(Operation::Binary(operation)) => {
					match (stack.pop(), stack.pop()) {
						(Some(Parameter::Number(y)), Some(Parameter::Number(x))) =>
							stack.push(Parameter::Number(match operation {
								Binary::Add       => x + y,
								Binary::Subtract  => x - y,
								Binary::Multiply  => x * y,
								Binary::Divide    => if y != 0 { x / y } else { 0 },
								Binary::Remainder => if y != 0 { x % y } else { 0 },

								Binary::AND => x & y,
								Binary::OR  => x | y,
								Binary::XOR => x ^ y,

								Binary::And => (x != 0 && y != 0) as i32,
								Binary::Or  => (x != 0 || y != 0) as i32,

								Binary::Equal   => (x == y) as i32,
								Binary::Greater => (x > y) as i32,
								Binary::Lesser  => (x < y) as i32,
							})),

						(Some(_), Some(_)) =>
							return Err(error::Expand::TypeMismatch.into()),

						_ =>
							return Err(error::Expand::StackUnderflow.into()),
					}
				}

				Item::Operation(Operation::Unary(operation)) => {
					match stack.pop() {
						Some(Parameter::Number(x)) =>
							stack.push(Parameter::Number(match operation {
								Unary::Not => (x != 0) as i32,
								Unary::NOT => !x,
							})),

						Some(_) =>
							return Err(error::Expand::TypeMismatch.into()),

						_ =>
							return Err(error::Expand::StackUnderflow.into()),
					}
				}

				Item::Print(p) => {
					/// Calculate the length of a formatted number.
					fn length(value: i32, p: &Print) -> usize {
						let digits = match p.format {
							Format::Dec =>
								(value as f32).abs().log(10.0).floor() as usize + 1,

							Format::Oct =>
								(value as f32).abs().log(8.0).floor() as usize + 1,

							Format::Hex |
							Format::HEX =>
								(value as f32).abs().log(16.0).floor() as usize + 1,

							_ => unreachable!(),
						};

						let mut length = digits;

						// Add the minimum number of digits.
						if p.flags.precision > digits {
							length += p.flags.precision - digits;
						}

						// Add the sign if present.
						if p.format == Format::Dec && (value < 0 || p.flags.sign) {
							length += 1;
						}

						// Add the alternate representation.
						if p.flags.alternate {
							match p.format {
								Format::Hex | Format::HEX =>
									length += 2,

								Format::Oct =>
									length += 1,

								_ => ()
							}
						}

						length
					}

					macro_rules! w {
						($value:expr) => (
							output.write_all($value)?;
						);

						($($item:tt)*) => (
							write!(output, $($item)*)?;
						);
					}

					macro_rules! f {
						(by $length:expr) => (
							for _ in 0 .. p.flags.width - $length {
								output.write_all(if p.flags.space { b" " } else { b"0" })?;
							}
						);

						(before by $length:expr) => (
							if !p.flags.left && p.flags.width > $length {
								f!(by $length);
							}
						);

						(after by $length:expr) => (
							if p.flags.left && p.flags.width > $length {
								f!(by $length);
							}
						);

						(before $value:expr) => (
							f!(before by length($value, &p));
						);

						(after $value:expr) => (
							f!(after by length($value, &p));
						);
					}

					match (p.format, stack.pop()) {
						(Format::Str, Some(Parameter::String(ref value))) => {
							let mut value = &value[..];

							if p.flags.precision > 0 && p.flags.precision < value.len() {
								value = &value[.. p.flags.precision];
							}

							f!(before by value.len());
							w!(value);
							f!(after by value.len());
						}

						(Format::Chr, Some(Parameter::Number(value))) =>
							w!("{}", value as u8 as char),

						(Format::Uni, Some(Parameter::Number(value))) =>
							w!("{}", char::from_u32(value as u32).ok_or(error::Expand::TypeMismatch)?),

						(Format::Dec, Some(Parameter::Number(value))) => {
							f!(before value);

							if p.flags.sign && value >= 0 {
								w!(b"+");
							}

							w!("{:.1$}", value, p.flags.precision);

							f!(after value);
						}

						(Format::Oct, Some(Parameter::Number(value))) => {
							f!(before value);

							if p.flags.alternate {
								w!(b"0");
							}

							w!("{:.1$o}", value, p.flags.precision);

							f!(after value);
						}

						(Format::Hex, Some(Parameter::Number(value))) => {
							f!(before value);

							if p.flags.alternate {
								w!(b"0x");
							}

							w!("{:.1$x}", value, p.flags.precision);

							f!(after value);
						}

						(Format::HEX, Some(Parameter::Number(value))) => {
							f!(before value);

							if p.flags.alternate {
								w!(b"0X");
							}

							w!("{:.1$X}", value, p.flags.precision);

							f!(after value);
						}

						(_, Some(_)) =>
							return Err(error::Expand::TypeMismatch.into()),

						(_, None) =>
							return Err(error::Expand::StackUnderflow.into()),
					}
				}
			}
		}

		Ok(())
	}
}

#[cfg(test)]
mod test {
	#[test]
	fn test_basic_setabf() {
		assert_eq!(b"\\E[48;5;1m".to_vec(),
			expand!(b"\\E[48;5;%p1%dm"; 1).unwrap());
	}

	#[test]
	fn print() {
		assert_eq!(b"0001".to_vec(),
			expand!(b"%p1%4d"; 1).unwrap());

		assert_eq!(b"10".to_vec(),
			expand!(b"%p1%o"; 8).unwrap());
	}

	#[test]
	fn conditional() {
		assert_eq!(b"1".to_vec(),
			expand!(b"%?%p1%t1%e2%;"; 1).unwrap());

		assert_eq!(b"2".to_vec(),
			expand!(b"%?%p1%t1%e2%;"; 0).unwrap());

		assert_eq!(b"3".to_vec(),
			expand!(b"%?%p1%t%e%p2%t2%e%p3%t3%;"; 0, 0, 1).unwrap());
	}
}
