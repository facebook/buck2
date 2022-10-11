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

use std::env;
use std::path::{Path, PathBuf};
use std::fs::{self, File};
use std::io::Read;
use std::collections::HashMap;
use std::hash::BuildHasherDefault;
use fnv::FnvHasher;
use dirs;

use crate::capability::{Capability, Value};
use crate::names;
use crate::error::{self, Error};
use crate::parser::compiled;

/// A capability database.
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Database {
	name:        String,
	aliases:     Vec<String>,
	description: String,
	inner:       HashMap<String, Value, BuildHasherDefault<FnvHasher>>,
}

/// Builder for a new `Database`.
#[derive(Default, Debug)]
pub struct Builder {
	name:        Option<String>,
	aliases:     Vec<String>,
	description: Option<String>,
	inner:       HashMap<String, Value, BuildHasherDefault<FnvHasher>>
}

impl Builder {
	/// Build the database.
	pub fn build(self) -> Result<Database, ()> {
		Ok(Database {
			name:        self.name.ok_or(())?,
			aliases:     self.aliases,
			description: self.description.ok_or(())?,
			inner:       self.inner,
		})
	}

	/// Set the terminal name.
	pub fn name<T: Into<String>>(&mut self, name: T) -> &mut Self {
		self.name = Some(name.into());
		self
	}

	/// Set the terminal aliases.
	pub fn aliases<T, I>(&mut self, iter: I) -> &mut Self
		where T: Into<String>,
		      I: IntoIterator<Item = T>,
	{
		self.aliases = iter.into_iter().map(|a| a.into()).collect();
		self
	}

	/// Set the terminal description.
	pub fn description<T: Into<String>>(&mut self, description: T) -> &mut Self {
		self.description = Some(description.into());
		self
	}

	/// Set a capability.
	///
	/// ## Example
	///
	/// ```
	/// use terminfo::{Database, capability as cap};
	///
	/// let mut info = Database::new();
	/// info.name("foo");
	/// info.description("foo terminal");
	///
	/// // Set the amount of available colors.
	/// info.set(cap::MaxColors(16));
	///
	/// info.build().unwrap();
	/// ```
	pub fn set<'a, C: Capability<'a>>(&'a mut self, value: C) -> &mut Self {
		if !self.inner.contains_key(C::name()) {
			if let Some(value) = C::into(value) {
				self.inner.insert(C::name().into(), value);
			}
		}

		self
	}

	/// Set a raw capability.
	///
	/// ## Example
	///
	/// ```
	/// use terminfo::{Database, capability as cap};
	///
	/// let mut info = Database::new();
	/// info.name("foo");
	/// info.description("foo terminal");
	///
	/// // Set the amount of available colors.
	/// info.raw("colors", 16);
	///
	/// info.build().unwrap();
	/// ```
	pub fn raw<S: AsRef<str>, V: Into<Value>>(&mut self, name: S, value: V) -> &mut Self {
		let name = name.as_ref();
		let name = names::ALIASES.get(name).map(|s| *s).unwrap_or(name);

		if !self.inner.contains_key(name) {
			self.inner.insert(name.into(), value.into());
		}

		self
	}
}

impl Database {
	/// Create a new empty database.
	pub fn new() -> Builder {
		Builder::default()
	}

	/// Load a database from the current environment.
	pub fn from_env() -> error::Result<Self> {
		if let Ok(name) = env::var("TERM") {
			Self::from_name(name)
		}
		else {
			Err(Error::NotFound)
		}
	}

	/// Load a database for the given name.
	pub fn from_name<N: AsRef<str>>(name: N) -> error::Result<Self> {
		let name  = name.as_ref();
		let first = name.chars().next().ok_or(Error::NotFound)?;

		// See https://manpages.debian.org/buster/ncurses-bin/terminfo.5.en.html#Fetching_Compiled_Descriptions
		let mut search = Vec::<PathBuf>::new();

		if let Some(dir) = env::var_os("TERMINFO") {
			search.push(dir.into());
		} else {
			if let Some(mut home) = dirs::home_dir() {
				home.push(".terminfo");
				search.push(home.into());
			}
		}

		if let Ok(dirs) = env::var("TERMINFO_DIRS") {
			for dir in dirs.split(':') {
				search.push(dir.into());
			}
		}

		// handle non-FHS systems like Termux
		if let Ok(prefix) = env::var("PREFIX") {
			let path = Path::new(&prefix);
			search.push(path.join("etc/terminfo"));
			search.push(path.join("lib/terminfo"));
			search.push(path.join("share/terminfo"));
		}

		search.push("/etc/terminfo".into());
		search.push("/lib/terminfo".into());
		search.push("/usr/share/terminfo".into());
		search.push("/boot/system/data/terminfo".into());

		for path in search {
			if fs::metadata(&path).is_err() {
				continue;
			}

			// Check standard location.
			{
				let mut path = path.clone();
				path.push(first.to_string());
				path.push(name);

				if fs::metadata(&path).is_ok() {
					return Self::from_path(path);
				}
			}

			// Check non-standard location.
			{
				let mut path = path.clone();
				path.push(format!("{:x}", first as usize));
				path.push(name);

				if fs::metadata(&path).is_ok() {
					return Self::from_path(path);
				}
			}
		}

		Err(Error::NotFound)
	}

	/// Load a database from the given path.
	pub fn from_path<P: AsRef<Path>>(path: P) -> error::Result<Self> {
		let mut file = File::open(path)?;
		let mut buffer = Vec::new();
		file.read_to_end(&mut buffer)?;

		Self::from_buffer(buffer)
	}

	/// Load a database from a buffer.
	pub fn from_buffer<T: AsRef<[u8]>>(buffer: T) -> error::Result<Self> {
		if let Ok((_, database)) = compiled::parse(buffer.as_ref()) {
			Ok(database.into())
		}
		else {
			Err(Error::Parse)
		}
	}

	/// The terminal name.
	pub fn name(&self) -> &str {
		&self.name
	}

	/// The terminal aliases.
	pub fn aliases(&self) -> &[String] {
		&self.aliases
	}

	/// The terminal description.
	pub fn description(&self) -> &str {
		&self.description
	}

	/// Get a capability.
	///
	/// ## Example
	///
	/// ```
	/// use terminfo::{Database, capability as cap};
	///
	/// let info        = Database::from_env().unwrap();
	/// let colors: i32 = info.get::<cap::MaxColors>().unwrap().into();
	/// ```
	pub fn get<'a, C: Capability<'a>>(&'a self) -> Option<C> {
		C::from(self.inner.get(C::name()))
	}

	/// Get a capability by name.
	///
	/// ## Note
	///
	/// This interface only makes sense for extended capabilities since they
	/// don't have standardized types.
	///
	/// ## Example
	///
	/// ```
	/// use terminfo::Database;
	///
	/// let info      = Database::from_env().unwrap();
	/// let truecolor = info.raw("Tc").is_some();
	/// ```
	pub fn raw<S: AsRef<str>>(&self, name: S) -> Option<&Value> {
		let name = name.as_ref();
		let name = names::ALIASES.get(name).map(|s| *s).unwrap_or(name);

		self.inner.get(name)
	}
}
