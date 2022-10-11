extern crate terminfo;

use std::io;
use terminfo::{Database, capability as cap};

fn main() {
  let info = Database::from_env().unwrap();

  if let Some(cap::MaxColors(n)) = info.get::<cap::MaxColors>() {
    println!("The terminal supports {} colors.", n);
  }
  else {
    println!("The terminal does not support colors, what year is this?");
  }

  if let Some(flash) = info.get::<cap::FlashScreen>() {
		flash.expand().to(io::stdout()).unwrap();
  }
	else {
		println!("FLASH GORDON!");
	}

	info.get::<cap::SetAForeground>().unwrap().expand().color(2).to(io::stdout()).unwrap();
	info.get::<cap::SetABackground>().unwrap().expand().color(4).to(io::stdout()).unwrap();
	println!("SUP");
	info.get::<cap::ExitAttributeMode>().unwrap().expand().to(io::stdout()).unwrap();
}
