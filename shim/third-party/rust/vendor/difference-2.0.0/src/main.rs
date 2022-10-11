#![deny(warnings)]

#[cfg(feature = "bin")]
extern crate difference;
#[cfg(feature = "bin")]
extern crate getopts;

#[cfg(feature = "bin")]
use getopts::Options;
#[cfg(feature = "bin")]
use std::env;

#[cfg(not(feature = "bin"))]
fn main() {
    panic!("Needs to be compiled with --features=bin");
}

#[cfg(feature = "bin")]
fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optopt("s", "split", "", "char|word|line");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string()),
    };

    let split = match matches.opt_str("s") {
        Some(ref x) if x == "char" => "",
        Some(ref x) if x == "word" => " ",
        Some(ref x) if x == "line" => "\n",
        _ => " ",
    };

    if matches.free.len() > 1 {
        let ch = difference::Changeset::new(&matches.free[0], &matches.free[1], split);
        println!("{}", ch);
    } else {
        print!("{}", opts.usage(&format!("Usage: {} [options]", program)));
        return;
    };


}
