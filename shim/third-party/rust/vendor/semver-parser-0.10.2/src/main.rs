use semver_parser::Compat;
use semver_parser::RangeSet;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    // default operation
    let range_set: RangeSet = "1.2.3".parse()?;
    println!("Found range set: {:?}", range_set);

    // npm compatibility
    let range_set = RangeSet::parse("1.2.3", Compat::Npm)?;
    println!("Found range set (node): {:?}", range_set);

    Ok(())
}
