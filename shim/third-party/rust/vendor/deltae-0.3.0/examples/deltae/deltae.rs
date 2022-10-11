use deltae::*;
use std::error::Error;
use std::str::FromStr;

mod cli;

fn main() -> Result<(), Box<dyn Error>> {
    //Parse command line arguments with clap
    let matches = cli::app().get_matches();

    let method = DEMethod::from_str(matches.value_of("METHOD").unwrap())?;
    let color_type = matches.value_of("COLORTYPE").unwrap();
    let color0 = matches.value_of("COLOR0").unwrap();
    let color1 = matches.value_of("COLOR1").unwrap();

    let delta = match color_type {
        "lab" => LabValue::from_str(color0)?.delta(LabValue::from_str(color1)?, method),
        "lch" => LchValue::from_str(color0)?.delta(LchValue::from_str(color1)?, method),
        "xyz" => XyzValue::from_str(color0)?.delta(XyzValue::from_str(color1)?, method),
        _ => unreachable!("COLORTYPE"),
    };

    println!("{}: {}", delta.method(), delta.value());

    Ok(())
}
