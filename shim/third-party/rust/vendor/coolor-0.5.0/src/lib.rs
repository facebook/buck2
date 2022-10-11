/*!

Definition of ANSI, RGB and HSL color types and all the conversions between them.

There are many other color conversion crates.
This one may be useful when you're interested into

- variations of an ANSI color for your TUI application, for example fading, lightening, darkening, with compatibility with terminals that don't support RGB.
- translations of color schemes
- automatic downgrading of RGB color schemes for non RGB terminals
- automated building of harmonious color schemes with guarantees of contrast
- etc.

Be warned that the ANSI range is limited and that not all intuitive operations will give good results.

The included example shows luminosity and saturation variants of all 240 ANSI colors, with all variants still ANSI colors.

*/

#![no_std]

mod ansi;
mod color;
mod error;
mod hsl;
mod rgb;

pub use {ansi::*, color::*, error::*, hsl::*, rgb::*};
