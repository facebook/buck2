[![crates.io](https://img.shields.io/crates/v/output_vt100.svg?style=flat)](https://crates.io/crates/output_vt100)
[![Released API docs](https://docs.rs/output_vt100/badge.svg)](https://docs.rs/output_vt100)
[![Downloads](https://img.shields.io/crates/d/output_vt100.svg?style=flat)](https://crates.io/crates/output_vt100)
[![MIT Licensed](https://img.shields.io/crates/l/output_vt100.svg?style=flat)](https://crates.io/crates/output_vt100)
[![AppVeyor CI](https://img.shields.io/appveyor/ci/Phundrak/output-vt100-rs.svg?style=flat)](https://ci.appveyor.com/project/Phundrak/output-vt100-rs)
[![Build Status](https://drone.phundrak.com/api/badges/phundrak/output-vt100-rs/status.svg)](https://drone.phundrak.com/phundrak/output-vt100-rs)

# Output-VT100

This simple crates allows developers to enable ANSI escape characters in Windows' console, be it CMD or PowerShell. Its usage is very simple, as shown below:

```rust
extern crate output_vt100;

fn main() {
    output_vt100::init();
    println!("\x1b[31mThis text is red!\x1b[0m");
}
```

If you wish to ensure the `output_vt100::init()` function is only ran once, you can use the crate [ctor](https://crates.io/crates/ctor). Be aware though it might not be suited for every use case, as explained on the crate’s presentation.

```rust
extern crate output_vt100;
extern crate ctor;
use ctor::*;

#[ctor]
fn init_term() {
    output_vt100::init();
}

fn main() {
    println!("\x1b[31mThis text is red!\x1b[0m");
}
```

Not  that init  panics on  error, if  you do  not wish  to panic,  use
`output_vt100::try_init` which returns a `Result<(), ()>`

# Acknowledgements

A big thank you to [nbouteme](https://github.com/nbouteme) who helped me a lot during the development of this create.
