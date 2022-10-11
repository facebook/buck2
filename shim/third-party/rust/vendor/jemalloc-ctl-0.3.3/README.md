# jemalloc-ctl

[![Travis-CI Status]][travis] [![Appveyor Status]][appveyor] [![Latest Version]][crates.io] [![docs]][docs.rs]

> A safe wrapper over `jemalloc`'s `mallctl*()` control and introspection APIs.

## Documentation

* [Latest release (docs.rs)][docs.rs]
* [master branch`][master_docs]

## Platform support

Supported on all platforms supported by the [`jemallocator`] crate.

## Example

```no_run
extern crate jemallocator;
extern crate jemalloc_ctl;

use std::thread;
use std::time::Duration;
use jemalloc_ctl::{stats, epoch};

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

fn main() {
    // Obtain a MIB for the `epoch`, `stats.allocated`, and
    // `atats.resident` keys:
    let e = epoch::mib().unwrap();
    let allocated = stats::allocated::mib().unwrap();
    let resident = stats::resident::mib().unwrap();
    
    loop {
        // Many statistics are cached and only updated 
        // when the epoch is advanced:
        e.advance().unwrap();
        
        // Read statistics using MIB key:
        let allocated = allocated.read().unwrap();
        let resident = resident.read().unwrap();
        println!("{} bytes allocated/{} bytes resident", allocated, resident);
        thread::sleep(Duration::from_secs(10));
    }
}
```

## License

This project is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in `jemalloc-ctl` by you, as defined in the Apache-2.0 license,
shall be dual licensed as above, without any additional terms or conditions.

[`jemallocator`]: https://github.com/gnzlbg/jemallocator
[travis]: https://travis-ci.org/gnzlbg/jemallocator
[Travis-CI Status]: https://travis-ci.org/gnzlbg/jemallocator.svg?branch=master
[appveyor]: https://ci.appveyor.com/project/gnzlbg/jemallocator/branch/master
[Appveyor Status]: https://ci.appveyor.com/api/projects/status/github/gnzlbg/jemallocator?branch=master&svg=true
[Latest Version]: https://img.shields.io/crates/v/jemalloc-ctl.svg
[crates.io]: https://crates.io/crates/jemalloc-ctl
[docs]: https://docs.rs/jemalloc-ctl/badge.svg
[docs.rs]: https://docs.rs/jemalloc-ctl/
[master_docs]: https://gnzlbg.github.io/jemallocator/jemalloc-ctl
