/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;

#[linkme::distributed_slice]
pub static DOWNSTREAM_CRATE_STARLARK_GLOBALS: [fn(&mut GlobalsBuilder)] = [..];

pub fn register_downstream_crate_starlark_globals(globals: &mut GlobalsBuilder) {
    for f in DOWNSTREAM_CRATE_STARLARK_GLOBALS {
        f(globals);
    }
}

#[doc(hidden)]
pub mod __macro_refs {
    pub use linkme;
    pub use starlark;
}

#[macro_export]
macro_rules! link_buck2_downstream_crate_starlark_globals {
    ($globals:ident) => {
        #[$crate::downstream_crate_starlark_globals::__macro_refs::linkme::distributed_slice(
            $crate::downstream_crate_starlark_globals::DOWNSTREAM_CRATE_STARLARK_GLOBALS
        )]
        #[linkme(crate = $crate::downstream_crate_starlark_globals::__macro_refs::linkme)]
        static LINK_STARLARK_GLOBALS: fn(&mut $crate::downstream_crate_starlark_globals::__macro_refs::starlark::environment::GlobalsBuilder) = $globals;
    };
}

pub use link_buck2_downstream_crate_starlark_globals;
