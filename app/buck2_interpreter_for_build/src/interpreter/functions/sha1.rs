/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use sha1::Digest;
use sha1::Sha1;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;

/// Contains functions that we include in all contexts.
#[starlark_module]
pub(crate) fn register_sha1(builder: &mut GlobalsBuilder) {
    /// Computes a sha1 digest for a string. Returns the hex representation of the digest.
    ///
    /// ```python
    /// sha1("Buck2 is the best build system") == "d39e9f9030da819a5be667a409ea979551df6211"
    /// ```
    fn sha1(#[starlark(require = pos)] val: &str) -> starlark::Result<String> {
        let hash = Sha1::digest(val.as_bytes());
        Ok(hex::encode(hash))
    }
}

#[cfg(test)]
mod tests {

    use starlark::assert::Assert;

    use crate::interpreter::functions::sha1::register_sha1;

    #[test]
    fn test_sha1() {
        let mut a = Assert::new();
        a.globals_add(register_sha1);
        a.eq("sha1('123')", "'40bd001563085fc35165329ea1ff5c5ecbdbbeef'");
    }
}
