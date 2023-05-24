/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[derive(Clone)]
pub struct Argv {
    pub argv: Vec<String>,
    pub expanded_argv: Vec<String>,
}

#[derive(Clone)]
pub struct SanitizedArgv {
    pub argv: Vec<String>,
    pub expanded_argv: Vec<String>,
}

impl Argv {
    pub fn no_need_to_sanitize(self) -> SanitizedArgv {
        let Argv {
            argv,
            expanded_argv,
        } = self;
        SanitizedArgv {
            argv,
            expanded_argv,
        }
    }
}
