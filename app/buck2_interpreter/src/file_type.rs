/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use dupe::Dupe;
use starlark::syntax::Dialect;
use starlark::syntax::DialectTypes;

#[derive(Copy, Clone, Dupe, Debug, Eq, PartialEq, Hash)]
pub enum StarlarkFileType {
    Bzl,
    Bxl,
    Buck,
    Package,
}

/// What type of file are we parsing - a `.bzl` file, `.bxl` file, or a `BUCK`/`TARGETS` file.
impl StarlarkFileType {
    pub fn dialect(&self, disable_starlark_types: bool) -> Dialect {
        let buck_dialect: Dialect = Dialect {
            enable_def: false,
            enable_lambda: true,
            enable_load: true,
            enable_keyword_only_arguments: false,
            enable_types: DialectTypes::Disable,
            enable_tabs: false,
            enable_load_reexport: false,
            enable_top_level_stmt: false,
        };
        let package_dialect: Dialect = Dialect {
            enable_def: false,
            enable_lambda: true,
            enable_load: true,
            enable_keyword_only_arguments: false,
            enable_types: DialectTypes::Disable,
            enable_tabs: false,
            enable_load_reexport: false,
            enable_top_level_stmt: false,
        };
        let bzl_dialect: Dialect = Dialect {
            enable_def: true,
            enable_lambda: true,
            enable_load: true,
            enable_keyword_only_arguments: true,
            enable_types: if disable_starlark_types {
                DialectTypes::ParseOnly
            } else {
                DialectTypes::Enable
            },
            enable_tabs: false,
            enable_load_reexport: false,
            enable_top_level_stmt: true,
        };
        let bxl_dialect: Dialect = Dialect {
            enable_def: true,
            enable_lambda: true,
            enable_load: true,
            enable_keyword_only_arguments: true,
            enable_types: if disable_starlark_types {
                DialectTypes::ParseOnly
            } else {
                DialectTypes::Enable
            },
            enable_tabs: false,
            enable_load_reexport: false,
            enable_top_level_stmt: true,
        };

        match self {
            Self::Bzl => bzl_dialect,
            Self::Buck => buck_dialect,
            Self::Package => package_dialect,
            Self::Bxl => bxl_dialect,
        }
    }
}
