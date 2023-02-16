/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::path::StarlarkPath;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneType;

use crate::interpreter::build_context::BuildContext;

#[derive(Debug, thiserror::Error)]
enum PackageFileError {
    #[error(
        "`package()` can only be called in `PACKAGE` files \
        or in `bzl` files included from `PACKAGE` files"
    )]
    NotPackage,
}

/// Globals for `PACKAGE` files and `bzl` files included from `PACKAGE` files.
#[starlark_module]
pub(crate) fn register_package_function(globals: &mut GlobalsBuilder) {
    fn package(
        #[starlark(require=named, default=false)] inherit: bool,
        #[starlark(require=named, default=Vec::new())] visibility: Vec<String>,
        #[starlark(require=named, default=Vec::new())] within_view: Vec<String>,
        eval: &mut Evaluator,
    ) -> anyhow::Result<NoneType> {
        let _ignore = (inherit, visibility, within_view);
        let build_context = BuildContext::from_context(eval)?;
        match build_context.starlark_path {
            StarlarkPath::PackageFile(_) => {}
            _ => return Err(PackageFileError::NotPackage.into()),
        };
        // TODO(nga): implement visibility.
        Ok(NoneType)
    }
}
