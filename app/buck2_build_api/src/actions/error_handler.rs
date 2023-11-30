/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

use allocative::Allocative;
use derive_more::Display;
use display_container::fmt_container;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::starlark_value;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::StarlarkDocs;

#[derive(
    ProvidesStaticType,
    Trace,
    Allocative,
    StarlarkDocs,
    Debug,
    Display,
    NoSerialize,
    Clone
)]
#[display(
    fmt = "ActionErrorCtx(stderr: {}, stdout: {})",
    "self.stderr",
    "self.stdout"
)]
pub struct StarlarkActionErrorContext {
    stderr: String,
    stdout: String,
}

starlark_simple_value!(StarlarkActionErrorContext);

#[starlark_value(type = "ActionErrorCtx")]
impl<'v> StarlarkValue<'v> for StarlarkActionErrorContext {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(action_error_context_methods)
    }
}

/// Methods available on `ActionErrorCtx` to help categorize the action failure.
#[starlark_module]
fn action_error_context_methods(builder: &mut MethodsBuilder) {
    /// The stderr of the failed action.
    #[starlark(attribute)]
    fn stderr<'v>(this: &'v StarlarkActionErrorContext) -> anyhow::Result<&'v str> {
        Ok(&this.stderr)
    }

    /// The stdout of the failed action.
    #[starlark(attribute)]
    fn stdout<'v>(this: &'v StarlarkActionErrorContext) -> anyhow::Result<&'v str> {
        Ok(&this.stdout)
    }

    /// Create a new error location, specifying a file name and an optional line number
    fn new_error_location<'v>(
        #[starlark(this)] _this: &'v StarlarkActionErrorContext,
        #[starlark(require = named)] file: String,
        #[starlark(require = named)] line: Option<u64>,
    ) -> anyhow::Result<StarlarkActionErrorLocation> {
        Ok(StarlarkActionErrorLocation { file, line })
    }

    /// Create a new sub error, specifying an error category name, optional message, and
    /// an optional list of error locations.
    fn new_sub_error<'v>(
        #[starlark(this)] _this: &'v StarlarkActionErrorContext,
        #[starlark(require = named)] category: String,
        #[starlark(require = named)] message: Option<String>,
        #[starlark(require = named)] locations: Option<
            UnpackListOrTuple<&'v StarlarkActionErrorLocation>,
        >,
    ) -> anyhow::Result<StarlarkActionSubError<'v>> {
        Ok(StarlarkActionSubError {
            category,
            message,
            locations,
        })
    }
}

#[derive(
    ProvidesStaticType,
    Trace,
    Allocative,
    StarlarkDocs,
    Debug,
    Display,
    NoSerialize,
    Clone,
    Default
)]
#[display(
    fmt = "ActionErrorLocation(file={}, line={})",
    "self.file",
    "self.line.map_or(\"None\".to_owned(), |l| l.to_string())"
)]
pub struct StarlarkActionErrorLocation {
    file: String,
    line: Option<u64>,
}

#[starlark_value(type = "ActionErrorLocation", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkActionErrorLocation {}

impl<'v> AllocValue<'v> for StarlarkActionErrorLocation {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

#[derive(
    ProvidesStaticType,
    Trace,
    Allocative,
    StarlarkDocs,
    Debug,
    NoSerialize,
    Clone
)]
pub(crate) struct StarlarkActionSubError<'v> {
    category: String,
    message: Option<String>,
    #[allocative(skip)]
    #[trace(unsafe_ignore)]
    locations: Option<UnpackListOrTuple<&'v StarlarkActionErrorLocation>>,
}

impl<'v> Display for StarlarkActionSubError<'v> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = format!(
            "ActionSubError(category={}, message={}, locations=[",
            self.category,
            self.message.clone().unwrap_or_default()
        );
        fmt_container(
            f,
            &prefix,
            "])",
            self.locations
                .as_ref()
                .map_or(Vec::new(), |l| l.items.iter().collect()),
        )
    }
}

impl<'v> AllocValue<'v> for StarlarkActionSubError<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

#[starlark_value(type = "ActionSubError", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkActionSubError<'v> {}
