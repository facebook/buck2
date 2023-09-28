/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::typing::Ty;
use starlark::values::starlark_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

/// Wrapper for `regex::Regex`.
#[derive(ProvidesStaticType, Debug, NoSerialize, StarlarkDocs, Allocative)]
pub struct BuckStarlarkRegex(
    // TODO(nga): do not skip.
    //   And this is important because regex can have a lot of cache.
    #[allocative(skip)] pub regex::Regex,
);

#[starlark_value(type = "buck_regex")] // "regex" is used for "experimental_regex" in starlark-rust.
impl<'v> StarlarkValue<'v> for BuckStarlarkRegex {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(regex_methods)
    }

    fn typechecker_ty(&self) -> Option<Ty> {
        Some(Ty::starlark_value::<Self>())
    }
}

impl Display for BuckStarlarkRegex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // TODO(nga): should use starlark string repr.
        write!(f, "regex({:?})", &self.0.as_str())
    }
}

starlark_simple_value!(BuckStarlarkRegex);

#[starlark_module]
fn regex_methods(builder: &mut MethodsBuilder) {
    fn r#match(
        this: &BuckStarlarkRegex,
        #[starlark(require = pos)] str: &str,
    ) -> anyhow::Result<bool> {
        Ok(this.0.is_match(str))
    }
}

#[starlark_module]
pub fn register_buck_regex(builder: &mut GlobalsBuilder) {
    #[starlark(as_type = BuckStarlarkRegex)]
    fn regex<'v>(#[starlark(require = pos)] regex: &str) -> anyhow::Result<BuckStarlarkRegex> {
        Ok(BuckStarlarkRegex(regex::Regex::new(regex)?))
    }
}

#[cfg(test)]
mod tests {
    use starlark::assert::Assert;

    use crate::types::regex::register_buck_regex;

    #[test]
    fn test_match() {
        let mut a = Assert::new();
        a.globals_add(register_buck_regex);

        a.is_true("regex('abc|def|ghi').match('abc')");
        a.is_true("regex('x').match('aaaxbbb')");
    }

    #[test]
    fn test_str() {
        let mut a = Assert::new();
        a.globals_add(register_buck_regex);
        a.is_true(
            r#"
str(regex("foo")) == 'regex("foo")'
"#,
        );
    }

    #[test]
    fn test_as_type() {
        let mut a = Assert::new();
        a.globals_add(register_buck_regex);
        a.is_true("isinstance(regex('foo'), regex)");
        a.is_false("isinstance(1, regex)");
    }
}
