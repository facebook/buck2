/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::typing::Ty;
use starlark::values::starlark_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

/// Wrapper for `regex::Regex`.
#[derive(ProvidesStaticType, Debug, NoSerialize, Allocative)]
pub enum BuckStarlarkRegex {
    // TODO(nga): do not skip.
    //   And this is important because regex can have a lot of cache.
    Regular(#[allocative(skip)] regex::Regex),
    Fancy(#[allocative(skip)] fancy_regex::Regex),
}

impl BuckStarlarkRegex {
    pub fn as_str(&self) -> &str {
        match self {
            BuckStarlarkRegex::Regular(r) => r.as_str(),
            BuckStarlarkRegex::Fancy(r) => r.as_str(),
        }
    }

    fn is_match(&self, s: &str) -> buck2_error::Result<bool> {
        match self {
            BuckStarlarkRegex::Regular(r) => Ok(r.is_match(s)),
            BuckStarlarkRegex::Fancy(r) => Ok(r.is_match(s)?),
        }
    }

    pub fn replace_all<'a>(&self, haystack: &'a str, rep: &str) -> Cow<'a, str> {
        match self {
            BuckStarlarkRegex::Regular(r) => r.replace_all(haystack, rep),
            BuckStarlarkRegex::Fancy(r) => r.replace_all(haystack, rep),
        }
    }
}

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
        write!(f, "regex({:?})", self.as_str())
    }
}

starlark_simple_value!(BuckStarlarkRegex);

#[starlark_module]
fn regex_methods(builder: &mut MethodsBuilder) {
    fn r#match(
        this: &BuckStarlarkRegex,
        #[starlark(require = pos)] str: &str,
    ) -> starlark::Result<bool> {
        Ok(this.is_match(str)?)
    }
}

#[starlark_module]
pub fn register_buck_regex(builder: &mut GlobalsBuilder) {
    #[starlark(as_type = BuckStarlarkRegex)]
    fn regex<'v>(
        #[starlark(require = pos)] regex: &str,
        #[starlark(require = named, default = false)] fancy: bool,
    ) -> starlark::Result<BuckStarlarkRegex> {
        match fancy {
            false => Ok(BuckStarlarkRegex::Regular(
                regex::Regex::new(regex).map_err(buck2_error::Error::from)?,
            )),
            true => Ok(BuckStarlarkRegex::Fancy(
                fancy_regex::Regex::new(regex).map_err(buck2_error::Error::from)?,
            )),
        }
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
    fn test_fancy() {
        let mut a = Assert::new();
        a.globals_add(register_buck_regex);

        a.fail(r"regex('(?=x)')", "not supported");
        a.pass(r"regex('(?=x)', fancy=True)");

        a.is_true(r"regex('^(?=x)x$', fancy=True).match('x')");
    }

    #[test]
    fn test_as_type() {
        let mut a = Assert::new();
        a.globals_add(register_buck_regex);
        a.is_true("isinstance(regex('foo'), regex)");
        a.is_false("isinstance(1, regex)");
    }
}
