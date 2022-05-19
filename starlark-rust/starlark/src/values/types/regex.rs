/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! A type [`StarlarkRegex`] which wraps Rust value fancy_regex::Regex.
use std::fmt::{self, Display};

use fancy_regex::Regex;
use gazebo::any::AnyLifetime;

use crate as starlark;
use crate::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::StarlarkValue,
};

/// A type that can be passed around as a StarlarkRegex, which wraps Rust value
/// fancy_regex::Regex.
#[derive(AnyLifetime, Debug, NoSerialize)]
pub struct StarlarkRegex(pub Regex);

impl StarlarkValue<'_> for StarlarkRegex {
    starlark_type!("regex");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(regex_type_methods)
    }
}

impl Display for StarlarkRegex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "regex({:?})", &self.0.as_str())
    }
}

starlark_simple_value!(StarlarkRegex);
impl StarlarkRegex {
    /// Create a new [`StarlarkRegex`] value. Such a value can be allocated on a heap with
    /// `heap.alloc(StarlarkRegex::new(x))`.
    pub fn new(x: &str) -> anyhow::Result<Self> {
        Ok(Self(Regex::new(x)?))
    }
}

#[starlark_module]
fn regex_type_methods(builder: &mut MethodsBuilder) {
    fn r#match(this: &StarlarkRegex, ref str: &str) -> anyhow::Result<bool> {
        Ok(this.0.is_match(str)?)
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;

    #[test]
    fn test_match() {
        assert::all_true(
            r#"
regex("abc|def|ghi").match("abc")
not regex("abc|def|ghi").match("xyz")
not regex("^((?!abc).)*$").match("abc")
regex("^((?!abc).)*$").match("xyz")
"#,
        );
    }

    #[test]
    fn test_str() {
        assert::is_true(
            r#"
str(regex("foo")) == 'regex("foo")'
"#,
        );
    }
}
