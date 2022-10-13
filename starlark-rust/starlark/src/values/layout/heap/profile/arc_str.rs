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

use std::borrow::Borrow;
use std::ops::Deref;
use std::sync::Arc;

use gazebo::dupe::Dupe;

/// Wrapper for `Arc<str>`.
#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash, derive_more::Display)]
#[display(fmt = "{}", _0)]
pub(crate) struct ArcStr(Arc<str>);

impl Deref for ArcStr {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for ArcStr {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl<'a> From<&'a str> for ArcStr {
    fn from(s: &'a str) -> Self {
        ArcStr(s.into())
    }
}
