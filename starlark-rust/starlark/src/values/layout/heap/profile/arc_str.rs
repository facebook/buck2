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
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;
use std::sync::Arc;

use gazebo::dupe::Dupe;

#[derive(Clone, Dupe, Debug)]
enum Inner {
    Arc(Arc<str>),
    Static(&'static str),
}

/// Wrapper for `Arc<str>`.
#[derive(Clone, Dupe, Debug, derive_more::Display)]
#[display(fmt = "{}", "&**self")]
pub(crate) struct ArcStr(Inner);

impl ArcStr {
    pub(crate) fn new_static(s: &'static str) -> ArcStr {
        ArcStr(Inner::Static(s))
    }

    pub(crate) fn as_str(&self) -> &str {
        match &self.0 {
            Inner::Arc(s) => s,
            Inner::Static(s) => s,
        }
    }
}

impl PartialEq for ArcStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for ArcStr {}

impl Hash for ArcStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl Deref for ArcStr {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl Borrow<str> for ArcStr {
    fn borrow(&self) -> &str {
        self
    }
}

impl<'a> From<&'a str> for ArcStr {
    fn from(s: &'a str) -> Self {
        if s.is_empty() {
            ArcStr(Inner::Static(""))
        } else {
            ArcStr(Inner::Arc(Arc::from(s)))
        }
    }
}
