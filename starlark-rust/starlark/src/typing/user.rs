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

use std::cmp::Ordering;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use dupe::Dupe;
use starlark_map::sorted_map::SortedMap;

use crate::typing::custom::TyCustomImpl;
use crate::typing::Ty;
use crate::values::types::type_instance_id::TypeInstanceId;
use crate::values::typing::type_compiled::alloc::TypeMatcherAlloc;
use crate::values::typing::type_compiled::type_matcher_factory::TypeMatcherFactory;

/// Type description for arbitrary type.
#[derive(Allocative, Debug, derive_more::Display)]
#[display(fmt = "{}", name)]
pub(crate) struct TyUser {
    pub(crate) name: String,
    pub(crate) matcher: TypeMatcherFactory,
    pub(crate) id: TypeInstanceId,
    pub(crate) fields: SortedMap<String, Ty>,
}

impl PartialEq for TyUser {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TyUser {}

impl PartialOrd for TyUser {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TyUser {
    fn cmp(&self, other: &Self) -> Ordering {
        (&self.name, &self.fields, self.id).cmp(&(&other.name, &self.fields, other.id))
    }
}

impl Hash for TyUser {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.fields.hash(state);
    }
}

impl TyCustomImpl for TyUser {
    fn as_name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn attribute(&self, attr: &str) -> Result<Ty, ()> {
        match self.fields.get(attr) {
            Some(ty) => Ok(ty.dupe()),
            None => Err(()),
        }
    }

    fn matcher<T: TypeMatcherAlloc>(&self, factory: T) -> T::Result {
        factory.from_type_matcher_factory(&self.matcher)
    }

    fn intersects(x: &Self, y: &Self) -> bool {
        x == y
    }
}
