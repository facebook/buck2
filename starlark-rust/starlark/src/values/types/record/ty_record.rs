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

use allocative::Allocative;
use dupe::Dupe;

use crate::typing::custom::TyCustomImpl;
use crate::typing::Ty;
use crate::values::record::ty_record_type::TyRecordType;
use crate::values::record::Record;
use crate::values::types::type_instance_id::TypeInstanceId;
use crate::values::typing::type_compiled::alloc::TypeMatcherAlloc;
use crate::values::typing::type_compiled::matcher::TypeMatcher;
use crate::values::Value;

/// Type of record instance i. e. type of `record()()`.
#[derive(
    Debug,
    Allocative,
    derive_more::Display,
    Clone,
    Dupe,
    Eq,
    PartialEq,
    Hash,
    Ord,
    PartialOrd
)]
#[display(fmt = "record(name = \"{}\", ...)", "record_type.data.name")]
pub(crate) struct TyRecord {
    pub(crate) record_type: TyRecordType,
}

impl TyCustomImpl for TyRecord {
    fn as_name(&self) -> Option<&str> {
        Some(&self.record_type.data.name)
    }

    fn attribute(&self, attr: &str) -> Result<Ty, ()> {
        match self.record_type.data.fields.get(attr) {
            Some(ty) => Ok(ty.dupe()),
            None => Err(()),
        }
    }

    fn matcher<T: TypeMatcherAlloc>(&self, factory: T) -> T::Result {
        #[derive(Hash, Debug, Eq, PartialEq, Clone, Dupe, Allocative)]
        struct RecordTypeMatcher {
            id: TypeInstanceId,
        }

        impl TypeMatcher for RecordTypeMatcher {
            fn matches(&self, value: Value) -> bool {
                match Record::from_value(value) {
                    None => false,
                    Some(record) => record.record_type_id() == self.id,
                }
            }
        }

        factory.alloc(RecordTypeMatcher {
            id: self.record_type.data.id,
        })
    }

    fn intersects(x: &Self, y: &Self) -> bool {
        x == y
    }
}
