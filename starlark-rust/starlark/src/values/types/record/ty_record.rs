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
use crate::typing::TypingAttr;
use crate::values::record::ty_record_type::TyRecordType;
use crate::values::record::Record;
use crate::values::types::type_instance_id::TypeInstanceId;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::typing::type_compiled::compiled::TypeCompiledImpl;
use crate::values::typing::type_compiled::factory::TypeCompiledFactory;
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

    fn attribute(&self, attr: TypingAttr) -> Result<Ty, ()> {
        match attr {
            TypingAttr::Regular(name) => match self.record_type.data.fields.get(name) {
                Some(ty) => Ok(ty.dupe()),
                None => Err(()),
            },
            _ => Err(()),
        }
    }

    fn matcher<'v>(&self, factory: TypeCompiledFactory<'v>) -> TypeCompiled<Value<'v>> {
        #[derive(Hash, Debug, Eq, PartialEq, Clone, Dupe, Allocative)]
        struct RecordTypeMatcher {
            id: TypeInstanceId,
        }

        impl TypeCompiledImpl for RecordTypeMatcher {
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
