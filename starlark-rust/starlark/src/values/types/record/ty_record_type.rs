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

use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use starlark_map::sorted_map::SortedMap;

use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::typing::custom::TyCustomImpl;
use crate::typing::error::TypingOrInternalError;
use crate::typing::oracle::traits::TypingAttr;
use crate::typing::Arg;
use crate::typing::Ty;
use crate::typing::TypingOracleCtx;
use crate::values::record::record_type::RecordType;
use crate::values::record::ty_record::TyRecord;
use crate::values::types::type_instance_id::TypeInstanceId;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::typing::type_compiled::factory::TypeCompiledFactory;
use crate::values::StarlarkValue;
use crate::values::Value;

#[derive(Allocative, Ord, PartialOrd, Debug)]
pub(crate) struct TyRecordData {
    /// Name of the record type.
    pub(crate) name: String,
    /// Types of fields.
    pub(crate) fields: SortedMap<String, Ty>,
    /// Globally unique id of the record type.
    // Id must be last so `Ord` is deterministic.
    pub(crate) id: TypeInstanceId,
}

impl PartialEq for TyRecordData {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TyRecordData {}

impl Hash for TyRecordData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Do not hash `id` because hashing should be deterministic.
        self.name.hash(state);
        self.fields.hash(state);
    }
}

/// Type of record type instance, i.e. type of `record()`.
#[derive(
    Clone,
    Dupe,
    Allocative,
    Ord,
    PartialOrd,
    Eq,
    PartialEq,
    Hash,
    derive_more::Display,
    Debug
)]
#[display(fmt = "record[name = \"{}\"]", "self.data.name")]
pub struct TyRecordType {
    /// This is `Arc` so `TyRecord` could grab `TyRecordType`.
    pub(crate) data: Arc<TyRecordData>,
}

impl TyRecordType {
    pub(crate) fn instance_ty(&self) -> Ty {
        Ty::custom(TyRecord {
            record_type: self.dupe(),
        })
    }
}

impl TyCustomImpl for TyRecordType {
    fn as_name(&self) -> Option<&str> {
        Some(RecordType::TYPE)
    }

    fn attribute(&self, attr: TypingAttr) -> Result<Ty, ()> {
        match attr {
            TypingAttr::Regular("type") => Ok(Ty::string()),
            _ => Err(()),
        }
    }

    fn validate_call(
        &self,
        _span: Span,
        _args: &[Spanned<Arg>],
        _oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingOrInternalError> {
        // TODO(nga): better checks.
        Ok(self.instance_ty())
    }

    fn is_callable(&self) -> bool {
        true
    }

    fn matcher<'v>(&self, factory: TypeCompiledFactory<'v>) -> TypeCompiled<Value<'v>> {
        let _ignore = factory;
        // TODO(nga): replace panic with error.
        unreachable!("Cannot apprear in type expressions")
    }

    fn intersects(x: &Self, y: &Self) -> bool {
        x == y
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;

    #[test]
    fn test_good() {
        assert::pass(
            r#"
MyRec = record(x = int)

def foo(x: MyRec): pass

foo(MyRec(x = 1))
        "#,
        );
    }

    #[test]
    fn test_fail_compile_time() {
        assert::fail(
            r#"
MyRec = record(x = int)
WrongRec = record(x = int)

def foo(x: MyRec): pass

def bar():
    foo(WrongRec(x = 1))
        "#,
            r#"Expected type `record(name = "MyRec", ...)` but got `record(name = "WrongRec", ...)`"#,
        );
    }

    #[test]
    fn test_fail_runtime_time() {
        assert::fail_skip_typecheck(
            r#"
MyRec = record(x = int)
WrongRec = record(x = int)

def foo(x: MyRec): pass

foo(WrongRec(x = 1))
        "#,
            r#"Value `record(x=1)` of type `record` does not match the type annotation `record(name = "MyRec","#,
        );
    }

    #[test]
    fn test_record_instance_typechecker_ty() {
        assert::pass(
            r#"
MyRec = record(x = int)
X = MyRec(x = 1)

def foo() -> MyRec:
    # This fails if record instance does not override `typechecker_ty`.
    return X
"#,
        );
    }

    #[test]
    fn test_typecheck_field_pass() {
        assert::pass(
            r#"
MyRec = record(x = int, y = int)

def f(rec: MyRec) -> int:
    return rec.x + rec.y

assert_eq(f(MyRec(x = 1, y = 2)), 3)
"#,
        );
    }

    #[test]
    fn test_typecheck_field_fail() {
        assert::fail(
            r#"
MyRec = record(x = int, y = int)

def f(rec: MyRec) -> int:
    return rec.z
"#,
            r#"The attribute `z` is not available on the type `record(name = "MyRec"#,
        );
    }
}
