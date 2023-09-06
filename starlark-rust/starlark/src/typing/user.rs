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
use starlark_syntax::codemap::Span;
use starlark_syntax::codemap::Spanned;

use crate::typing::custom::TyCustomImpl;
use crate::typing::error::TypingOrInternalError;
use crate::typing::function::TyCustomFunctionImpl;
use crate::typing::starlark_value::TyStarlarkValue;
use crate::typing::Arg;
use crate::typing::Ty;
use crate::typing::TyBasic;
use crate::typing::TyFunction;
use crate::typing::TypingOracleCtx;
use crate::values::types::type_instance_id::TypeInstanceId;
use crate::values::typing::type_compiled::alloc::TypeMatcherAlloc;
use crate::values::typing::type_compiled::type_matcher_factory::TypeMatcherFactory;

#[derive(Debug, thiserror::Error)]
enum TyUserError {
    #[error(
        "Type `{0}` specifies custom callable, but underlying `StarlarkValue` is not callable"
    )]
    CallableNotCallable(String),
    #[error(
        "Type `{0}` specifies custom indexable, but underlying `StarlarkValue` is not indexable"
    )]
    IndexableNotIndexable(String),
    #[error(
        "Type `{0}` specifies custom iterable, but underlying `StarlarkValue` is not iterable"
    )]
    IterableNotIterable(String),
}

/// Types of `[]` operator.
#[derive(Allocative, Debug)]
pub struct TyUserIndex {
    /// Type of index argument.
    pub(crate) index: Ty,
    /// Type of result.
    pub(crate) result: Ty,
}

/// Fields of the struct.
#[derive(Allocative, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct TyUserFields {
    /// Known fields.
    pub known: SortedMap<String, Ty>,
    /// Are there unknown fields?
    /// Unknown fields are possible if this type represents an abstract type like a provider.
    pub unknown: bool,
}

impl TyUserFields {
    /// No fields.
    pub fn no_fields() -> TyUserFields {
        TyUserFields {
            known: SortedMap::new(),
            unknown: false,
        }
    }

    /// All fields are not known.
    pub fn unknown() -> TyUserFields {
        TyUserFields {
            known: SortedMap::new(),
            unknown: true,
        }
    }
}

/// Type description for arbitrary type.
#[derive(Allocative, Debug, derive_more::Display)]
#[display(fmt = "{}", name)]
pub struct TyUser {
    name: String,
    /// Base type for this custom type, e.g. generic record for record with known fields.
    base: TyStarlarkValue,
    matcher: Option<TypeMatcherFactory>,
    id: TypeInstanceId,
    fields: TyUserFields,
    /// Set if more precise callable signature is known than `base` provides.
    callable: Option<TyFunction>,
    /// Set if more precise index signature is known than `base` provides.
    index: Option<TyUserIndex>,
    /// Set if more precise iter item is known than `base` provides.
    iter_item: Option<Ty>,
}

impl TyUser {
    /// Constructor.
    pub fn new(
        name: String,
        base: TyStarlarkValue,
        matcher: Option<TypeMatcherFactory>,
        id: TypeInstanceId,
        fields: TyUserFields,
        callable: Option<TyFunction>,
        index: Option<TyUserIndex>,
        iter_item: Option<Ty>,
    ) -> anyhow::Result<TyUser> {
        if callable.is_some() {
            if !base.is_callable() {
                return Err(TyUserError::CallableNotCallable(name).into());
            }
        }
        if index.is_some() {
            if !base.is_indexable() {
                return Err(TyUserError::IndexableNotIndexable(name).into());
            }
        }
        if iter_item.is_some() {
            if base.iter_item().is_err() {
                return Err(TyUserError::IterableNotIterable(name).into());
            }
        }
        Ok(TyUser {
            name,
            base,
            matcher,
            id,
            fields,
            callable,
            index,
            iter_item,
        })
    }
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
        if let Ok(ty) = self.base.attr_from_methods(attr) {
            Ok(ty)
        } else {
            match self.fields.known.get(attr) {
                Some(ty) => Ok(ty.dupe()),
                None => {
                    if self.fields.unknown {
                        Ok(Ty::any())
                    } else {
                        Err(())
                    }
                }
            }
        }
    }

    fn index(&self, item: &TyBasic, ctx: &TypingOracleCtx) -> Result<Ty, ()> {
        if let Some(index) = &self.index {
            if !ctx.intersects(&Ty::basic(item.dupe()), &index.index) {
                return Err(());
            }
            Ok(index.result.dupe())
        } else {
            self.base.index(item)
        }
    }

    fn iter_item(&self) -> Result<Ty, ()> {
        if let Some(iter_item) = &self.iter_item {
            Ok(iter_item.dupe())
        } else {
            self.base.iter_item()
        }
    }

    fn is_callable(&self) -> bool {
        self.base.is_callable()
    }

    fn validate_call(
        &self,
        span: Span,
        args: &[Spanned<Arg>],
        oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingOrInternalError> {
        if let Some(callable) = &self.callable {
            callable.validate_call(span, args, oracle)
        } else {
            Ok(self.base.validate_call(span, oracle)?)
        }
    }

    fn matcher<T: TypeMatcherAlloc>(&self, factory: T) -> T::Result {
        match &self.matcher {
            Some(matcher) => factory.from_type_matcher_factory(matcher),
            None => factory.unreachable_cannot_appear_in_type_expr(),
        }
    }

    fn intersects(x: &Self, y: &Self) -> bool {
        x == y
    }
}
