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

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use dupe::IterDupedExt;
use either::Either;
use serde::Serialize;
use serde::Serializer;

use crate::docs::DocFunction;
use crate::docs::DocMember;
use crate::docs::DocParam;
use crate::docs::DocProperty;
use crate::eval::compiler::small_vec_1::SmallVec1;
use crate::typing::arc_ty::ArcTy;
use crate::typing::basic::TyBasic;
use crate::typing::custom::TyCustom;
use crate::typing::custom::TyCustomImpl;
use crate::typing::function::Param;
use crate::typing::function::ParamMode;
use crate::typing::function::TyCustomFunction;
use crate::typing::function::TyCustomFunctionImpl;
use crate::typing::function::TyFunction;
use crate::typing::small_arc_vec::SmallArcVec1;
use crate::typing::structs::TyStruct;
use crate::typing::tuple::TyTuple;
use crate::values::bool::StarlarkBool;
use crate::values::layout::heap::profile::arc_str::ArcStr;
use crate::values::tuple::value::FrozenTuple;
use crate::values::typing::never::TypingNever;
use crate::values::StarlarkValue;
use crate::values::Value;

/// A typing operation wasn't able to produce a precise result,
/// so made some kind of approximation.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Approximation {
    /// The category of the approximation, e.g. `"Unknown type"`.
    pub category: &'static str,
    /// The precise details of this approximation, e.g. which type was unknown.
    pub message: String,
}

impl Approximation {
    /// Create a new [`Approximation`].
    pub fn new(category: &'static str, message: impl Debug) -> Self {
        Self {
            category,
            message: format!("{:?}", message),
        }
    }
}

impl Display for Approximation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Approximation: {} = {:?}", self.category, self.message)
    }
}

/// A Starlark type.
#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative)]
pub struct Ty {
    /// A series of alternative types.
    ///
    /// When typechecking, we try all alternatives, and if at least one of them
    /// succeeds, then the whole expression is considered to be a success.
    ///
    /// For example, when typechecking:
    ///
    /// ```python
    /// x = ... # string or int
    /// y = ... # string
    /// x + y   # `int + string` fails, but `string + string` succeeds,
    ///         # so the whole expression is typechecked successfully as `string`
    /// ```
    ///
    /// This is different handling of union types than in TypeScript for example,
    /// TypeScript would consider such expression to be an error.
    alternatives: SmallArcVec1<TyBasic>,
}

impl Serialize for Ty {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Arbitrary custom types are not deserializable, so serialization to string is enough.
        serializer.serialize_str(&self.to_string())
    }
}

/// The name of an atomic type.
#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative)]
pub struct TyName(ArcStr);

impl Display for TyName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.0.as_str())
    }
}

impl PartialEq<str> for TyName {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl TyName {
    pub(crate) fn new(s: impl Into<ArcStr>) -> TyName {
        TyName(s.into())
    }

    pub(crate) fn new_static(s: &'static str) -> TyName {
        TyName(ArcStr::new_static(s))
    }

    /// Get the underlying `str` for a `TyName`.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

fn merge_adjacent<T>(xs: Vec<T>, f: impl Fn(T, T) -> Either<T, (T, T)>) -> SmallVec1<T> {
    let mut res = SmallVec1::new();
    let mut last = None;
    for x in xs {
        match last {
            None => last = Some(x),
            Some(l) => match f(l, x) {
                Either::Left(x) => last = Some(x),
                Either::Right((l, x)) => {
                    res.push(l);
                    last = Some(x)
                }
            },
        }
    }
    if let Some(l) = last {
        res.push(l)
    }
    res
}

impl Ty {
    /// Create a [`Ty::any`], but tagged in such a way it can easily be found.
    pub fn todo() -> Self {
        Ty::any()
    }

    fn try_name_special(name: &str) -> Option<Self> {
        match name {
            "list" => Some(Self::list(Ty::any())),
            "dict" => Some(Self::dict(Ty::any(), Ty::any())),
            "function" => Some(Self::any_function()),
            "struct" => Some(Self::custom(TyStruct::any())),
            "never" => Some(Self::never()),
            "NoneType" => Some(Self::none()),
            "bool" => Some(Self::bool()),
            "int" => Some(Self::int()),
            "float" => Some(Self::float()),
            "string" => Some(Self::string()),
            "tuple" => Some(Self::any_tuple()),
            // Note that "tuple" cannot be converted to Ty::Tuple
            // since we don't know the length of the tuple.
            _ => None,
        }
    }

    /// Create a [`Ty::Name`], or one of the standard functions.
    pub(crate) fn name_static(name: &'static str) -> Self {
        if let Some(x) = Self::try_name_special(name) {
            x
        } else {
            Ty::basic(TyBasic::Name(TyName::new_static(name)))
        }
    }

    pub(crate) fn name(name: &str) -> Self {
        if let Some(x) = Self::try_name_special(name) {
            x
        } else {
            Ty::basic(TyBasic::Name(TyName::new(name)))
        }
    }

    /// Create a named type.
    ///
    /// This function is deprecated because types should be migrated to either
    /// types based on `StarlarkValue` or `TyCustom`.
    pub fn name_deprecated(name: &str) -> Self {
        Self::name(name)
    }

    /// Turn a type back into a name, potentially erasing some structure.
    /// E.g. the type `[bool]` would return `list`.
    /// Types like [`Ty::any`] will return `None`.
    pub fn as_name(&self) -> Option<&str> {
        match self.alternatives.as_slice() {
            [x] => x.as_name(),
            _ => None,
        }
    }

    pub(crate) const fn basic(basic: TyBasic) -> Self {
        Ty {
            alternatives: SmallArcVec1::one(basic),
        }
    }

    /// "any" type: can hold any value.
    pub const fn any() -> Self {
        Ty::basic(TyBasic::Any)
    }

    pub(crate) const fn never() -> Self {
        Ty {
            alternatives: SmallArcVec1::empty(),
        }
    }

    /// Create a `None` type.
    pub const fn none() -> Self {
        Ty::basic(TyBasic::none())
    }

    /// Create a boolean type.
    pub const fn bool() -> Self {
        Ty::starlark_value::<StarlarkBool>()
    }

    /// Create the int type.
    pub const fn int() -> Self {
        Ty::basic(TyBasic::int())
    }

    /// Create a float type.
    pub const fn float() -> Self {
        Ty::basic(TyBasic::float())
    }

    /// Create a string type.
    pub const fn string() -> Self {
        Ty::basic(TyBasic::string())
    }

    /// Create a list type.
    pub fn list(element: Ty) -> Self {
        Ty::basic(TyBasic::list(element))
    }

    pub(crate) fn any_list() -> Self {
        Self::list(Ty::any())
    }

    /// Create a iterable type.
    pub fn iter(item: Ty) -> Self {
        Ty::basic(TyBasic::iter(item))
    }

    /// Create a dictionary type.
    pub fn dict(key: Ty, value: Ty) -> Self {
        Ty::basic(TyBasic::dict(key, value))
    }

    pub(crate) fn any_dict() -> Self {
        Self::dict(Ty::any(), Ty::any())
    }

    /// Create a tuple of two elements
    pub fn tuple2(a: Ty, b: Ty) -> Self {
        Ty::tuple(vec![a, b])
    }

    /// Create a tuple of given elements.
    pub fn tuple(elems: Vec<Ty>) -> Self {
        Ty::basic(TyBasic::Tuple(TyTuple {
            elems: elems.into(),
        }))
    }

    /// Tuple where elements are unknown.
    pub(crate) fn any_tuple() -> Self {
        Ty::starlark_value::<FrozenTuple>()
    }

    /// Create a function type.
    pub fn function(params: Vec<Param>, result: Ty) -> Self {
        Self::ty_function(TyFunction::new(params, result))
    }

    /// Create a function type.
    pub fn ty_function(f: TyFunction) -> Self {
        Self::custom(TyCustomFunction(f))
    }

    /// Create a function, where the first argument is the result of `.type`.
    pub fn ctor_function(type_attr: &Ty, params: Vec<Param>, result: Ty) -> Self {
        Self::custom(TyCustomFunction(TyFunction::new_with_type_attr(
            params,
            result,
            type_attr.clone(),
        )))
    }

    /// Function type that accepts any arguments and returns any result.
    pub(crate) fn any_function() -> Self {
        Ty::basic(TyBasic::Callable)
    }

    /// Type from the implementation of [`StarlarkValue`].
    pub const fn starlark_value<'v, T: StarlarkValue<'v>>() -> Self {
        Ty::basic(TyBasic::starlark_value::<T>())
    }

    pub(crate) fn is_any(&self) -> bool {
        self == &Ty::any()
    }

    pub(crate) fn is_never(&self) -> bool {
        self.alternatives.is_empty()
    }

    pub(crate) fn is_list(&self) -> bool {
        matches!(self.alternatives.as_slice(), [TyBasic::List(_)])
    }

    pub(crate) fn is_function(&self) -> bool {
        self.as_name() == Some("function")
    }

    /// Create a unions type, which will be normalised before being created.
    pub fn unions(xs: Vec<Self>) -> Self {
        // Handle common cases first.

        if xs.iter().any(|x| x.is_any()) {
            return Ty::any();
        }

        fn next_skip_never<I: Iterator<Item = Ty>>(iter: &mut I) -> Option<Ty> {
            iter.find(|x| !x.is_never())
        }
        let mut xs = xs.into_iter();
        let Some(x0) = next_skip_never(&mut xs) else {
            return Ty::never();
        };
        let Some(x1) = next_skip_never(&mut xs) else {
            return x0;
        };
        if xs.len() == 0 && x0 == x1 {
            return x0;
        }

        // Now default slow version.

        let xs = xs.as_slice();
        let mut xs: Vec<TyBasic> = [x0, x1]
            .iter()
            .chain(xs)
            .flat_map(|x| x.iter_union())
            .duped()
            .collect();
        xs.sort();
        xs.dedup();
        // Try merging adjacent elements
        let xs = merge_adjacent(xs, |x, y| match (x, y) {
            (TyBasic::List(x), TyBasic::List(y)) => {
                Either::Left(TyBasic::List(ArcTy::union2(x, y)))
            }
            (TyBasic::Dict(x_k, x_v), TyBasic::Dict(y_k, y_v)) => Either::Left(TyBasic::Dict(
                ArcTy::union2(x_k, y_k),
                ArcTy::union2(x_v, y_v),
            )),
            (TyBasic::Custom(x), TyBasic::Custom(y)) => match TyCustom::union2(x, y) {
                Ok(u) => Either::Left(TyBasic::Custom(u)),
                Err((x, y)) => Either::Right((TyBasic::Custom(x), TyBasic::Custom(y))),
            },
            xy => Either::Right(xy),
        });

        Ty {
            alternatives: xs.into_iter().collect(),
        }
    }

    /// Iterate over the types within a union, pretending the type is a singleton union if not a union.
    pub(crate) fn iter_union(&self) -> &[TyBasic] {
        &self.alternatives
    }

    /// Apply typechecking operation for each alternative.
    ///
    /// If at least one was successful, return the union of all successful results.
    pub(crate) fn typecheck_union_simple(
        &self,
        typecheck: impl Fn(&TyBasic) -> Result<Ty, ()>,
    ) -> Result<Ty, ()> {
        if self.is_any() || self.is_never() {
            Ok(self.dupe())
        } else {
            match self.iter_union() {
                // Optimize common case.
                [x] => typecheck(x),
                xs => {
                    let mut good = Vec::with_capacity(self.alternatives.len());
                    for basic in xs {
                        match typecheck(basic) {
                            Ok(ty) => good.push(ty),
                            Err(()) => {}
                        }
                    }
                    if good.is_empty() {
                        Err(())
                    } else {
                        Ok(Ty::unions(good))
                    }
                }
            }
        }
    }

    /// Create a union of two entries.
    pub fn union2(a: Self, b: Self) -> Self {
        // Handle fast cases first.
        // Optimizations, semantically identical to default implementation.
        if a.is_any() || b.is_any() {
            Ty::any()
        } else if a == b {
            a
        } else if a.is_never() {
            b
        } else if b.is_never() {
            a
        } else {
            Ty::unions(vec![a, b])
        }
    }

    /// Create a custom type.
    /// This is called from generated code.
    pub fn custom(t: impl TyCustomImpl) -> Self {
        Ty::basic(TyBasic::Custom(TyCustom(Arc::new(t))))
    }

    /// Create a custom function type.
    pub fn custom_function(f: impl TyCustomFunctionImpl) -> Self {
        Ty::custom(TyCustomFunction(f))
    }

    /// If I do `self[i]` what will the resulting type be.
    pub(crate) fn indexed(self, i: usize) -> Ty {
        Ty::unions(self.alternatives.iter().map(|x| x.indexed(i)).collect())
    }

    pub(crate) fn from_docs_member(member: &DocMember) -> Self {
        match member {
            DocMember::Property(x) => x.typ.clone(),
            DocMember::Function(x) => Self::from_docs_function(x),
        }
    }

    /// Typechecker type of value.
    pub fn of_value(value: Value) -> Ty {
        if let Some(t) = value.get_ref().typechecker_ty() {
            t
        } else {
            Ty::from_docs_member(&DocMember::from_value(value))
        }
    }

    pub(crate) fn from_docs_property(property: &DocProperty) -> Self {
        property.typ.clone()
    }

    pub(crate) fn from_docs_function(function: &DocFunction) -> Self {
        let mut params = Vec::with_capacity(function.params.len());
        let mut seen_no_args = false;
        for p in &function.params {
            match p {
                DocParam::Arg {
                    name,
                    typ,
                    default_value,
                    ..
                } => {
                    let mut r = if seen_no_args {
                        Param::name_only(name, typ.clone())
                    } else {
                        Param::pos_or_name(name, typ.clone())
                    };
                    if default_value.is_some() {
                        r = r.optional();
                    }
                    params.push(r);
                }
                DocParam::OnlyPosBefore => {
                    for x in params.iter_mut() {
                        if matches!(x.mode, ParamMode::PosOrName(_)) {
                            x.mode = ParamMode::PosOnly;
                        }
                    }
                }
                DocParam::NoArgs => seen_no_args = true,
                DocParam::Args { typ, .. } => {
                    seen_no_args = true;
                    params.push(Param::args(typ.clone()))
                }
                DocParam::Kwargs { typ, .. } => params.push(Param::kwargs(typ.clone())),
            }
        }
        let result = function.ret.typ.clone();
        match &function.as_type {
            None => Ty::function(params, result),
            Some(type_attr) => Ty::ctor_function(type_attr, params, result),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.alternatives.as_slice() {
            [] => write!(f, "{}", TypingNever::TYPE),
            xs => {
                for (i, x) in xs.iter().enumerate() {
                    if i != 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", x)?;
                }
                Ok(())
            }
        }
    }
}
