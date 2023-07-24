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

use allocative::Allocative;
use either::Either;
use serde::Serialize;
use serde::Serializer;

use crate::codemap::CodeMap;
use crate::codemap::Spanned;
use crate::docs::DocFunction;
use crate::docs::DocMember;
use crate::docs::DocParam;
use crate::docs::DocProperty;
use crate::eval::compiler::constants::Constants;
use crate::eval::compiler::scope::payload::CstIdent;
use crate::eval::compiler::scope::payload::CstPayload;
use crate::eval::compiler::scope::payload::CstTypeExpr;
use crate::eval::compiler::scope::ResolvedIdent;
use crate::slice_vec_ext::SliceExt;
use crate::slice_vec_ext::VecExt;
use crate::syntax::type_expr::TypeExprUnpackP;
use crate::typing::basic::TyBasic;
use crate::typing::custom::TyCustom;
use crate::typing::custom::TyCustomImpl;
use crate::typing::error::InternalError;
use crate::typing::function::Param;
use crate::typing::function::ParamMode;
use crate::typing::function::TyCustomFunction;
use crate::typing::function::TyCustomFunctionImpl;
use crate::typing::function::TyFunction;
use crate::typing::mode::TypecheckMode;
use crate::typing::oracle::ctx::TypingOracleCtx;
use crate::typing::oracle::traits::TypingAttr;
use crate::typing::structs::TyStruct;
use crate::typing::TypingOracle;
use crate::values::bool::StarlarkBool;
use crate::values::float::StarlarkFloat;
use crate::values::tuple::value::FrozenTuple;
use crate::values::types::bigint::StarlarkBigInt;
use crate::values::typing::type_compiled::TypeCompiled;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;

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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Allocative)]
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
    alternatives: Vec<TyBasic>,
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Allocative)]
pub struct TyName(String);

impl Display for TyName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.as_str() {
            "string" => write!(f, "str.type"),
            "int" => write!(f, "int.type"),
            "bool" => write!(f, "bool.type"),
            "NoneType" => write!(f, "None"),
            other => write!(f, "\"{}\"", other),
        }
    }
}

impl PartialEq<str> for TyName {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl TyName {
    /// Get the underlying `str` for a `TyName`.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub(crate) fn new(name: &str) -> TyName {
        TyName(name.to_owned())
    }
}

/// A series of types that are unioned together.
/// Must be at least two elements, all distinct elements, with no nested `Union` types directly inside it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Allocative)]
pub struct TyUnion(Vec<Ty>);

impl Display for TyUnion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_container::fmt_container(f, "[", "]", &self.0)
    }
}

impl TyUnion {
    /// The alternatives within a union, will always be at least two elements.
    pub fn alternatives(&self) -> &[Ty] {
        &self.0
    }
}

fn merge_adjacent<T>(xs: Vec<T>, f: impl Fn(T, T) -> Either<T, (T, T)>) -> Vec<T> {
    let mut res = Vec::new();
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

    /// Create a [`Ty::Name`], or one of the standard functions.
    pub(crate) fn name(name: &str) -> Self {
        match name {
            "list" => Self::list(Ty::any()),
            "dict" => Self::dict(Ty::any(), Ty::any()),
            "function" => Self::any_function(),
            "struct" => Self::custom(TyStruct::any()),
            "never" => Self::never(),
            "NoneType" => Self::none(),
            "bool" => Self::bool(),
            "int" => Self::int(),
            "float" => Self::float(),
            "string" => Self::string(),
            "tuple" => Self::any_tuple(),
            // Note that "tuple" cannot be converted to Ty::Tuple
            // since we don't know the length of the tuple.
            _ => Ty::basic(TyBasic::Name(TyName(name.to_owned()))),
        }
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

    pub(crate) fn basic(basic: TyBasic) -> Self {
        Ty {
            alternatives: vec![basic],
        }
    }

    /// "any" type: can hold any value.
    pub fn any() -> Self {
        Ty::basic(TyBasic::Any)
    }

    pub(crate) fn never() -> Self {
        Ty {
            alternatives: Vec::new(),
        }
    }

    /// Create a `None` type.
    pub fn none() -> Self {
        Ty::basic(TyBasic::none())
    }

    /// Create a boolean type.
    pub fn bool() -> Self {
        Ty::starlark_value::<StarlarkBool>()
    }

    /// Create the int type.
    pub fn int() -> Self {
        Ty::starlark_value::<StarlarkBigInt>()
    }

    /// Create a float type.
    pub fn float() -> Self {
        Ty::starlark_value::<StarlarkFloat>()
    }

    /// Create a string type.
    pub fn string() -> Self {
        Ty::basic(TyBasic::string())
    }

    /// Create a list type.
    pub fn list(element: Ty) -> Self {
        Ty::basic(TyBasic::list(element))
    }

    /// Create a iterable type.
    pub fn iter(item: Ty) -> Self {
        Ty::basic(TyBasic::iter(item))
    }

    /// Create a dictionary type.
    pub fn dict(key: Ty, value: Ty) -> Self {
        Ty::basic(TyBasic::dict(key, value))
    }

    /// Create a tuple of two elements
    pub fn tuple2(a: Ty, b: Ty) -> Self {
        Ty::tuple(vec![a, b])
    }

    /// Create a tuple of given elements.
    pub fn tuple(elements: Vec<Ty>) -> Self {
        Ty::basic(TyBasic::Tuple(elements))
    }

    /// Tuple where elements are unknown.
    pub(crate) fn any_tuple() -> Self {
        Ty::starlark_value::<FrozenTuple>()
    }

    /// Create a function type.
    pub fn function(params: Vec<Param>, result: Ty) -> Self {
        Self::custom(TyCustomFunction(TyFunction {
            type_attr: None,
            params,
            result: Box::new(result),
        }))
    }

    /// Create a function, where the first argument is the result of `.type`.
    pub fn ctor_function(type_attr: &Ty, params: Vec<Param>, result: Ty) -> Self {
        Self::custom(TyCustomFunction(TyFunction {
            type_attr: Some(type_attr.clone()),
            params,
            result: Box::new(result),
        }))
    }

    /// Function type that accepts any arguments and returns any result.
    pub(crate) fn any_function() -> Self {
        Self::custom(TyCustomFunction(TyFunction::any()))
    }

    pub(crate) fn starlark_value<'v, T: StarlarkValue<'v>>() -> Self {
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

    /// Create a unions type, which will be normalised before being created.
    pub fn unions(xs: Vec<Self>) -> Self {
        let mut xs: Vec<TyBasic> = xs.into_iter().flat_map(|x| x.into_iter_union()).collect();
        xs.sort();
        xs.dedup();
        if xs.contains(&TyBasic::Any) {
            return Ty::any();
        }
        // Try merging adjacent elements
        let xs = merge_adjacent(xs, |x, y| match (x, y) {
            (TyBasic::List(x), TyBasic::List(y)) => Either::Left(TyBasic::list(Ty::union2(*x, *y))),
            (TyBasic::Dict(x), TyBasic::Dict(y)) => {
                Either::Left(TyBasic::dict(Ty::union2(x.0, y.0), Ty::union2(x.1, y.1)))
            }
            (TyBasic::Custom(x), TyBasic::Custom(y)) => match TyCustom::union2(x, y) {
                Ok(u) => Either::Left(TyBasic::Custom(u)),
                Err((x, y)) => Either::Right((TyBasic::Custom(x), TyBasic::Custom(y))),
            },
            xy => Either::Right(xy),
        });

        Ty { alternatives: xs }
    }

    /// Iterate over the types within a union, pretending the type is a singleton union if not a union.
    pub(crate) fn iter_union(&self) -> &[TyBasic] {
        &self.alternatives
    }

    /// Iterate over the types within a union, pretending the type is a singleton union if not a union.
    pub(crate) fn into_iter_union(self) -> impl Iterator<Item = TyBasic> {
        self.alternatives.into_iter()
    }

    /// Create a union of two entries.
    pub fn union2(a: Self, b: Self) -> Self {
        Self::unions(vec![a, b])
    }

    /// Create a custom type.
    /// This is called from generated code.
    pub fn custom(t: impl TyCustomImpl) -> Self {
        Ty::basic(TyBasic::Custom(TyCustom(Box::new(t))))
    }

    /// Create a custom function type.
    pub fn custom_function(f: impl TyCustomFunctionImpl) -> Self {
        Ty::custom(TyCustomFunction(f))
    }

    /// If I do `self[i]` what will the resulting type be.
    pub(crate) fn indexed(self, i: usize) -> Ty {
        Ty::unions(self.alternatives.into_map(|x| x.indexed(i)))
    }

    /// Returns false on Void, since that is definitely not a list
    pub(crate) fn probably_a_list(&self, ctx: TypingOracleCtx) -> bool {
        if self.is_never() {
            return false;
        }
        self.intersects(&Self::list(Ty::any()), ctx)
    }

    /// If you get to a point where these types are being checked, might they succeed
    pub(crate) fn intersects(&self, other: &Self, oracle: TypingOracleCtx) -> bool {
        if self.is_any() || self.is_never() || other.is_any() || other.is_never() {
            return true;
        }

        let equal_names =
            |x: &TyName, y: &TyName| x == y || oracle.subtype(x, y) || oracle.subtype(y, x);

        let itered = |ty: &TyBasic| oracle.attribute(ty, TypingAttr::Iter)?.ok();

        for x in self.iter_union() {
            for y in other.iter_union() {
                let b = match (x, y) {
                    (TyBasic::Name(x), TyBasic::Name(y)) => equal_names(x, y),
                    (TyBasic::List(x), TyBasic::List(y)) => x.intersects(y, oracle),
                    (TyBasic::Dict(x), TyBasic::Dict(y)) => {
                        x.0.intersects(&y.0, oracle) && x.1.intersects(&y.1, oracle)
                    }
                    (TyBasic::Tuple(_), t) | (t, TyBasic::Tuple(_))
                        if t.as_name() == Some("tuple") =>
                    {
                        true
                    }
                    (TyBasic::Tuple(xs), TyBasic::Tuple(ys)) if xs.len() == ys.len() => {
                        std::iter::zip(xs, ys).all(|(x, y)| x.intersects(y, oracle))
                    }
                    (TyBasic::Iter(x), TyBasic::Iter(y)) => x.intersects(y, oracle),
                    (TyBasic::Iter(x), y) | (y, TyBasic::Iter(x)) => match itered(y) {
                        Some(yy) => x.intersects(&yy, oracle),
                        None => false,
                    },
                    (TyBasic::Custom(x), TyBasic::Custom(y)) => TyCustom::intersects(x, y),
                    (x, y)
                        if x.as_name() == Some("function") && y.as_name() == Some("function") =>
                    {
                        true
                    }
                    // There are lots of other cases that overlap, but add them as we need them
                    (x, y) => x == y,
                };
                if b {
                    return true;
                }
            }
        }
        return false;
    }

    pub(crate) fn from_type_expr_opt(
        x: &Option<Box<CstTypeExpr>>,
        typecheck_mode: TypecheckMode,
        approximations: &mut Vec<Approximation>,
        codemap: &CodeMap,
    ) -> Result<Self, InternalError> {
        match x {
            None => Ok(Ty::any()),
            Some(x) => Self::from_type_expr(x, typecheck_mode, approximations, codemap),
        }
    }

    pub(crate) fn from_type_expr(
        x: &CstTypeExpr,
        typecheck_mode: TypecheckMode,
        approximations: &mut Vec<Approximation>,
        codemap: &CodeMap,
    ) -> Result<Self, InternalError> {
        match typecheck_mode {
            TypecheckMode::Lint => {
                // TODO(nga): remove this branch: in lint, populate types in CstPayload
                //   before running typechecking, and always fetch the type from the payload.
                Self::from_type_expr_for_lint(x, codemap, approximations)
            }
            TypecheckMode::Compiler => match x.payload {
                Some(ty) => Ok(ty.as_ty()),
                None => Err(InternalError::msg(
                    "type payload is not populated",
                    x.span,
                    codemap,
                )),
            },
        }
    }

    fn from_type_expr_for_lint(
        x: &CstTypeExpr,
        codemap: &CodeMap,
        approximations: &mut Vec<Approximation>,
    ) -> Result<Self, InternalError> {
        let x = TypeExprUnpackP::unpack(&x.expr, codemap)
            .map_err(InternalError::from_eval_exception)?;
        Ok(Self::from_expr_impl(&x, approximations))
    }

    // This should go away when `ExprType` is disconnected from `Expr`.
    fn from_expr_impl(
        x: &Spanned<TypeExprUnpackP<CstPayload>>,
        approximations: &mut Vec<Approximation>,
    ) -> Self {
        let mut unknown = || {
            approximations.push(Approximation::new("Unknown type", x));
            Ty::any()
        };

        fn ident_global(ident: &CstIdent) -> Option<FrozenValue> {
            match &ident.node.1 {
                Some(ResolvedIdent::Global(x)) => Some(*x),
                _ => None,
            }
        }

        match &x.node {
            TypeExprUnpackP::Tuple(xs) => {
                Ty::tuple(xs.map(|x| Self::from_expr_impl(x, approximations)))
            }
            TypeExprUnpackP::Any(xs) => {
                Ty::unions(xs.map(|x| Self::from_expr_impl(x, approximations)))
            }
            TypeExprUnpackP::ListOf(x) => Ty::list(Self::from_expr_impl(x, approximations)),
            TypeExprUnpackP::DictOf(k, v) => Ty::dict(
                Self::from_expr_impl(k, approximations),
                Self::from_expr_impl(v, approximations),
            ),
            TypeExprUnpackP::Literal(x) => {
                if x.is_empty() || x.starts_with('_') {
                    Ty::any()
                } else {
                    Ty::name(x)
                }
            }
            TypeExprUnpackP::Path(first, rem) => {
                if rem.is_empty() {
                    if let Some(v) = ident_global(first) {
                        let heap = Heap::new();
                        match TypeCompiled::new(v.to_value(), &heap) {
                            Ok(ty) => ty.as_ty(),
                            Err(_) => unknown(),
                        }
                    } else {
                        unknown()
                    }
                } else if rem.len() == 1 {
                    if rem[0].node == "type" {
                        if first.node.0 == "str" {
                            Ty::string()
                        } else {
                            Ty::name(&first.node.0)
                        }
                    } else {
                        unknown()
                    }
                } else {
                    unknown()
                }
            }
            TypeExprUnpackP::Index(a, i) => {
                if let Some(a) = ident_global(a) {
                    if !a.to_value().ptr_eq(Constants::get().fn_list.0.to_value()) {
                        approximations.push(Approximation::new("Not list", x));
                        return Ty::any();
                    }
                    let i = Self::from_expr_impl(i, approximations);
                    let heap = Heap::new();
                    let i = TypeCompiled::from_ty(&i, &heap);
                    match a.to_value().get_ref().at(i.to_inner(), &heap) {
                        Ok(t) => match TypeCompiled::new(t, &heap) {
                            Ok(ty) => ty.as_ty(),
                            Err(_) => {
                                approximations
                                    .push(Approximation::new("TypeCompiled::new failed", x));
                                Ty::any()
                            }
                        },
                        Err(e) => {
                            approximations.push(Approximation::new("Getitem failed", e));
                            Ty::any()
                        }
                    }
                } else {
                    approximations.push(Approximation::new("Not global", x));
                    Ty::any()
                }
            }
            TypeExprUnpackP::Index2(a, i0, i1) => {
                if let Some(a) = ident_global(a) {
                    if !a.to_value().ptr_eq(Constants::get().fn_dict.0.to_value()) {
                        approximations.push(Approximation::new("Not dict", x));
                        return Ty::any();
                    }
                    let i0 = Self::from_expr_impl(i0, approximations);
                    let i1 = Self::from_expr_impl(i1, approximations);
                    let heap = Heap::new();
                    let i0 = TypeCompiled::from_ty(&i0, &heap);
                    let i1 = TypeCompiled::from_ty(&i1, &heap);
                    match a
                        .to_value()
                        .get_ref()
                        .at2(i0.to_inner(), i1.to_inner(), &heap)
                    {
                        Ok(t) => match TypeCompiled::new(t, &heap) {
                            Ok(ty) => ty.as_ty(),
                            Err(_) => {
                                approximations
                                    .push(Approximation::new("TypeCompiled::new failed", x));
                                Ty::any()
                            }
                        },
                        Err(e) => {
                            approximations.push(Approximation::new("Getitem2 failed", e));
                            Ty::any()
                        }
                    }
                } else {
                    approximations.push(Approximation::new("Not global", x));
                    Ty::any()
                }
            }
        }
    }

    pub(crate) fn from_docs_member(member: &DocMember) -> Self {
        match member {
            DocMember::Property(x) => x.typ.clone(),
            DocMember::Function(x) => Self::from_docs_function(x),
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
            [] => write!(f, "\"never\""),
            [x] => write!(f, "{}", x),
            xs => display_container::fmt_container(f, "[", "]", xs),
        }
    }
}
