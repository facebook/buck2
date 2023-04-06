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

use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use either::Either;
use gazebo::prelude::SliceExt;
use gazebo::prelude::VecExt;

use crate::docs;
use crate::eval::compiler::scope::CstExpr;
use crate::syntax::ast::AstExpr;
use crate::syntax::ast::AstExprP;
use crate::syntax::ast::AstLiteral;
use crate::syntax::ast::AstPayload;
use crate::syntax::ast::AstStmt;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::StmtP;
use crate::syntax::AstModule;
use crate::syntax::Dialect;
use crate::typing::ctx::TypingContext;

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

/// An argument being passed to a function
#[derive(Debug)]
pub enum Arg {
    /// A positional argument.
    Pos(Ty),
    /// A named argument.
    Name(String, Ty),
    /// A `*args`.
    Args(Ty),
    /// A `**kwargs`.
    Kwargs(Ty),
}

/// The type of a parameter - can be positional, by name, `*args` or `**kwargs`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParamMode {
    /// Parameter can only be passed by position.
    PosOnly,
    /// Parameter can be passed by position or name.
    PosOrName(String),
    /// Parameter can only be passed by name.
    NameOnly(String),
    /// Parameter is `*args`.
    Args,
    /// Parameter is `**kwargs`.
    Kwargs,
}

/// A parameter argument to a function
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Param {
    /// The type of parameter
    pub mode: ParamMode,
    /// Whether the parameter have a default value or is otherwise optional
    pub optional: bool,
    /// The type of the parameter
    pub ty: Ty,
}

impl Param {
    /// Create a [`ParamMode::PosOnly`] parameter.
    pub fn pos_only(ty: Ty) -> Self {
        Self {
            mode: ParamMode::PosOnly,
            optional: false,
            ty,
        }
    }

    /// Create a [`ParamMode::NameOnly`] parameter.
    pub fn name_only(name: &str, ty: Ty) -> Self {
        Self {
            mode: ParamMode::NameOnly(name.to_owned()),
            optional: false,
            ty,
        }
    }

    /// Create a [`ParamMode::PosOrName`] parameter.
    pub fn pos_or_name(name: &str, ty: Ty) -> Self {
        Self {
            mode: ParamMode::PosOrName(name.to_owned()),
            optional: false,
            ty,
        }
    }

    /// Make a parameter optional.
    pub fn optional(self) -> Self {
        Self {
            optional: true,
            ..self
        }
    }

    /// Create a [`ParamMode::Args`] parameter.
    pub fn args(ty: Ty) -> Self {
        Self {
            mode: ParamMode::Args,
            optional: true,
            ty,
        }
    }

    /// Create a [`ParamMode::Kwargs`] parameter.
    pub fn kwargs(ty: Ty) -> Self {
        Self {
            mode: ParamMode::Kwargs,
            optional: true,
            ty,
        }
    }

    pub(crate) fn allows_pos(&self) -> bool {
        match self.mode {
            ParamMode::PosOnly | ParamMode::PosOrName(_) | ParamMode::Args => true,
            ParamMode::NameOnly(_) | ParamMode::Kwargs => false,
        }
    }

    pub(crate) fn allows_many(&self) -> bool {
        match self.mode {
            ParamMode::Args | ParamMode::Kwargs => true,
            _ => false,
        }
    }

    /// Get a display name for this parameter.
    pub fn name(&self) -> &str {
        match &self.mode {
            ParamMode::PosOnly => "_",
            ParamMode::PosOrName(x) => x,
            ParamMode::NameOnly(x) => x,
            ParamMode::Args => "*args",
            ParamMode::Kwargs => "**kwargs",
        }
    }
}

/// A Starlark type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Ty {
    /// Type that can't be inhabited.
    /// If an expression has this type, then the code cannot be reached.
    Void,
    /// Type that contain anything
    Any,
    /// A series of alternative types.
    Union(TyUnion),
    /// A name, represented by `"name"` in the Starlark type.
    /// Will never be a type that can be represented by another operation,
    /// e.g. never `"list"` because `Ty::List` could be used instead.
    Name(TyName),
    /// The `None` type.
    None,
    /// Iter is a type that supports iteration, only used as arguments to primitive functions.
    /// The inner type is applicable for each iteration element.
    Iter(Box<Ty>),
    /// A list.
    List(Box<Ty>),
    /// A tuple. May be empty, to indicate the empty tuple.
    Tuple(Vec<Ty>),
    /// A dictionary, with key and value types
    Dict(Box<(Ty, Ty)>),
    /// A `struct`.
    Struct {
        /// The fields that are definitely present in the struct, with their types.
        fields: BTreeMap<String, Ty>,
        /// [`true`] if there might be additional fields not captured above,
        /// [`false`] if this struct has no extra members.
        extra: bool,
    },
    /// A `function`.
    Function(TyFunction),
}

/// The name of an atomic type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyName(String);

impl Display for TyName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.0)
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
}

/// A series of types that are unioned together.
/// Must be at least two elements, all distinct elements, with no nested `Union` types directly inside it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyUnion(Vec<Ty>);

impl Display for TyUnion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut comma = commas();
        write!(f, "[")?;
        for x in &self.0 {
            comma(f)?;
            write!(f, "{}", x)?;
        }
        write!(f, "]")
    }
}

impl TyUnion {
    /// The alternatives within a union, will always be at least two elements.
    pub fn alternatives(&self) -> &[Ty] {
        &self.0
    }
}

/// A function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyFunction {
    /// The name of the function. Typically `""`, but for a few special builtin functions the name
    /// is used later to call [`TypingOracle::builtin_call`](crate::typing::TypingOracle::builtin_call).
    pub name: String,
    /// The `.type` property of the function, often `""`.
    pub type_attr: String,
    /// The parameters to the function.
    pub params: Vec<Param>,
    /// The result type of the function.
    pub result: Box<Ty>,
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
    /// Create a [`Ty::Any`], but tagged in such a way it can easily be found.
    pub fn todo() -> Self {
        Ty::Any
    }

    /// Create a [`Ty::Name`], or one of the standard functions.
    pub fn name(name: &str) -> Self {
        match name {
            "list" => Self::List(Box::new(Ty::Any)),
            "dict" => Self::Dict(Box::new((Ty::Any, Ty::Any))),
            "NoneType" => Self::None,
            "function" => {
                Self::function(vec![Param::args(Ty::Any), Param::kwargs(Ty::Any)], Ty::Any)
            }
            "struct" => Self::Struct {
                fields: BTreeMap::new(),
                extra: true,
            },
            _ => Self::Name(TyName(name.to_owned())),
        }
    }

    /// Create a boolean type.
    pub fn bool() -> Self {
        Self::name("bool")
    }

    /// Create the int type.
    pub fn int() -> Self {
        Self::name("int")
    }

    /// Create a float type.
    pub fn float() -> Self {
        Self::name("float")
    }

    /// Create a string type.
    pub fn string() -> Self {
        Self::name("string")
    }

    /// Create a list type.
    pub fn list(inner: Ty) -> Self {
        Ty::List(Box::new(inner))
    }

    /// Create a dictionary type.
    pub fn dict(key: Ty, value: Ty) -> Self {
        Ty::Dict(Box::new((key, value)))
    }

    /// Create a tuple of two elements
    pub fn tuple2(a: Ty, b: Ty) -> Self {
        Ty::Tuple(vec![a, b])
    }

    /// Create a function type.
    pub fn function(params: Vec<Param>, result: Ty) -> Self {
        Self::Function(TyFunction {
            name: String::new(),
            type_attr: String::new(),
            params,
            result: Box::new(result),
        })
    }

    /// Create a function, where the first argument is the result of `.type`.
    pub fn ctor_function(type_attr: &str, params: Vec<Param>, result: Ty) -> Self {
        Self::Function(TyFunction {
            name: String::new(),
            type_attr: type_attr.to_owned(),
            params,
            result: Box::new(result),
        })
    }

    /// Create a function, where the function has a name that is tracked and passed to `builtin_call`.
    pub fn special_function(name: &str, params: Vec<Param>, result: Ty) -> Self {
        Self::Function(TyFunction {
            name: name.to_owned(),
            type_attr: String::new(),
            params,
            result: Box::new(result),
        })
    }

    pub(crate) fn is_any(&self) -> bool {
        self == &Ty::Any
    }

    pub(crate) fn is_void(&self) -> bool {
        self == &Ty::Void
    }

    pub(crate) fn is_list(&self) -> bool {
        matches!(self, Ty::List(_))
    }

    pub(crate) fn is_name(&self, name: &str) -> bool {
        match self {
            Ty::Name(x) => x == name,
            _ => false,
        }
    }

    /// Create a unions type, which will be normalised before being created.
    pub fn unions(mut xs: Vec<Self>) -> Self {
        xs = xs.into_iter().flat_map(|x| x.into_iter_union()).collect();
        xs.sort();
        xs.dedup();
        xs.retain(|x| x != &Ty::Void);
        if xs.contains(&Ty::Any) {
            return Ty::Any;
        }
        // Try merging adjacent elements
        xs = merge_adjacent(xs, |x, y| match (x, y) {
            (Ty::List(x), Ty::List(y)) => Either::Left(Ty::list(Ty::union2(*x, *y))),
            (Ty::Dict(x), Ty::Dict(y)) => {
                Either::Left(Ty::dict(Ty::union2(x.0, y.0), Ty::union2(x.1, y.1)))
            }
            (
                Ty::Struct { fields, extra },
                Ty::Struct {
                    fields: mut fields2,
                    extra: extra2,
                },
            ) if extra == extra2 && itertools::equal(fields.keys(), fields2.keys()) => {
                let mut res = BTreeMap::new();
                for (k, v) in fields {
                    let v2 = fields2.remove(&k).unwrap();
                    res.insert(k, Ty::union2(v, v2));
                }
                Either::Left(Ty::Struct { fields: res, extra })
            }
            xy => Either::Right(xy),
        });

        if xs.is_empty() {
            Ty::Void
        } else if xs.len() == 1 {
            xs.pop().unwrap()
        } else {
            Self::Union(TyUnion(xs))
        }
    }

    /// Iterate over the types within a union, pretending the type is a singleton union if not a union.
    pub(crate) fn iter_union(&self) -> impl Iterator<Item = &Self> {
        match self {
            Self::Union(xs) => Either::Left(xs.0.iter()),
            Self::Void => Either::Left([].iter()),
            _ => Either::Right(std::iter::once(self)),
        }
    }

    /// Iterate over the types within a union, pretending the type is a singleton union if not a union.
    pub(crate) fn into_iter_union(self) -> impl Iterator<Item = Self> {
        match self {
            Self::Union(xs) => Either::Left(xs.0.into_iter()),
            Self::Void => Either::Right(Either::Left(std::iter::empty())),
            _ => Either::Right(Either::Right(std::iter::once(self))),
        }
    }

    /// Create a union of two entries.
    pub fn union2(a: Self, b: Self) -> Self {
        Self::unions(vec![a, b])
    }

    /// If I do `self[i]` what will the resulting type be.
    pub(crate) fn indexed(self, i: usize) -> Ty {
        match self {
            Ty::Any => Ty::Any,
            Ty::Void => Ty::Void,
            Ty::None => Ty::Void,
            Ty::List(x) => *x,
            Ty::Tuple(xs) => xs.get(i).cloned().unwrap_or(Ty::Void),
            Ty::Union(xs) => Ty::unions(xs.0.into_map(|x| x.indexed(i))),
            // Not exactly sure what we should do here
            _ => Ty::Any,
        }
    }

    /// Returns false on Void, since that is definitely not a list
    pub(crate) fn probably_a_list(&self) -> bool {
        if self.is_void() {
            return false;
        }
        self.intersects(&Self::list(Ty::Any), None)
    }

    /// See what lies behind an attribute on a type
    pub(crate) fn attribute(&self, attr: &str, ctx: &TypingContext) -> Result<Ty, ()> {
        // There are some structural types which have to be handled in a specific way
        match self {
            Ty::Any => Ok(Ty::Any),
            Ty::Void => Ok(Ty::Void),
            Ty::Union(xs) => {
                let rs = xs
                    .alternatives()
                    .iter()
                    .flat_map(|x| x.attribute(attr, ctx))
                    .collect::<Vec<_>>();
                if rs.is_empty() {
                    // Since xs wasn't empty, we must have had all types give us an invalid attribute.
                    // So therefore this attribute must be invalid.
                    Err(())
                } else {
                    Ok(Ty::unions(rs))
                }
            }
            _ => match ctx.oracle.attribute(self, attr) {
                Some(r) => r,
                None => Ok(ctx.approximation("oracle.attribute", format!("{}.{}", self, attr))),
            },
        }
    }

    /// If you get to a point where these types are being checked, might they succeed
    pub(crate) fn intersects(&self, other: &Self, ctx: Option<&TypingContext>) -> bool {
        if self.is_any() || self.is_void() || other.is_any() || other.is_void() {
            return true;
        }

        let equal_names = |x: &TyName, y: &TyName| {
            x == y
                || match ctx {
                    None => false,
                    Some(ctx) => ctx.oracle.subtype(x, y) || ctx.oracle.subtype(y, x),
                }
        };

        let itered = |ty: &Ty| ctx?.oracle.attribute(ty, "__iter__")?.ok();

        for x in self.iter_union() {
            for y in other.iter_union() {
                let b = match (x, y) {
                    (Ty::Name(x), Ty::Name(y)) => equal_names(x, y),
                    (Ty::List(x), Ty::List(y)) => x.intersects(y, ctx),
                    (Ty::Dict(x), Ty::Dict(y)) => {
                        x.0.intersects(&y.0, ctx) && x.1.intersects(&y.1, ctx)
                    }
                    (Ty::Tuple(_), t) | (Ty::Tuple(_), t) if t.is_name("tuple") => true,
                    (Ty::Tuple(xs), Ty::Tuple(ys)) if xs.len() == ys.len() => {
                        std::iter::zip(xs, ys).all(|(x, y)| x.intersects(y, ctx))
                    }
                    (Ty::Iter(x), Ty::Iter(y)) => x.intersects(y, ctx),
                    (Ty::Iter(x), y) | (y, Ty::Iter(x)) => match itered(y) {
                        Some(yy) => x.intersects(&yy, ctx),
                        None => false,
                    },
                    (Ty::Function(_), Ty::Function(_)) => true,
                    (Ty::Struct { .. }, Ty::Struct { .. }) => {
                        // FIXME: Can probably be a bit more precise here
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

    pub(crate) fn from_expr_opt(
        x: &Option<Box<CstExpr>>,
        approximations: &mut Vec<Approximation>,
    ) -> Self {
        match x {
            None => Ty::Any,
            Some(x) => Self::from_expr(x, approximations),
        }
    }

    pub(crate) fn from_expr<P: AstPayload>(
        x: &AstExprP<P>,
        approximations: &mut Vec<Approximation>,
    ) -> Self {
        match &**x {
            ExprP::Tuple(xs) => Ty::Tuple(xs.map(|x| Self::from_expr(x, approximations))),
            ExprP::Dot(x, b) if &**b == "type" => match &***x {
                ExprP::Identifier(x, _) => match (*x).as_str() {
                    "str" => Ty::string(),
                    x => Ty::name(x),
                },
                _ => {
                    approximations.push(Approximation::new("Unknown type", x));
                    Ty::Any
                }
            },
            ExprP::Literal(AstLiteral::String(x)) => {
                if x.is_empty() || x.starts_with('_') {
                    Ty::Any
                } else {
                    Ty::name(x.as_str())
                }
            }
            ExprP::List(x) => {
                if x.len() == 1 {
                    Ty::list(Self::from_expr(&x[0], approximations))
                } else {
                    Ty::unions(x.map(|x| Self::from_expr(x, approximations)))
                }
            }
            ExprP::Dict(x) if x.len() == 1 => Ty::dict(
                Self::from_expr(&x[0].0, approximations),
                Self::from_expr(&x[0].1, approximations),
            ),
            ExprP::Identifier(x, _) if &**x == "None" => Ty::None,
            _ => {
                approximations.push(Approximation::new("Unknown type", x));
                Ty::Any
            }
        }
    }

    pub(crate) fn from_docs_member(member: &docs::Member) -> Self {
        match member {
            docs::Member::Property(x) => Self::from_docs_type(&x.typ),
            docs::Member::Function(x) => Self::from_docs_function(x),
        }
    }

    pub(crate) fn from_docs_function(function: &docs::Function) -> Self {
        let mut params = Vec::with_capacity(function.params.len());
        let mut no_args = false;
        for p in &function.params {
            match p {
                docs::Param::Arg {
                    name,
                    typ,
                    default_value,
                    ..
                } => {
                    let mut r = if no_args {
                        Param::name_only(name, Ty::from_docs_type(typ))
                    } else {
                        Param::pos_or_name(name, Ty::from_docs_type(typ))
                    };
                    if default_value.is_some() {
                        r = r.optional();
                    }
                    params.push(r);
                }
                docs::Param::NoArgs => no_args = true,
                docs::Param::Args { typ, .. } => params.push(Param::args(Ty::from_docs_type(typ))),
                docs::Param::Kwargs { typ, .. } => {
                    params.push(Param::kwargs(Ty::from_docs_type(typ)))
                }
            }
        }
        Ty::function(params, Self::from_docs_type(&function.ret.typ))
    }

    pub(crate) fn from_docs_type(ty: &Option<docs::Type>) -> Self {
        fn get_expr(x: &AstStmt) -> Option<&AstExpr> {
            match &x.node {
                StmtP::Statements(x) if x.len() == 1 => get_expr(&x[0]),
                StmtP::Expression(x) => Some(x),
                _ => None,
            }
        }

        match ty {
            None => Ty::Any,
            Some(x) => {
                if let Ok(ast) = AstModule::parse("type", x.raw_type.clone(), &Dialect::Standard) {
                    if let Some(expr) = &get_expr(&ast.statement) {
                        // We just ignore the approximations, given we ignore lots of other type details
                        return Ty::from_expr(expr, &mut Vec::new());
                    }
                }
                Ty::Any
            }
        }
    }
}

// Returns a function that produces commas every time apart from the first
pub(crate) fn commas() -> impl FnMut(&mut fmt::Formatter<'_>) -> fmt::Result {
    let mut with_comma = false;
    move |f: &mut fmt::Formatter<'_>| -> fmt::Result {
        if with_comma {
            write!(f, ", ")?;
        }
        with_comma = true;
        Ok(())
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut comma = commas();
        match self {
            Ty::Void => write!(f, "Void"),
            Ty::Any => write!(f, "\"\""),
            Ty::Union(xs) => write!(f, "{}", xs),
            Ty::Name(x) => write!(f, "{}", x),
            Ty::None => write!(f, "None"),
            Ty::Iter(x) => write!(f, "iter({})", x),
            Ty::List(x) => write!(f, "[{}]", x),
            Ty::Tuple(xs) => {
                write!(f, "(")?;
                for x in xs {
                    comma(f)?;
                    write!(f, "{}", x)?;
                }
                if xs.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            Ty::Dict(k_v) => write!(f, "{{{}: {}}}", k_v.0, k_v.1),
            Ty::Struct { fields, extra } => {
                write!(f, "struct(")?;
                for (k, v) in fields {
                    comma(f)?;
                    write!(f, "{} = {}", k, v)?;
                }
                if *extra {
                    comma(f)?;
                    write!(f, "..")?;
                }
                write!(f, ")")
            }
            Ty::Function(TyFunction {
                name,
                params,
                result,
                ..
            }) => {
                write!(f, "def{}{}(", if name.is_empty() { "" } else { " " }, name)?;
                for param in params {
                    comma(f)?;
                    let opt = if param.optional { "=.." } else { "" };
                    match &param.mode {
                        ParamMode::PosOnly => write!(f, "#: {}{}", param.ty, opt)?,
                        ParamMode::PosOrName(name) => write!(f, "#{}: {}{}", name, param.ty, opt)?,
                        ParamMode::NameOnly(name) => write!(f, "{}: {}{}", name, param.ty, opt)?,
                        ParamMode::Args => write!(f, "*args: {}", param.ty)?,
                        ParamMode::Kwargs => write!(f, "**kwargs: {}", param.ty)?,
                    }
                }
                write!(f, ") -> {}", result)
            }
        }
    }
}
