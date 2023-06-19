/*
 * Copyright 2018 The Starlark in Rust Authors.
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

//! AST for parsed starlark files.

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use dupe::Dupe;

use crate::codemap::Pos;
use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::syntax::lexer::TokenInt;

/// Payload types attached to AST nodes.
pub(crate) trait AstPayload: Debug {
    type LoadPayload: Debug;
    type IdentPayload: Debug;
    type IdentAssignPayload: Debug;
    type DefPayload: Debug;
    type TypeExprPayload: Debug;
}

/// Default implementation of payload, which attaches `()` to nodes.
/// This payload is returned with AST by parser.
#[derive(Debug, Copy, Clone, Dupe, Eq, PartialEq)]
pub(crate) struct AstNoPayload;
impl AstPayload for AstNoPayload {
    type LoadPayload = ();
    type IdentPayload = ();
    type IdentAssignPayload = ();
    type DefPayload = ();
    type TypeExprPayload = ();
}

pub(crate) type Expr = ExprP<AstNoPayload>;
pub(crate) type TypeExpr = TypeExprP<AstNoPayload>;
pub(crate) type Assign = AssignP<AstNoPayload>;
pub(crate) type AssignIdent = AssignIdentP<AstNoPayload>;
pub(crate) type Ident = IdentP<AstNoPayload>;
pub(crate) type Clause = ClauseP<AstNoPayload>;
pub(crate) type ForClause = ForClauseP<AstNoPayload>;
pub(crate) type Argument = ArgumentP<AstNoPayload>;
pub(crate) type Parameter = ParameterP<AstNoPayload>;
pub(crate) type Load = LoadP<AstNoPayload>;
pub(crate) type Stmt = StmtP<AstNoPayload>;

// Boxed types used for storing information from the parsing will be used
// especially for the location of the AST item
pub(crate) type AstExprP<P> = Spanned<ExprP<P>>;
pub(crate) type AstTypeExprP<P> = Spanned<TypeExprP<P>>;
pub(crate) type AstAssignP<P> = Spanned<AssignP<P>>;
pub(crate) type AstAssignIdentP<P> = Spanned<AssignIdentP<P>>;
pub(crate) type AstIdentP<P> = Spanned<IdentP<P>>;
pub(crate) type AstArgumentP<P> = Spanned<ArgumentP<P>>;
pub(crate) type AstParameterP<P> = Spanned<ParameterP<P>>;
pub(crate) type AstLoadP<P> = Spanned<LoadP<P>>;
pub(crate) type AstStmtP<P> = Spanned<StmtP<P>>;

pub(crate) type AstExpr = AstExprP<AstNoPayload>;
pub(crate) type AstTypeExpr = AstTypeExprP<AstNoPayload>;
pub(crate) type AstAssign = AstAssignP<AstNoPayload>;
pub(crate) type AstAssignIdent = AstAssignIdentP<AstNoPayload>;
pub(crate) type AstIdent = AstIdentP<AstNoPayload>;
pub(crate) type AstArgument = AstArgumentP<AstNoPayload>;
pub(crate) type AstString = Spanned<String>;
pub(crate) type AstParameter = AstParameterP<AstNoPayload>;
pub(crate) type AstInt = Spanned<TokenInt>;
pub(crate) type AstFloat = Spanned<f64>;
pub(crate) type AstStmt = AstStmtP<AstNoPayload>;

// A trait rather than a function to allow .ast() chaining in the parser.
pub(crate) trait ToAst: Sized {
    fn ast(self, begin: usize, end: usize) -> Spanned<Self> {
        Spanned {
            span: Span::new(Pos::new(begin as u32), Pos::new(end as u32)),
            node: self,
        }
    }
}

impl<T> ToAst for T {}

#[derive(Debug)]
pub(crate) enum ArgumentP<P: AstPayload> {
    Positional(AstExprP<P>),
    Named(AstString, AstExprP<P>),
    Args(AstExprP<P>),
    KwArgs(AstExprP<P>),
}

#[derive(Debug)]
pub(crate) enum ParameterP<P: AstPayload> {
    Normal(AstAssignIdentP<P>, Option<Box<AstTypeExprP<P>>>),
    WithDefaultValue(
        AstAssignIdentP<P>,
        Option<Box<AstTypeExprP<P>>>,
        Box<AstExprP<P>>,
    ),
    NoArgs,
    Args(AstAssignIdentP<P>, Option<Box<AstTypeExprP<P>>>),
    KwArgs(AstAssignIdentP<P>, Option<Box<AstTypeExprP<P>>>),
}

#[derive(Debug, Clone)]
pub(crate) enum AstLiteral {
    Int(AstInt),
    Float(AstFloat),
    String(AstString),
}

#[derive(Debug)]
pub(crate) struct LambdaP<P: AstPayload> {
    pub(crate) params: Vec<AstParameterP<P>>,
    pub(crate) body: Box<AstExprP<P>>,
    pub(crate) payload: P::DefPayload,
}

impl<P: AstPayload> LambdaP<P> {
    pub(crate) fn signature_span(&self) -> Span {
        self.params
            .iter()
            .map(|p| p.span)
            .reduce(|a, b| a.merge(b))
            .unwrap_or(
                // TODO(nga): this is not correct span.
                self.body.span,
            )
    }
}

#[derive(Debug)]
pub(crate) enum ExprP<P: AstPayload> {
    Tuple(Vec<AstExprP<P>>),
    Dot(Box<AstExprP<P>>, AstString),
    Call(Box<AstExprP<P>>, Vec<AstArgumentP<P>>),
    ArrayIndirection(Box<(AstExprP<P>, AstExprP<P>)>),
    Slice(
        Box<AstExprP<P>>,
        Option<Box<AstExprP<P>>>,
        Option<Box<AstExprP<P>>>,
        Option<Box<AstExprP<P>>>,
    ),
    Identifier(AstIdentP<P>),
    Lambda(LambdaP<P>),
    Literal(AstLiteral),
    Not(Box<AstExprP<P>>),
    Minus(Box<AstExprP<P>>),
    Plus(Box<AstExprP<P>>),
    BitNot(Box<AstExprP<P>>),
    Op(Box<AstExprP<P>>, BinOp, Box<AstExprP<P>>),
    If(Box<(AstExprP<P>, AstExprP<P>, AstExprP<P>)>), // Order: condition, v1, v2 <=> v1 if condition else v2
    List(Vec<AstExprP<P>>),
    Dict(Vec<(AstExprP<P>, AstExprP<P>)>),
    ListComprehension(Box<AstExprP<P>>, Box<ForClauseP<P>>, Vec<ClauseP<P>>),
    DictComprehension(
        Box<(AstExprP<P>, AstExprP<P>)>,
        Box<ForClauseP<P>>,
        Vec<ClauseP<P>>,
    ),
}

/// Restricted expression at type position.
#[derive(Debug)]
pub(crate) struct TypeExprP<P: AstPayload> {
    /// Currently it is an expr.
    /// Planning to restrict it.
    /// [Context](https://fb.workplace.com/groups/buck2eng/posts/3196541547309990).
    pub(crate) expr: AstExprP<P>,
    pub(crate) payload: P::TypeExprPayload,
}

/// In some places e.g. AssignModify, the Tuple case is not allowed.
#[derive(Debug)]
pub(crate) enum AssignP<P: AstPayload> {
    // We use Tuple for both Tuple and List,
    // as these have the same semantics in Starlark.
    Tuple(Vec<AstAssignP<P>>),
    ArrayIndirection(Box<(AstExprP<P>, AstExprP<P>)>),
    Dot(Box<AstExprP<P>>, AstString),
    Identifier(AstAssignIdentP<P>),
}

/// Identifier in assign position.
#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct AssignIdentP<P: AstPayload>(pub String, pub P::IdentAssignPayload);

/// Identifier in read position, e. g. `foo` in `[foo.bar]`.
/// `foo` in `foo = 1` or `bar.foo` are **not** represented by this type.
#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct IdentP<P: AstPayload>(pub String, pub P::IdentPayload);

/// `load` statement.
#[derive(Debug)]
pub(crate) struct LoadP<P: AstPayload> {
    pub(crate) module: AstString,
    pub(crate) args: Vec<(AstAssignIdentP<P>, AstString)>,
    pub(crate) payload: P::LoadPayload,
}

#[derive(Debug)]
pub(crate) struct ForClauseP<P: AstPayload> {
    pub(crate) var: AstAssignP<P>,
    pub(crate) over: AstExprP<P>,
}

#[derive(Debug)]
pub(crate) enum ClauseP<P: AstPayload> {
    For(ForClauseP<P>),
    If(AstExprP<P>),
}

#[derive(Debug, Clone, Copy, Dupe, Eq, PartialEq)]
pub(crate) enum BinOp {
    Or,
    And,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    In,
    NotIn,
    Subtract,
    Add,
    Multiply,
    Percent,
    Divide,
    FloorDivide,
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
}

#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq)]
pub(crate) enum AssignOp {
    Add,         // +=
    Subtract,    // -=
    Multiply,    // *=
    Divide,      // /=
    FloorDivide, // //=
    Percent,     // %=
    BitAnd,      // &=
    BitOr,       // |=
    BitXor,      // ^=
    LeftShift,   // <<=
    RightShift,  // >>=
}

#[derive(Debug, Copy, Clone, Dupe, Eq, PartialEq, Allocative)]
pub enum Visibility {
    Private,
    Public,
}

#[derive(Debug)]
pub(crate) struct DefP<P: AstPayload> {
    pub(crate) name: AstAssignIdentP<P>,
    pub(crate) params: Vec<AstParameterP<P>>,
    pub(crate) return_type: Option<Box<AstTypeExprP<P>>>,
    pub(crate) body: Box<AstStmtP<P>>,
    pub(crate) payload: P::DefPayload,
}

impl<P: AstPayload> DefP<P> {
    pub(crate) fn signature_span(&self) -> Span {
        let mut span = self.name.span;
        for param in &self.params {
            span = span.merge(param.span);
        }
        if let Some(return_type) = &self.return_type {
            span = span.merge(return_type.span);
        }
        span
    }
}

#[derive(Debug)]
pub(crate) enum StmtP<P: AstPayload> {
    Break,
    Continue,
    Pass,
    Return(Option<AstExprP<P>>),
    Expression(AstExprP<P>),
    // LHS : TYPE = RHS for the fields
    Assign(AstAssignP<P>, Box<(Option<AstTypeExprP<P>>, AstExprP<P>)>),
    AssignModify(AstAssignP<P>, AssignOp, Box<AstExprP<P>>),
    Statements(Vec<AstStmtP<P>>),
    If(AstExprP<P>, Box<AstStmtP<P>>),
    IfElse(AstExprP<P>, Box<(AstStmtP<P>, AstStmtP<P>)>),
    For(AstAssignP<P>, Box<(AstExprP<P>, AstStmtP<P>)>),
    Def(DefP<P>),
    Load(LoadP<P>),
}

impl<P: AstPayload> StmtP<P> {
    /// Ensure we produce normalised Statements, rather than singleton Statements
    pub(crate) fn statements(mut xs: Vec<AstStmtP<P>>, begin: usize, end: usize) -> AstStmtP<P> {
        if xs.len() == 1 {
            xs.pop().unwrap()
        } else {
            Self::Statements(xs).ast(begin, end)
        }
    }
}

impl<P: AstPayload> ArgumentP<P> {
    pub(crate) fn expr(&self) -> &AstExprP<P> {
        match self {
            ArgumentP::Positional(x) => x,
            ArgumentP::Named(_, x) => x,
            ArgumentP::Args(x) => x,
            ArgumentP::KwArgs(x) => x,
        }
    }

    pub(crate) fn expr_mut(&mut self) -> &mut AstExprP<P> {
        match self {
            ArgumentP::Positional(x) => x,
            ArgumentP::Named(_, x) => x,
            ArgumentP::Args(x) => x,
            ArgumentP::KwArgs(x) => x,
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            BinOp::Or => f.write_str(" or "),
            BinOp::And => f.write_str(" and "),
            BinOp::Equal => f.write_str(" == "),
            BinOp::NotEqual => f.write_str(" != "),
            BinOp::Less => f.write_str(" < "),
            BinOp::Greater => f.write_str(" > "),
            BinOp::LessOrEqual => f.write_str(" <= "),
            BinOp::GreaterOrEqual => f.write_str(" >= "),
            BinOp::In => f.write_str(" in "),
            BinOp::NotIn => f.write_str(" not in "),
            BinOp::Subtract => f.write_str(" - "),
            BinOp::Add => f.write_str(" + "),
            BinOp::Multiply => f.write_str(" * "),
            BinOp::Percent => f.write_str(" % "),
            BinOp::Divide => f.write_str(" / "),
            BinOp::FloorDivide => f.write_str(" // "),
            BinOp::BitAnd => f.write_str(" & "),
            BinOp::BitOr => f.write_str(" | "),
            BinOp::BitXor => f.write_str(" ^ "),
            BinOp::LeftShift => f.write_str(" << "),
            BinOp::RightShift => f.write_str(" >> "),
        }
    }
}

impl Display for AssignOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            AssignOp::Add => f.write_str(" += "),
            AssignOp::Subtract => f.write_str(" -= "),
            AssignOp::Multiply => f.write_str(" *= "),
            AssignOp::Divide => f.write_str(" /= "),
            AssignOp::FloorDivide => f.write_str(" //= "),
            AssignOp::Percent => f.write_str(" %= "),
            AssignOp::BitAnd => f.write_str(" &= "),
            AssignOp::BitOr => f.write_str(" |= "),
            AssignOp::BitXor => f.write_str(" ^= "),
            AssignOp::LeftShift => f.write_str(" <<= "),
            AssignOp::RightShift => f.write_str(" >>= "),
        }
    }
}

fn comma_separated_fmt<I, F>(
    f: &mut Formatter<'_>,
    v: &[I],
    converter: F,
    for_tuple: bool,
) -> fmt::Result
where
    F: Fn(&I, &mut Formatter<'_>) -> fmt::Result,
{
    for (i, e) in v.iter().enumerate() {
        f.write_str(if i == 0 { "" } else { ", " })?;
        converter(e, f)?;
    }
    if v.len() == 1 && for_tuple {
        f.write_str(",")?;
    }
    Ok(())
}

fn fmt_string_literal(f: &mut Formatter<'_>, s: &str) -> fmt::Result {
    f.write_str("\"")?;
    for c in s.chars() {
        match c {
            '\n' => f.write_str("\\n")?,
            '\t' => f.write_str("\\t")?,
            '\r' => f.write_str("\\r")?,
            '\0' => f.write_str("\\0")?,
            '"' => f.write_str("\\\"")?,
            '\\' => f.write_str("\\\\")?,
            x => f.write_str(&x.to_string())?,
        }
    }
    f.write_str("\"")
}

impl Display for AstLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AstLiteral::Int(i) => write!(f, "{}", &i.node),
            AstLiteral::Float(n) => write!(f, "{}", &n.node),
            AstLiteral::String(s) => fmt_string_literal(f, &s.node),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Tuple(e) => {
                f.write_str("(")?;
                comma_separated_fmt(f, e, |x, f| write!(f, "{}", x.node), true)?;
                f.write_str(")")
            }
            Expr::Dot(e, s) => write!(f, "{}.{}", e.node, s.node),
            Expr::Lambda(LambdaP {
                params,
                body,
                payload: _,
            }) => {
                f.write_str("(lambda ")?;
                comma_separated_fmt(f, params, |x, f| write!(f, "{}", x.node), false)?;
                f.write_str(": ")?;
                write!(f, "{}", body.node)?;
                f.write_str(")")
            }
            Expr::Call(e, args) => {
                write!(f, "{}(", e.node)?;
                for (i, x) in args.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{}", x.node)?;
                }
                f.write_str(")")
            }
            Expr::ArrayIndirection(e_i) => {
                let (e, i) = &**e_i;
                write!(f, "{}[{}]", e.node, i.node)
            }
            Expr::Slice(e, i1, i2, i3) => {
                write!(f, "{}[]", e.node)?;
                if let Some(x) = i1 {
                    write!(f, "{}:", x.node)?
                } else {
                    f.write_str(":")?
                }
                if let Some(x) = i2 {
                    write!(f, "{}", x.node)?
                }
                if let Some(x) = i3 {
                    write!(f, ":{}", x.node)?
                }
                Ok(())
            }
            Expr::Identifier(s) => Display::fmt(&s.node, f),
            Expr::Not(e) => write!(f, "(not {})", e.node),
            Expr::Minus(e) => write!(f, "-{}", e.node),
            Expr::Plus(e) => write!(f, "+{}", e.node),
            Expr::BitNot(e) => write!(f, "~{}", e.node),
            Expr::Op(l, op, r) => write!(f, "({}{}{})", l.node, op, r.node),
            Expr::If(cond_v1_v2) => {
                let (cond, v1, v2) = &**cond_v1_v2;
                write!(f, "({} if {} else {})", v1.node, cond.node, v2.node)
            }
            Expr::List(v) => {
                f.write_str("[")?;
                comma_separated_fmt(f, v, |x, f| write!(f, "{}", x.node), false)?;
                f.write_str("]")
            }
            Expr::Dict(v) => {
                f.write_str("{")?;
                comma_separated_fmt(f, v, |x, f| write!(f, "{}: {}", x.0.node, x.1.node), false)?;
                f.write_str("}")
            }
            Expr::ListComprehension(e, for_, c) => {
                write!(f, "[{}", e.node)?;
                write!(f, "{}", for_)?;
                for x in c {
                    write!(f, "{}", x)?;
                }
                f.write_str("]")
            }
            Expr::DictComprehension(k_v, for_, c) => {
                let (k, v) = &**k_v;
                write!(f, "{{{}: {}", k.node, v.node)?;
                write!(f, "{}", for_)?;
                for x in c {
                    write!(f, "{}", x)?;
                }
                f.write_str("}}")
            }
            Expr::Literal(x) => write!(f, "{}", x),
        }
    }
}

impl Display for TypeExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.expr.node, f)
    }
}

impl Display for Assign {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Assign::Tuple(e) => {
                f.write_str("(")?;
                comma_separated_fmt(f, e, |x, f| write!(f, "{}", x.node), true)?;
                f.write_str(")")
            }
            Assign::Dot(e, s) => write!(f, "{}.{}", e.node, s.node),
            Assign::ArrayIndirection(e_i) => {
                let (e, i) = &**e_i;
                write!(f, "{}[{}]", e.node, i.node)
            }
            Assign::Identifier(s) => write!(f, "{}", s.node),
        }
    }
}

impl Display for AssignIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Argument::Positional(s) => write!(f, "{}", s.node),
            Argument::Named(s, e) => write!(f, "{} = {}", s.node, e.node),
            Argument::Args(s) => write!(f, "*{}", s.node),
            Argument::KwArgs(s) => write!(f, "**{}", s.node),
        }
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let (prefix, name, typ, default) = match self {
            Parameter::Normal(s, t) => ("", s, t, None),
            Parameter::WithDefaultValue(s, t, e) => ("", s, t, Some(e)),
            Parameter::NoArgs => return write!(f, "*"),
            Parameter::Args(s, t) => ("*", s, t, None),
            Parameter::KwArgs(s, t) => ("**", s, t, None),
        };
        write!(f, "{}{}", prefix, name.node)?;
        if let Some(t) = typ {
            write!(f, ": {}", t.node)?;
        }
        if let Some(d) = default {
            write!(f, " = {}", d.node)?;
        }
        Ok(())
    }
}

impl Display for ForClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, " for {} in {}", self.var.node, self.over.node)
    }
}

impl Display for Clause {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Clause::For(x) => write!(f, "{}", x),
            Clause::If(x) => write!(f, " if {}", x.node),
        }
    }
}

impl Stmt {
    fn fmt_with_tab(&self, f: &mut Formatter<'_>, tab: String) -> fmt::Result {
        match self {
            Stmt::Break => writeln!(f, "{}break", tab),
            Stmt::Continue => writeln!(f, "{}continue", tab),
            Stmt::Pass => writeln!(f, "{}pass", tab),
            Stmt::Return(Some(e)) => writeln!(f, "{}return {}", tab, e.node),
            Stmt::Return(None) => writeln!(f, "{}return", tab),
            Stmt::Expression(e) => writeln!(f, "{}{}", tab, e.node),
            Stmt::Assign(l, ty_r) => {
                let (ty, r) = &**ty_r;
                write!(f, "{}{} ", tab, l.node)?;
                if let Some(ty) = ty {
                    write!(f, ": {} ", ty.node)?;
                }
                writeln!(f, "= {}", r.node)
            }
            Stmt::AssignModify(l, op, r) => writeln!(f, "{}{}{}{}", tab, l.node, op, r.node),
            Stmt::Statements(v) => {
                for s in v {
                    s.node.fmt_with_tab(f, tab.clone())?;
                }
                Ok(())
            }
            Stmt::If(cond, suite) => {
                writeln!(f, "{}if {}:", tab, cond.node)?;
                suite.node.fmt_with_tab(f, tab + "  ")
            }
            Stmt::IfElse(cond, suite_1_2) => {
                let (suite1, suite2) = &**suite_1_2;
                writeln!(f, "{}if {}:", tab, cond.node)?;
                suite1.node.fmt_with_tab(f, tab.clone() + "  ")?;
                writeln!(f, "{}else:", tab)?;
                suite2.node.fmt_with_tab(f, tab + "  ")
            }
            Stmt::For(bind, coll_suite) => {
                let (coll, suite) = &**coll_suite;
                writeln!(f, "{}for {} in {}:", tab, bind.node, coll.node)?;
                suite.node.fmt_with_tab(f, tab + "  ")
            }
            Stmt::Def(DefP {
                name,
                params,
                return_type,
                body,
                payload: _,
            }) => {
                write!(f, "{}def {}(", tab, name.node)?;
                comma_separated_fmt(f, params, |x, f| write!(f, "{}", x.node), false)?;
                f.write_str(")")?;
                if let Some(rt) = return_type {
                    write!(f, " -> {}", rt.node)?;
                }
                f.write_str(":\n")?;
                body.node.fmt_with_tab(f, tab + "  ")
            }
            Stmt::Load(load) => {
                write!(f, "{}load(", tab)?;
                fmt_string_literal(f, &load.module.node)?;
                comma_separated_fmt(
                    f,
                    &load.args,
                    |x, f| {
                        write!(f, "{} = ", x.0.node)?;
                        fmt_string_literal(f, &(x.1.node))
                    },
                    false,
                )?;
                f.write_str(")\n")
            }
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_with_tab(f, "".to_owned())
    }
}
