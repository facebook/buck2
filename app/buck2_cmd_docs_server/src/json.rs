/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use dupe::Dupe;
use serde::Serialize;
use starlark::collections::SmallMap;
use starlark::typing::Ty;

fn serialize_ty<S: serde::Serializer>(ty: &Ty, s: S) -> Result<S::Ok, S::Error> {
    s.serialize_str(&ty.to_string())
}

fn serialize_opt_ty<S: serde::Serializer>(ty: &Option<Ty>, s: S) -> Result<S::Ok, S::Error> {
    match ty {
        Some(ty) => serialize_ty(ty, s),
        None => s.serialize_none(),
    }
}

#[derive(Serialize)]
struct JsonDoc {
    id: JsonIdentifier,
    item: JsonDocItem,
    custom_attrs: HashMap<String, String>,
}

impl JsonDoc {
    fn from_starlark(doc: starlark::docs::Doc) -> Self {
        Self {
            id: JsonIdentifier::from_starlark(doc.id),
            item: JsonDocItem::from_starlark(doc.item),
            custom_attrs: doc.custom_attrs,
        }
    }
}

#[derive(Serialize)]
struct JsonIdentifier {
    name: String,
    location: Option<JsonLocation>,
}

impl JsonIdentifier {
    fn from_starlark(id: starlark::docs::Identifier) -> Self {
        Self {
            name: id.name,
            location: id.location.map(JsonLocation::from_starlark),
        }
    }
}

#[derive(Serialize)]
struct JsonLocation {
    path: String,
}

impl JsonLocation {
    fn from_starlark(loc: starlark::docs::Location) -> Self {
        Self { path: loc.path }
    }
}

#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum JsonDocItem {
    Module(JsonDocModule),
    Object(JsonDocObject),
    Function(JsonDocFunction),
    Property(JsonDocProperty),
}

impl JsonDocItem {
    fn from_starlark(item: starlark::docs::DocItem) -> Self {
        match item {
            starlark::docs::DocItem::Module(m) => Self::Module(JsonDocModule::from_starlark(m)),
            starlark::docs::DocItem::Type(o) => Self::Object(JsonDocObject::from_starlark(o)),
            starlark::docs::DocItem::Member(starlark::docs::DocMember::Function(f)) => {
                Self::Function(JsonDocFunction::from_starlark(f))
            }
            starlark::docs::DocItem::Member(starlark::docs::DocMember::Property(p)) => {
                Self::Property(JsonDocProperty::from_starlark(p))
            }
        }
    }
}

#[derive(Serialize)]
struct JsonDocModule {
    docs: Option<JsonDocString>,
    members: SmallMap<String, JsonDocMember>,
}

impl JsonDocModule {
    fn from_starlark(m: starlark::docs::DocModule) -> Self {
        Self {
            docs: m.docs.map(JsonDocString::from_starlark),
            members: m
                .members
                .into_iter()
                .filter_map(|(k, v)| match v {
                    starlark::docs::DocItem::Member(v) => {
                        Some((k, JsonDocMember::from_starlark(v)))
                    }
                    _ => None,
                })
                .collect(),
        }
    }
}

#[derive(Serialize)]
struct JsonDocObject {
    docs: Option<JsonDocString>,
    members: SmallMap<String, JsonDocMember>,
}

impl JsonDocObject {
    fn from_starlark(o: starlark::docs::DocType) -> Self {
        Self {
            docs: o.docs.map(JsonDocString::from_starlark),
            members: o
                .members
                .into_iter()
                .map(|(k, v)| (k, JsonDocMember::from_starlark(v)))
                .collect(),
        }
    }
}

#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum JsonDocMember {
    Property(JsonDocProperty),
    Function(JsonDocFunction),
}

impl JsonDocMember {
    fn from_starlark(m: starlark::docs::DocMember) -> Self {
        match m {
            starlark::docs::DocMember::Property(p) => {
                Self::Property(JsonDocProperty::from_starlark(p))
            }
            starlark::docs::DocMember::Function(f) => {
                Self::Function(JsonDocFunction::from_starlark(f))
            }
        }
    }
}

/// A single property of an object. These are explicitly not functions (see [`DocMember`]).
#[derive(Serialize)]
struct JsonDocProperty {
    docs: Option<JsonDocString>,
    #[serde(rename = "type", serialize_with = "serialize_ty")]
    typ: Ty,
}

impl JsonDocProperty {
    fn from_starlark(p: starlark::docs::DocProperty) -> Self {
        Self {
            docs: p.docs.map(JsonDocString::from_starlark),
            typ: p.typ,
        }
    }
}

#[derive(Serialize)]
struct JsonDocFunction {
    docs: Option<JsonDocString>,
    params: Vec<JsonDocParam>,
    ret: JsonDocReturn,
    #[serde(serialize_with = "serialize_opt_ty")]
    as_type: Option<Ty>,
}

impl JsonDocFunction {
    fn from_starlark(f: starlark::docs::DocFunction) -> Self {
        Self {
            docs: f.docs.map(JsonDocString::from_starlark),
            params: f
                .params
                .fmt_params()
                .map(JsonDocParam::from_starlark)
                .collect(),
            ret: JsonDocReturn::from_starlark(f.ret),
            as_type: f.as_type,
        }
    }
}

#[derive(Serialize)]
struct JsonDocReturn {
    docs: Option<JsonDocString>,
    #[serde(rename = "type", serialize_with = "serialize_ty")]
    typ: Ty,
}

impl JsonDocReturn {
    fn from_starlark(ret: starlark::docs::DocReturn) -> Self {
        Self {
            docs: ret.docs.map(JsonDocString::from_starlark),
            typ: ret.typ,
        }
    }
}

/// A single parameter of a function.
#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum JsonDocParam {
    Arg {
        name: String,
        docs: Option<JsonDocString>,
        #[serde(rename = "type", serialize_with = "serialize_ty")]
        typ: Ty,
        default_value: Option<String>,
    },
    OnlyNamedAfter,
    OnlyPosBefore,
    Args {
        name: String,
        docs: Option<JsonDocString>,
        #[serde(rename = "type", serialize_with = "serialize_ty")]
        tuple_elem_ty: Ty,
    },
    Kwargs {
        name: String,
        docs: Option<JsonDocString>,
        #[serde(rename = "type", serialize_with = "serialize_ty")]
        dict_value_ty: Ty,
    },
}

impl JsonDocParam {
    fn from_starlark(param: starlark::docs::FmtParam<&'_ starlark::docs::DocParam>) -> Self {
        match param {
            starlark::docs::FmtParam::Regular(starlark::docs::DocParam {
                name,
                docs,
                typ,
                default_value,
            }) => Self::Arg {
                name: name.clone(),
                docs: docs.clone().map(JsonDocString::from_starlark),
                typ: typ.dupe(),
                default_value: default_value.clone(),
            },
            starlark::docs::FmtParam::Slash => Self::OnlyPosBefore,
            starlark::docs::FmtParam::Star => Self::OnlyNamedAfter,
            starlark::docs::FmtParam::Args(starlark::docs::DocParam {
                name,
                docs,
                typ,
                default_value: _,
            }) => Self::Args {
                name: name.clone(),
                docs: docs.clone().map(JsonDocString::from_starlark),
                tuple_elem_ty: typ.dupe(),
            },
            starlark::docs::FmtParam::Kwargs(starlark::docs::DocParam {
                name,
                docs,
                typ,
                default_value: _,
            }) => Self::Kwargs {
                name: name.clone(),
                docs: docs.clone().map(JsonDocString::from_starlark),
                dict_value_ty: typ.dupe(),
            },
        }
    }
}

#[derive(Serialize)]
struct JsonDocString {
    summary: String,
    details: Option<String>,
}

impl JsonDocString {
    fn from_starlark(s: starlark::docs::DocString) -> Self {
        Self {
            summary: s.summary,
            details: s.details,
        }
    }
}

pub(crate) fn to_json(docs: Vec<starlark::docs::Doc>) -> anyhow::Result<String> {
    let docs: Vec<_> = docs.into_iter().map(JsonDoc::from_starlark).collect();
    Ok(serde_json::to_string(&docs)?)
}
