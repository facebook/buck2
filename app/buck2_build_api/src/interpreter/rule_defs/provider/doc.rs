/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use itertools::Itertools;
use starlark::docs::DocFunction;
use starlark::docs::DocItem;
use starlark::docs::DocMember;
use starlark::docs::DocProperty;
use starlark::docs::DocString;
use starlark::docs::DocType;
use starlark::environment::GlobalsBuilder;
use starlark::typing::Ty;

pub fn provider_callable_documentation(
    creator: Option<for<'a> fn(&'a mut GlobalsBuilder)>,
    self_ty: Ty,
    overall: &Option<DocString>,
    fields: &[&str],
    field_docs: &[Option<DocString>],
    field_types: &[Ty],
) -> DocItem {
    let members = itertools::izip!(fields.iter(), field_docs.iter(), field_types.iter())
        .map(|(name, docs, return_type)| {
            let prop = DocProperty {
                docs: docs.clone(),
                typ: return_type.clone(),
            };
            (*name, prop)
        })
        .collect::<Vec<_>>();

    let ctor = match creator {
        Some(creator) => {
            let docs = GlobalsBuilder::new().with(creator).build().documentation();
            if docs.members.len() == 1 {
                match docs.members.into_iter().next() {
                    Some((name, DocItem::Member(DocMember::Function(x)))) => Some((name, x)),
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    };

    match ctor {
        None => DocItem::Type(DocType {
            docs: overall.clone(),
            ty: self_ty,
            members: members
                .into_iter()
                .map(|(a, b)| (a.to_owned(), DocMember::Property(b)))
                .collect(),
            constructor: None,
        }),
        Some((_name, DocFunction { docs, params, ret })) => {
            let summary = if let Some(x) = &docs {
                x.summary.clone()
            } else if let Some(x) = &overall {
                x.summary.clone()
            } else {
                "A provider that can be constructed and have its fields accessed. Returned by rules.".to_owned()
            };
            let mut details = vec![
                docs.as_ref().and_then(|x| x.details.clone()),
                if docs.is_some() {
                    overall.as_ref().map(|x| x.summary.clone())
                } else {
                    None
                },
                overall.as_ref().and_then(|x| x.details.clone()),
                Some("Provides a number of fields that can be accessed:".to_owned()),
            ];
            for (name, member) in members {
                let typ = member.typ.to_string();
                let description = member.docs.map_or_else(
                    || "field".to_owned(),
                    |x| x.summary + &x.details.unwrap_or_default(),
                );
                details.push(Some(format!("* `{name}: {typ}` - {description}")));
            }
            let docs = Some(DocString {
                summary,
                details: Some(details.iter().flatten().join("\n\n")),
            });
            DocItem::Member(DocMember::Function(DocFunction { docs, params, ret }))
        }
    }
}
