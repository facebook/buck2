/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::provider::id::ProviderId;
use dupe::Dupe;
use itertools::Itertools;
use starlark::any::ProvidesStaticType;
use starlark::docs::DocFunction;
use starlark::docs::DocItem;
use starlark::docs::DocMember;
use starlark::docs::DocObject;
use starlark::docs::DocProperty;
use starlark::docs::DocReturn;
use starlark::docs::DocString;
use starlark::docs::DocType;
use starlark::environment::GlobalsBuilder;
use starlark::values::ValueLike;

#[derive(Debug, thiserror::Error)]
enum ProviderCallableError {
    #[error("provider callable did not have a bound id; this is an internal error")]
    ProviderCallableMissingID,
}

pub trait ProviderCallableLike {
    fn id(&self) -> Option<&Arc<ProviderId>>;

    /// Frozen callables should always have this set. It's an error if somehow it doesn't.
    fn require_id(&self) -> anyhow::Result<Arc<ProviderId>> {
        match self.id() {
            Some(id) => Ok(id.dupe()),
            None => Err(ProviderCallableError::ProviderCallableMissingID.into()),
        }
    }

    fn provider_callable_documentation(
        &self,
        creator: Option<for<'a> fn(&'a mut GlobalsBuilder)>,
        overall: &Option<DocString>,
        fields: &[&str],
        field_docs: &[Option<DocString>],
        field_types: &[Option<DocType>],
    ) -> Option<DocItem> {
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
                match docs {
                    DocItem::Module(x) if x.members.len() == 1 => {
                        match x.members.into_iter().next() {
                            Some((name, DocMember::Function(x))) => Some((name, x)),
                            _ => None,
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        };

        match ctor {
            None => Some(DocItem::Object(DocObject {
                docs: overall.clone(),
                members: members
                    .into_iter()
                    .map(|(a, b)| (a.to_owned(), DocMember::Property(b)))
                    .collect(),
            })),
            Some((name, DocFunction { docs, params, ret })) => {
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
                    let typ = member.typ.map_or_else(|| "\"\"".to_owned(), |x| x.raw_type);
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
                let ret = DocReturn {
                    docs: ret.docs,
                    typ: Some(DocType {
                        raw_type: format!("{name}.type"),
                    }),
                };
                Some(DocItem::Function(DocFunction { docs, params, ret }))
            }
        }
    }
}

unsafe impl<'v> ProvidesStaticType for &'v dyn ProviderCallableLike {
    type StaticType = &'static dyn ProviderCallableLike;
}

pub trait ValueAsProviderCallableLike<'v> {
    fn as_provider_callable(&self) -> Option<&'v dyn ProviderCallableLike>;
}

impl<'v, V: ValueLike<'v>> ValueAsProviderCallableLike<'v> for V {
    fn as_provider_callable(&self) -> Option<&'v dyn ProviderCallableLike> {
        self.to_value().request_value::<&dyn ProviderCallableLike>()
    }
}
