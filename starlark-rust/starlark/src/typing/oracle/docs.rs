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

use std::collections::HashMap;

use starlark_map::small_map::SmallMap;

use crate::docs;
use crate::docs::Doc;
use crate::docs::DocItem;
use crate::docs::Member;
use crate::typing::Ty;
use crate::typing::TypingOracle;

/// A [`TypingOracle`] based on information from documentation.
#[derive(Default)]
pub struct OracleDocs {
    /// Indexed by type name, then the attribute
    objects: HashMap<String, HashMap<String, Ty>>,
    functions: HashMap<String, Ty>,
}

impl OracleDocs {
    /// Create a new [`OracleDocs`], usually given the output of
    /// [`get_registered_starlark_docs`](crate::docs::get_registered_starlark_docs).
    pub fn new(docs: &[Doc]) -> Self {
        let mut res = Self::default();
        for doc in docs {
            res.add_doc(doc);
        }
        res
    }

    /// Like [`Self::new`], but adding to an existing oracle (overwriting any duplicates).
    pub fn add_doc(&mut self, doc: &Doc) {
        fn add_members(me: &mut OracleDocs, doc: &Doc, members: &SmallMap<String, Member>) {
            let mut items = HashMap::with_capacity(members.len());
            for (name, member) in members {
                items.insert(name.clone(), Ty::from_docs_member(member));
            }
            me.objects.insert(doc.id.name.clone(), items);
        }

        match &doc.item {
            DocItem::Module(modu) => add_members(self, doc, &modu.members),
            DocItem::Object(obj) => add_members(self, doc, &obj.members),
            DocItem::Property(x) => {
                self.functions
                    .insert(doc.id.name.clone(), Ty::from_docs_property(x));
            }
            DocItem::Function(x) => {
                self.functions
                    .insert(doc.id.name.clone(), Ty::from_docs_function(x));
            }
        }
    }

    /// Create a new [`OracleDocs`] given the documentation, usually from
    /// [`Globals::documentation`](crate::environment::Globals::documentation).
    /// Only produces interesting content if the result is an [`Object`](crate::docs::Object) (which it is for those two).
    pub fn new_object(docs: &DocItem) -> Self {
        let mut res = Self::default();
        res.add_object(docs);
        res
    }

    /// Like [`Self::new_object`], but adding to an existing oracle (overwriting any duplicates).
    pub fn add_object(&mut self, docs: &DocItem) {
        match docs {
            DocItem::Object(docs::Object { members, .. })
            | DocItem::Module(docs::Module { members, .. }) => {
                for (name, member) in members {
                    self.functions
                        .insert(name.clone(), Ty::from_docs_member(member));
                }
            }
            _ => {}
        }
    }
}

impl TypingOracle for OracleDocs {
    fn attribute(&self, ty: &Ty, attr: &str) -> Option<Result<Ty, ()>> {
        if attr.starts_with("__") && attr.ends_with("__") {
            // We don't record operator info in the docs, so it is always missing
            return None;
        }
        let name = match ty {
            Ty::Name(x) => x.as_str(),
            Ty::List(_) => "list",
            Ty::Tuple(_) => "tuple",
            Ty::Dict(_) => "dict",
            Ty::Struct { .. } => "struct",
            _ => return None,
        };
        match self.objects.get(name)?.get(attr) {
            None => Some(Err(())),
            Some(res) => Some(Ok(res.clone())),
        }
    }

    fn builtin(&self, name: &str) -> Option<Result<Ty, ()>> {
        Some(Ok(self.functions.get(name)?.clone()))
    }
}
