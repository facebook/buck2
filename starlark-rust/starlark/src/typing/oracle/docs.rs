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

use crate::docs::Doc;
use crate::docs::DocItem;
use crate::docs::DocModule;
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
    /// Create a new [`OracleDocs`] with no information.
    /// You can then call the various methods to populate it with data.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add multiple entries to the documentation, usually given the results of
    /// [`get_registered_starlark_docs`](crate::docs::get_registered_starlark_docs).
    pub fn add_docs(&mut self, docs: &[Doc]) {
        for doc in docs {
            self.add_doc(doc);
        }
    }

    /// Add information from a [`Doc`] to the documentation, overwriting existing items.
    pub fn add_doc(&mut self, doc: &Doc) {
        match &doc.item {
            DocItem::Module(modu) => self.add_module(modu),
            DocItem::Object(obj) => {
                let mut items = HashMap::with_capacity(obj.members.len());
                for (name, member) in &obj.members {
                    items.insert(name.clone(), Ty::from_docs_member(member));
                }
                self.objects.insert(doc.id.name.clone(), items);
            }
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

    /// Add documentation usually from
    /// [`Globals::documentation`](crate::environment::Globals::documentation),
    /// overwriting any duplicates.
    pub fn add_module(&mut self, docs: &DocModule) {
        for (name, member) in &docs.members {
            self.functions
                .insert(name.clone(), Ty::from_docs_member(member));
        }
    }

    /// Is information known about this object.
    pub(crate) fn known_object(&self, name: &str) -> bool {
        self.objects.contains_key(name)
    }
}

impl TypingOracle for OracleDocs {
    fn attribute(&self, ty: &Ty, attr: &str) -> Option<Result<Ty, ()>> {
        if attr.starts_with("__") && attr.ends_with("__") {
            // We don't record operator info in the docs, so it is always missing
            return None;
        }
        match self.objects.get(ty.as_name()?)?.get(attr) {
            None => Some(Err(())),
            Some(res) => Some(Ok(res.clone())),
        }
    }

    fn builtin(&self, name: &str) -> Option<Result<Ty, ()>> {
        Some(Ok(self.functions.get(name)?.clone()))
    }
}
