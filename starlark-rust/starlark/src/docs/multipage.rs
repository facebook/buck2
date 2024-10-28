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

use crate::docs::markdown::render_doc_type;
use crate::docs::markdown::render_members;
use crate::docs::DocItem;
use crate::docs::DocModule;

/// Renders the contents of a `DocModule` into a multi-page tree structure
///
/// The output will contain page-paths like ``, `type1`, `mod1`, and `mod1/type2`, each mapped to
/// the contents of that page. That means that some of the paths may be prefixes of each other,
/// which will need consideration if this is to be materialized to a filesystem.
pub fn render_markdown_multipage(docs: DocModule, name: &str) -> HashMap<String, String> {
    render_markdown_multipage_inner(docs, name, "")
}

fn render_markdown_multipage_inner(
    docs: DocModule,
    name: &str,
    base_path: &str,
) -> HashMap<String, String> {
    let mut result = HashMap::new();
    let mut members = Vec::new();

    for (name, doc) in docs.members {
        let path = if base_path.is_empty() {
            name.clone()
        } else {
            format!("{}/{}", base_path, name)
        };
        match doc {
            DocItem::Module(m) => {
                result.extend(render_markdown_multipage_inner(m, &name, &path));
            }
            DocItem::Type(t) => {
                let rendered = render_doc_type(&name, &format!("{name}."), &t);
                result.insert(path, rendered);
            }
            DocItem::Member(m) => {
                members.push((name, m));
            }
        }
    }

    let rendered = render_members(
        name,
        &docs.docs,
        "",
        members.iter().map(|(n, m)| (&**n, m.clone())),
        None,
    );

    result.insert(base_path.to_owned(), rendered);

    result
}
