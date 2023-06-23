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

use crate::docs::Doc;
use crate::docs::DocItem;
use crate::docs::MarkdownFlavor;
use crate::docs::RenderMarkdown;
use crate::tests::golden_test_template::golden_test_template;

pub(crate) fn docs_golden_test(test_name: &str, doc: DocItem) -> String {
    let output = Doc::named_item("name".to_owned(), doc).render_markdown(MarkdownFlavor::DocFile);

    golden_test_template(
        &format!("src/tests/docs/golden/{test_name}.golden.md"),
        &output,
    );

    output
}
