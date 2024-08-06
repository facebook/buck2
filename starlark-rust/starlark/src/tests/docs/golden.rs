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

use starlark_syntax::golden_test_template::golden_test_template;

use crate::docs::markdown::render_doc_item;
use crate::docs::DocItem;

pub(crate) fn docs_golden_test(test_file_name: &str, doc: DocItem) -> String {
    assert!(test_file_name.ends_with(".golden.md"));
    assert!(!test_file_name.contains('/'));

    let output = render_doc_item("name", &doc);

    golden_test_template(&format!("src/tests/docs/golden/{test_file_name}"), &output);

    output
}
