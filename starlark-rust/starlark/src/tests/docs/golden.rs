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

use std::env;
use std::fmt::Write;
use std::fs;

use anyhow::Context;

use crate::docs::Doc;
use crate::docs::DocItem;
use crate::docs::MarkdownFlavor;
use crate::docs::RenderMarkdown;

const REGENERATE_VAR_NAME: &str = "STARLARK_RUST_REGENERATE_DOC_TESTS";

#[allow(clippy::write_literal)] // We mark generated files as generated, but not this file.
fn make_golden(item: DocItem) -> String {
    let doc = Doc::named_item("name".to_owned(), item);
    let markdown = doc.render_markdown(MarkdownFlavor::DocFile);

    let mut golden = String::new();
    writeln!(golden, "# {at}generated", at = "@").unwrap();
    writeln!(golden, "# To regenerate, run:").unwrap();
    writeln!(golden, "# ```").unwrap();
    writeln!(
        golden,
        "# {REGENERATE_VAR_NAME}=1 cargo test -p starlark --lib tests"
    )
    .unwrap();
    writeln!(golden, "# ```").unwrap();
    writeln!(golden).unwrap();
    writeln!(golden, "{}", markdown).unwrap();
    golden
}

pub(crate) fn docs_golden_test(test_name: &str, doc: DocItem) -> String {
    let manifest_dir =
        env::var("CARGO_MANIFEST_DIR").expect("`CARGO_MANIFEST_DIR` variable must be set");

    let golden_file_name = format!("{manifest_dir}/src/tests/docs/golden/{test_name}.golden.md");

    let actual = make_golden(doc);
    if env::var(REGENERATE_VAR_NAME).is_ok() {
        fs::write(golden_file_name, &actual).unwrap();
    } else {
        let expected = fs::read_to_string(&golden_file_name)
            .with_context(|| format!("Reading `{golden_file_name}`"))
            .unwrap();
        let expected = if cfg!(target_os = "windows") {
            // Git may check out files on Windows with \r\n as line separator.
            // We could configure git, but it's more reliable to handle it in the test.
            expected.replace("\r\n", "\n")
        } else {
            expected
        };
        assert_eq!(expected, actual);
    }
    actual
}
