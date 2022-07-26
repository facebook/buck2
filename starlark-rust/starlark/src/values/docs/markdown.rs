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

use gazebo::prelude::*;

/// The style of output that is being generated
#[derive(Copy, Clone, Dupe)]
pub enum MarkdownFlavor {
    /// A file that is written out to disk for a website or in repo.
    ///
    /// These pages are generally slightly more detailed (e.g. module summary tables at the top
    /// of the page) and have different formatting due differing use cases.
    DocFile,
    /// A summary that can be shown in the "Hover" event in the LSP.
    LspSummary,
}

/// This object can potentially generate markdown documentation about itself.
pub trait AsMarkdown {
    /// Generate markdown of the given flavor if possible. For some types, there may not be
    /// any useful documentation available.
    fn generate_markdown(&self, flavor: MarkdownFlavor) -> Option<String>;

    /// Convenience method that invokes [`AsMarkdown::generate_markdown`], and returns an
    /// empty string if that is `None`
    fn generate_markdown_or_empty(&self, flavor: MarkdownFlavor) -> String {
        self.generate_markdown(flavor).unwrap_or_default()
    }
}
