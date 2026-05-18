/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Home of buck2's `RelativePath` types.
//!
//! Currently re-exports the types from the `relative_path` crate. The intent is
//! to replace these with buck2-owned implementations; importing through this
//! module keeps the eventual swap from churning callers.

pub use relative_path::Component;
pub use relative_path::FromPathError;
pub use relative_path::FromPathErrorKind;
pub use relative_path::RelativePath;
pub use relative_path::RelativePathBuf;
