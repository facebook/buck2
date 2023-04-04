/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// Replace `%s` with remaining command line arguments which contain query literals.
pub const QUERY_PERCENT_S_PLACEHOLDER: &str = "%s";
/// Replace `%Ss` with query literals read from files from remaining command line arguments.
pub const QUERY_PERCENT_SS_PLACEHOLDER: &str = "%Ss";
