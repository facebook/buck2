/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use clap::builder::styling;
use clap::builder::Styles;

pub(crate) fn get_styles() -> Styles {
    let heading = styling::AnsiColor::Yellow.on_default().bold();
    Styles::styled()
        .header(heading)
        .usage(heading)
        .literal(styling::AnsiColor::Green.on_default())
        .placeholder(styling::AnsiColor::Cyan.on_default())
}
