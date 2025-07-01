/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use clap::builder::Styles;
use clap::builder::styling;

pub(crate) fn get_styles() -> Styles {
    let heading = styling::AnsiColor::Yellow.on_default().bold();
    Styles::styled()
        .header(heading)
        .usage(heading)
        .literal(styling::AnsiColor::Green.on_default())
        .placeholder(styling::AnsiColor::Cyan.on_default())
}
