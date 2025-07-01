/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Small module that re-exports a single entry from sidebars.js. This is done
// because docusaurus really does not like having anything else exported from the
// sidebars module, and we need to make some functionality available (itemFilter)
// in docusaurus.config.js

import { sidebars } from "./sidebars";

module.exports = sidebars
