/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Small module that re-exports a single entry from sidebars.js. This is done
// because docusaurus really does not like having anything else exported from the
// sidebars module, and we need to make some functionality available (itemFilter)
// in docusaurus.config.js

const manualSidebar = require('./sidebars.js').manualSidebar;

module.exports = {
  manualSidebar: manualSidebar,
}
