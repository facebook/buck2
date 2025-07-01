/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

import {ConfiguredTargetLabel} from './fbs/explain'

// Unique identifier for a configured target.
// Javascript copy of `impl Display for ConfiguredTargetLabel` in rust code
export function formatTargetLabel(label: ConfiguredTargetLabel): string {
  const unconfigured = label.targetLabel() ?? ''
  const cfg = label.cfg() ?? ''
  const execCfg = label.execCfg() ?? ''

  let res = `${unconfigured} (${cfg})`
  if (execCfg) {
    res = `${res} (${execCfg})`
  }
  return res
}
