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
