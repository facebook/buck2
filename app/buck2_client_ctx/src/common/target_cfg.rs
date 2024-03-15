/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::TargetCfg;

/// Defines options related to commands that involves a streaming daemon command.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize, Default)]
pub struct TargetCfgOptions {
    #[clap(
        long = "target-platforms",
        help = "Configuration target (one) to use to configure targets",
        number_of_values = 1,
        value_name = "PLATFORM"
    )]
    pub target_platforms: Option<String>,

    #[clap(
        value_name = "VALUE",
        long = "modifier",
        use_value_delimiter = true,
        value_delimiter=',',
        short = 'm',
        help = "A configuration modifier to configure all targets on the command line. This may be a constraint value target.",
        // Needs to be explicitly set, otherwise will treat `-c a b c` -> [a, b, c]
        // rather than [a] and other positional arguments `b c`.
        number_of_values = 1
    )]
    pub cli_modifiers: Vec<String>,
}

impl TargetCfgOptions {
    pub fn target_cfg(&self) -> TargetCfg {
        TargetCfg {
            target_platform: self.target_platforms.clone().unwrap_or_default(),
            cli_modifiers: self.cli_modifiers.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use clap::Parser;

    use super::*;

    fn parse(args: &[&str]) -> anyhow::Result<TargetCfgOptions> {
        Ok(TargetCfgOptions::from_iter_safe(
            std::iter::once("program").chain(args.iter().copied()),
        )?)
    }

    #[test]
    fn short_opt_multiple() -> anyhow::Result<()> {
        let opts = parse(&["-m", "value1", "-m", "value2"])?;

        assert_eq!(opts.cli_modifiers, vec!["value1", "value2"]);

        Ok(())
    }

    #[test]
    fn short_opt_comma_separated() -> anyhow::Result<()> {
        let opts = parse(&["-m", "value1,value2"])?;

        assert_eq!(opts.cli_modifiers, vec!["value1", "value2"]);

        Ok(())
    }

    #[test]
    fn long_opt_multiple() -> anyhow::Result<()> {
        let opts = parse(&["--modifier", "value1", "--modifier", "value2"])?;

        assert_eq!(opts.cli_modifiers, vec!["value1", "value2"]);

        Ok(())
    }

    #[test]
    fn long_opt_comma_separated() -> anyhow::Result<()> {
        let opts = parse(&["--modifier", "value1,value2"])?;

        assert_eq!(opts.cli_modifiers, vec!["value1", "value2"]);

        Ok(())
    }

    #[test]
    fn comma_separated_and_multiple() -> anyhow::Result<()> {
        let opts = parse(&["--modifier", "value1,value2", "--modifier", "value3"])?;

        assert_eq!(opts.cli_modifiers, vec!["value1", "value2", "value3"]);

        Ok(())
    }

    #[test]
    fn space_separated_fails() -> anyhow::Result<()> {
        assert_matches!(parse(&["-m", "value1", "value2"]), Err(..));

        Ok(())
    }
}
