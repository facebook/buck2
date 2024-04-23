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
        num_args = 1,
        value_name = "PLATFORM"
    )]
    pub target_platforms: Option<String>,
}

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize, Default)]
pub struct TargetCfgUnusedOptions {
    /// This option is not used.
    #[clap(long = "target-platforms", num_args = 1, value_name = "PLATFORM")]
    pub target_platforms: Option<String>,
}

impl TargetCfgOptions {
    pub fn target_cfg(&self) -> TargetCfg {
        TargetCfg {
            target_platform: self.target_platforms.clone().unwrap_or_default(),
            cli_modifiers: self.cli_modifiers(),
        }
    }

    fn cli_modifiers(&self) -> Vec<String> {
        Vec::new()
    }
}

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize, Default)]
pub struct TargetCfgWithUniverseOptions {
    #[clap(flatten)]
    pub target_cfg: TargetCfgOptions,

    /// Comma separated list of targets to construct a configured target universe.
    /// When the option is specified, command targets are be resolved in this universe.
    /// Additionally, `--target-platforms=` and `--modifier=` flags
    /// are be used to configure the universe targets, not the command targets.
    #[clap(long, short = 'u', use_value_delimiter = true, verbatim_doc_comment)]
    pub target_universe: Vec<String>,
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use clap::CommandFactory;
    use clap::Parser;

    use super::*;

    fn parse(args: &[&str]) -> anyhow::Result<TargetCfgOptions> {
        Ok(TargetCfgOptions::try_parse_from(
            std::iter::once("program").chain(args.iter().copied()),
        )?)
    }

    #[ignore]
    #[test]
    fn short_opt_multiple() -> anyhow::Result<()> {
        let opts = parse(&["-m", "value1", "-m", "value2"])?;

        assert_eq!(opts.cli_modifiers(), vec!["value1", "value2"]);

        Ok(())
    }

    #[ignore]
    #[test]
    fn short_opt_comma_separated() -> anyhow::Result<()> {
        let opts = parse(&["-m", "value1,value2"])?;

        assert_eq!(opts.cli_modifiers(), vec!["value1", "value2"]);

        Ok(())
    }

    #[ignore]
    #[test]
    fn long_opt_multiple() -> anyhow::Result<()> {
        let opts = parse(&["--modifier", "value1", "--modifier", "value2"])?;

        assert_eq!(opts.cli_modifiers(), vec!["value1", "value2"]);

        Ok(())
    }

    #[ignore]
    #[test]
    fn long_opt_comma_separated() -> anyhow::Result<()> {
        let opts = parse(&["--modifier", "value1,value2"])?;

        assert_eq!(opts.cli_modifiers(), vec!["value1", "value2"]);

        Ok(())
    }

    #[ignore]
    #[test]
    fn comma_separated_and_multiple() -> anyhow::Result<()> {
        let opts = parse(&["--modifier", "value1,value2", "--modifier", "value3"])?;

        assert_eq!(opts.cli_modifiers(), vec!["value1", "value2", "value3"]);

        Ok(())
    }

    #[ignore]
    #[test]
    fn space_separated_fails() -> anyhow::Result<()> {
        assert_matches!(parse(&["-m", "value1", "value2"]), Err(..));

        Ok(())
    }

    #[ignore]
    #[test]
    fn test_target_cfg_unused() {
        #[derive(Debug, Eq, PartialEq)]
        struct ReducedArg {
            name: String,
            long: Option<String>,
            value_delimiter: Option<char>,
            number_of_values: Option<clap::builder::ValueRange>,
        }

        fn args<C: CommandFactory>() -> Vec<ReducedArg> {
            C::command()
                .get_arguments()
                .map(|a| ReducedArg {
                    name: a.get_id().as_str().to_owned(),
                    long: a.get_long().map(|s| s.to_owned()),
                    value_delimiter: a.get_value_delimiter(),
                    number_of_values: a.get_num_args(),
                })
                .collect()
        }

        let a = args::<TargetCfgOptions>();
        let b = args::<TargetCfgUnusedOptions>();

        assert_eq!(a, b);
        assert!(!a.is_empty());
    }
}
