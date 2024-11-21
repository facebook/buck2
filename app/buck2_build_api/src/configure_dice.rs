/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::dice::cells::SetCellResolver;
use buck2_common::dice::data::SetIoProvider;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::dice::SetLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::rollout_percentage::RolloutPercentage;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::SetDigestConfig;
use dice::DetectCycles;
use dice::Dice;
use dice::WhichDice;

use crate::actions::execute::dice_data::SetInvalidationTrackingConfig;

/// This is just a simple version number to allow us to more easily rollout modern dice.
const CURRENT_MODERN_DICE_VERSION: u32 = 3;

/// Utility to configure the dice globals.
/// One place to not forget to initialize something in all places.
pub async fn configure_dice_for_buck(
    io: Arc<dyn IoProvider>,
    digest_config: DigestConfig,
    root_config: Option<&LegacyBuckConfig>,
    detect_cycles: Option<DetectCycles>,
    which_dice: Option<WhichDice>,
) -> buck2_error::Result<Arc<Dice>> {
    let detect_cycles = detect_cycles.map_or_else(
        || {
            root_config
                .and_then(|c| {
                    c.parse::<DetectCycles>(BuckconfigKeyRef {
                        section: "buck2",
                        property: "detect_cycles",
                    })
                    .transpose()
                })
                .unwrap_or(Ok(DetectCycles::Enabled))
        },
        Ok,
    )?;

    let mut dice = match determine_which_dice(root_config, which_dice)? {
        WhichDice::Legacy => Dice::builder(),
        WhichDice::Modern => Dice::modern(),
    };
    dice.set_io_provider(io);
    dice.set_digest_config(digest_config);
    let invalidation_tracking_enabled = match root_config {
        Some(c) => c
            .parse::<RolloutPercentage>(BuckconfigKeyRef {
                section: "buck2",
                property: "invalidation_tracking_enabled",
            })?
            .map_or(false, |v| v.roll()),
        None => false,
    };
    dice.set_invalidation_tracking_config(invalidation_tracking_enabled);

    let dice = dice.build(detect_cycles);
    let mut dice_ctx = dice.updater();
    dice_ctx.set_none_cell_resolver()?;
    dice_ctx.set_none_legacy_config_external_data()?;
    dice_ctx.commit().await;

    Ok(dice)
}

fn determine_which_dice(
    root_config: Option<&LegacyBuckConfig>,
    which_dice: Option<WhichDice>,
) -> buck2_error::Result<WhichDice> {
    if let Some(v) = which_dice {
        return Ok(v);
    }

    if let Some(cfg) = root_config {
        if let Some(v) = cfg.parse::<WhichDice>(BuckconfigKeyRef {
            section: "buck2",
            property: "dice",
        })? {
            return Ok(v);
        }

        if let Some(minimum_dice_version) = cfg.parse::<u32>(BuckconfigKeyRef {
            section: "buck2",
            property: "modern_dice_min_version",
        })? {
            if CURRENT_MODERN_DICE_VERSION >= minimum_dice_version {
                return Ok(WhichDice::Modern);
            }
        }

        if let Some(rollout) = cfg.parse::<RolloutPercentage>(BuckconfigKeyRef {
            section: "buck2",
            property: "modern_dice_rollout",
        })? {
            if rollout.roll() {
                return Ok(WhichDice::Modern);
            }
        }
    }

    Ok(WhichDice::Modern)
}

#[cfg(test)]
mod tests {
    use buck2_common::legacy_configs::configs::testing::parse;

    use super::*;

    struct Cfg {
        dice: &'static str,
        modern_dice_min_version: &'static str,
        modern_dice_rollout: &'static str,
    }

    impl Cfg {
        fn to_cfg(self) -> LegacyBuckConfig {
            let mut section = String::new();
            if self.dice != "" {
                section += &format!("dice = {}\n", self.dice);
            }
            if self.modern_dice_min_version != "" {
                section += &format!(
                    "modern_dice_min_version = {}\n",
                    self.modern_dice_min_version
                );
            }
            if self.modern_dice_rollout != "" {
                section += &format!("modern_dice_rollout = {}\n", self.modern_dice_rollout);
            }

            parse(&[("config", &format!("[buck2]\n{}", section))], "config").unwrap()
        }
    }

    #[test]
    fn test_determine_which_dice() -> buck2_error::Result<()> {
        assert_eq!(
            WhichDice::Modern,
            determine_which_dice(
                Some(
                    &Cfg {
                        dice: "",
                        modern_dice_min_version: "",
                        modern_dice_rollout: "",
                    }
                    .to_cfg()
                ),
                None
            )?
        );

        assert_eq!(
            WhichDice::Legacy,
            determine_which_dice(
                Some(
                    &Cfg {
                        dice: "modern",
                        modern_dice_min_version: "",
                        modern_dice_rollout: "",
                    }
                    .to_cfg()
                ),
                Some(WhichDice::Legacy)
            )?
        );

        assert_eq!(
            WhichDice::Modern,
            determine_which_dice(
                Some(
                    &Cfg {
                        dice: "legacy",
                        modern_dice_min_version: "",
                        modern_dice_rollout: "",
                    }
                    .to_cfg()
                ),
                Some(WhichDice::Modern)
            )?
        );

        assert_eq!(
            WhichDice::Legacy,
            determine_which_dice(
                Some(
                    &Cfg {
                        dice: "legacy",
                        modern_dice_min_version: "",
                        modern_dice_rollout: "",
                    }
                    .to_cfg()
                ),
                None
            )?
        );

        assert_eq!(
            WhichDice::Modern,
            determine_which_dice(
                Some(
                    &Cfg {
                        dice: "modern",
                        modern_dice_min_version: "",
                        modern_dice_rollout: "",
                    }
                    .to_cfg()
                ),
                None
            )?
        );

        assert_eq!(
            WhichDice::Modern,
            determine_which_dice(
                Some(
                    &Cfg {
                        dice: "",
                        modern_dice_min_version: "100000",
                        modern_dice_rollout: "",
                    }
                    .to_cfg()
                ),
                None
            )?
        );

        assert_eq!(
            WhichDice::Modern,
            determine_which_dice(
                Some(
                    &Cfg {
                        dice: "",
                        modern_dice_min_version: "1",
                        modern_dice_rollout: "",
                    }
                    .to_cfg()
                ),
                None
            )?
        );

        assert_eq!(
            WhichDice::Modern,
            determine_which_dice(
                Some(
                    &Cfg {
                        dice: "",
                        modern_dice_min_version: "",
                        modern_dice_rollout: "hostname:1",
                    }
                    .to_cfg()
                ),
                None
            )?
        );

        Ok(())
    }
}
