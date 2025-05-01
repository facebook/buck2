/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::sync::Arc;

use buck2_core::cells::name::CellName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use gazebo::prelude::SliceExt as _;
use gazebo::prelude::VecExt as _;

use crate::legacy_configs::dice::HasLegacyConfigs;
use crate::legacy_configs::key::BuckconfigKeyRef;
use crate::legacy_configs::view::LegacyBuckConfigView;

const DEFAULT_BUILDFILES: &[&str] = &["BUCK.v2", "BUCK"];

/// Deal with the `buildfile.name` key (and `name_v2`)
pub fn parse_buildfile_name(
    mut config: impl LegacyBuckConfigView,
) -> buck2_error::Result<Vec<FileNameBuf>> {
    // For buck2, we support a slightly different mechanism for setting the buildfile to
    // assist with easier migration from v1 to v2.
    // First, we check the key `buildfile.name_v2`, if this is provided, we use it.
    // Second, if that wasn't provided, we will use `buildfile.name` like buck1 does,
    // but for every entry `FOO` we will insert a preceding `FOO.v2`.
    // If neither of those is provided, we will use the default of `["BUCK.v2", "BUCK"]`.
    // This scheme provides a natural progression to buckv2, with the ability to use separate
    // buildfiles for the two where necessary.
    let mut base = if let Some(buildfiles_value) =
        config.parse_list::<String>(BuckconfigKeyRef {
            section: "buildfile",
            property: "name_v2",
        })? {
        buildfiles_value.into_try_map(FileNameBuf::try_from)?
    } else if let Some(buildfiles_value) = config.parse_list::<String>(BuckconfigKeyRef {
        section: "buildfile",
        property: "name",
    })? {
        let mut buildfiles = Vec::new();
        for buildfile in buildfiles_value {
            buildfiles.push(FileNameBuf::try_from(format!("{}.v2", buildfile))?);
            buildfiles.push(FileNameBuf::try_from(buildfile)?);
        }
        buildfiles
    } else {
        DEFAULT_BUILDFILES.map(|&n| FileNameBuf::try_from(n.to_owned()).unwrap())
    };

    if let Some(buildfile) = config.parse::<String>(BuckconfigKeyRef {
        section: "buildfile",
        property: "extra_for_test",
    })? {
        base.push(FileNameBuf::try_from(buildfile)?);
    }

    Ok(base)
}

pub trait HasBuildfiles {
    fn get_buildfiles(
        &mut self,
        cell: CellName,
    ) -> impl Future<Output = buck2_error::Result<Arc<[FileNameBuf]>>>;
}

#[derive(
    Clone,
    derive_more::Display,
    Debug,
    Hash,
    Eq,
    PartialEq,
    allocative::Allocative
)]
#[display("BuildfilesKey({})", self.0)]
struct BuildfilesKey(CellName);

#[async_trait::async_trait]
impl Key for BuildfilesKey {
    type Value = buck2_error::Result<Arc<[FileNameBuf]>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let config = ctx.get_legacy_config_on_dice(self.0).await?;
        Ok(parse_buildfile_name(config.view(ctx))?.into())
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

impl HasBuildfiles for DiceComputations<'_> {
    async fn get_buildfiles(&mut self, cell: CellName) -> buck2_error::Result<Arc<[FileNameBuf]>> {
        self.compute(&BuildfilesKey(cell)).await?
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::cells::name::CellName;
    use gazebo::prelude::SliceExt;
    use indoc::indoc;

    use crate::buildfiles::parse_buildfile_name;
    use crate::legacy_configs::cells::BuckConfigBasedCells;
    use crate::legacy_configs::configs::testing::TestConfigParserFileOps;

    #[tokio::test]
    async fn test_buildfiles() -> buck2_error::Result<()> {
        let mut file_ops = TestConfigParserFileOps::new(&[
            (
                ".buckconfig",
                indoc!(
                    r#"
                            [cells]
                                root = .
                                other = other/
                                third_party = third_party/
                        "#
                ),
            ),
            (
                "other/.buckconfig",
                indoc!(
                    r#"
                            [cells]
                                other = .
                            [buildfile]
                                name = TARGETS
                                extra_for_test = TARGETS.test
                        "#
                ),
            ),
            (
                "third_party/.buckconfig",
                indoc!(
                    r#"
                            [cells]
                                third_party = .
                            [buildfile]
                                name_v2 = OKAY
                                name = OKAY_v1
                        "#
                ),
            ),
        ])?;

        let cells = BuckConfigBasedCells::testing_parse_with_file_ops(&mut file_ops, &[]).await?;

        let config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("root"), &mut file_ops)
            .await?;
        assert_eq!(
            vec!["BUCK.v2", "BUCK"],
            parse_buildfile_name(&config)?.map(|f| f.as_str()),
        );

        let config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("other"), &mut file_ops)
            .await?;
        assert_eq!(
            vec!["TARGETS.v2", "TARGETS", "TARGETS.test"],
            parse_buildfile_name(&config)?.map(|f| f.as_str()),
        );

        let config = cells
            .parse_single_cell_with_file_ops(CellName::testing_new("third_party"), &mut file_ops)
            .await?;
        assert_eq!(
            vec!["OKAY"],
            parse_buildfile_name(&config)?.map(|f| f.as_str()),
        );

        Ok(())
    }
}
