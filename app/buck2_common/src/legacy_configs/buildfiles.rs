/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::default_buildfiles;
use buck2_core::fs::paths::file_name::FileNameBuf;
use gazebo::prelude::VecExt as _;

use crate::legacy_configs::LegacyBuckConfig;

/// Deal with the `buildfile.name` key (and `name_v2`)
pub fn parse_buildfile_name(config: &LegacyBuckConfig) -> anyhow::Result<Vec<FileNameBuf>> {
    // For buck2, we support a slightly different mechanism for setting the buildfile to
    // assist with easier migration from v1 to v2.
    // First, we check the key `buildfile.name_v2`, if this is provided, we use it.
    // Second, if that wasn't provided, we will use `buildfile.name` like buck1 does,
    // but for every entry `FOO` we will insert a preceding `FOO.v2`.
    // If neither of those is provided, we will use the default of `["BUCK.v2", "BUCK"]`.
    // This scheme provides a natural progression to buckv2, with the ability to use separate
    // buildfiles for the two where necessary.
    let mut base =
        if let Some(buildfiles_value) = config.parse_list::<String>("buildfile", "name_v2")? {
            buildfiles_value.into_try_map(FileNameBuf::try_from)?
        } else if let Some(buildfiles_value) = config.parse_list::<String>("buildfile", "name")? {
            let mut buildfiles = Vec::new();
            for buildfile in buildfiles_value {
                buildfiles.push(FileNameBuf::try_from(format!("{}.v2", buildfile))?);
                buildfiles.push(FileNameBuf::try_from(buildfile)?);
            }
            buildfiles
        } else {
            default_buildfiles()
        };

    if let Some(buildfile) = config.parse::<String>("buildfile", "extra_for_test")? {
        base.push(FileNameBuf::try_from(buildfile)?);
    }

    Ok(base)
}

#[cfg(test)]
mod tests {
    use buck2_core::cells::name::CellName;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use gazebo::prelude::SliceExt;
    use indoc::indoc;

    use crate::legacy_configs::buildfiles::parse_buildfile_name;
    use crate::legacy_configs::cells::create_project_filesystem;
    use crate::legacy_configs::cells::BuckConfigBasedCells;
    use crate::legacy_configs::testing::TestConfigParserFileOps;

    #[test]
    fn test_buildfiles() -> anyhow::Result<()> {
        let mut file_ops = TestConfigParserFileOps::new(&[
            (
                "/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                root = .
                                other = other/
                                third_party = third_party/
                        "#
                ),
            ),
            (
                "/other/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                other = .
                            [buildfile]
                                name = TARGETS
                                extra_for_test = TARGETS.test
                        "#
                ),
            ),
            (
                "/third_party/.buckconfig",
                indoc!(
                    r#"
                            [repositories]
                                third_party = .
                            [buildfile]
                                name_v2 = OKAY
                                name = OKAY_v1
                        "#
                ),
            ),
        ])?;

        let project_fs = create_project_filesystem();
        let configs = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &mut file_ops,
            &[],
            ProjectRelativePath::empty(),
        )?
        .configs_by_name;

        assert_eq!(
            vec!["BUCK.v2", "BUCK"],
            parse_buildfile_name(configs.get(CellName::testing_new("root"))?)?.map(|f| f.as_str()),
        );
        assert_eq!(
            vec!["TARGETS.v2", "TARGETS", "TARGETS.test"],
            parse_buildfile_name(configs.get(CellName::testing_new("other"))?)?.map(|f| f.as_str()),
        );
        assert_eq!(
            vec!["OKAY"],
            parse_buildfile_name(configs.get(CellName::testing_new("third_party"))?)?
                .map(|f| f.as_str()),
        );

        Ok(())
    }
}
