/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter::Peekable;

use anyhow::Context;
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_core::target::label::TargetLabel;
use buck2_core::target::name::TargetNameRef;
use buck2_core::target::name::EQ_SIGN_SUBST;
use buck2_interpreter::paths::bxl::BxlFilePath;
use dupe::Dupe;
use itertools::Itertools;

#[derive(Debug, thiserror::Error)]
enum BuckOutPathParserError {
    #[error(
        "Malformed buck-out path. Expected format: `buck-out/<isolation_prefix>/<gen|tmp|test|gen-anon|gen-bxl>/<cell_name>/<cfg_hash>/<target_path?>/__<target_name>__/<__action__id__?>/<outputs>`. Actual path was: `{0}`"
    )]
    MalformedOutputPath(String),
}

/// The common attributes of each `buck-out` path type,
pub(crate) struct BuckOutPathTypeCommon {
    /// Configuration hash within the `buck-out` path.
    pub(crate) config_hash: String,
    /// The path starting from cell to the artifact, without the configuration hash. For example, in
    /// `buck-out/v2/gen/cell/<CONFIG_HASH>/path/to/__target_name__/target`, it would be `cell/path/to/__target_name__/target`.
    pub(crate) raw_path_to_output: ForwardRelativePathBuf,
}

/// The types of the `buck-out` path.
pub(crate) enum BuckOutPathType {
    BxlOutput {
        // `BxlFunctionLabel` contains the `CellPath` to the bxl function.
        bxl_function_label: BxlFunctionLabel,
        common_attrs: BuckOutPathTypeCommon,
    },
    AnonOutput {
        path: CellPath,
        target_label: TargetLabel,
        // Rule attr hash is part of anonymous target buck-outs.
        attr_hash: String,
        common_attrs: BuckOutPathTypeCommon,
    },
    RuleOutput {
        path: CellPath,
        target_label: TargetLabel,
        // This is the part of the buck-out after target name. For example, it would `artifact` in  `gen/path/to/__target_name__/artifact`
        path_after_target_name: ForwardRelativePathBuf,
        common_attrs: BuckOutPathTypeCommon,
    },
    TestOutput {
        path: CellPath,
        common_attrs: BuckOutPathTypeCommon,
    },
    TmpOutput {
        path: CellPath,
        target_label: TargetLabel,
        common_attrs: BuckOutPathTypeCommon,
    },
}

pub(crate) struct BuckOutPathParser<'v> {
    cell_resolver: &'v CellResolver,
}

fn validate_buck_out_and_isolation_prefix<'v>(
    iter: &mut Peekable<impl Iterator<Item = &'v FileName>>,
) -> anyhow::Result<()> {
    // Validate path starts with buck-out.
    match iter.next() {
        Some(buck_out) => {
            if buck_out != "buck-out" {
                return Err(anyhow::anyhow!("Path does not start with buck-out"));
            }
        }
        None => return Err(anyhow::anyhow!("Path does not start with buck-out")),
    }

    // Advance the iterator to isolation dir.
    match iter.next() {
        Some(_) => Ok(()),
        None => Err(anyhow::anyhow!("Path does not have an isolation dir")),
    }
}

struct BuckOutPathData {
    // Cell path of the target label that created the artifact.
    cell_path: CellPath,
    config_hash: String,
    anon_hash: Option<String>,
    /// The path starting from cell to the artifact, without the configuration hash. For example, in
    /// `buck-out/v2/gen/cell/<CONFIG_HASH>/path/to/__target_name__/target`, it would be `cell/path/to/__target_name__/target`.
    raw_path_to_output: ForwardRelativePathBuf,
}

fn get_cell_path<'v>(
    iter: &mut Peekable<impl Iterator<Item = &'v FileName> + Clone>,
    cell_resolver: &'v CellResolver,
    generated_prefix: &'v str,
) -> anyhow::Result<BuckOutPathData> {
    let is_anon = generated_prefix == "gen-anon";
    let is_test = generated_prefix == "test";
    // Get cell name and validate it exists
    match iter.next() {
        Some(cell_name) => {
            let cell_name = CellName::unchecked_new(cell_name.as_str())?;
            let mut raw_path_to_output = ForwardRelativePath::new(cell_name.as_str())?.to_buf();

            cell_resolver.get(cell_name)?;

            // Advance iterator to the config hash
            let config_hash = match iter.next() {
                Some(config_hash) => config_hash,
                None => {
                    return Err(anyhow::anyhow!(
                        "Path does not have a platform configuration"
                    ));
                }
            };

            iter.clone().for_each(|f| {
                raw_path_to_output.push(f);
            });

            // Get cell relative path and construct the cell path
            let mut cell_relative_path = CellRelativePath::unchecked_new("").to_owned();

            while let Some(part) = iter.next() {
                cell_relative_path = cell_relative_path.join(part).to_owned();

                // We make sure not to consume the target name part via the iterator.
                match iter.peek() {
                    Some(maybe_target_name) => {
                        let maybe_target_name = maybe_target_name.as_str();
                        // TODO(@wendyy) We assume that the first string that we find that starts with "__"
                        // is the target name. There is a small risk of naming collisions (ex: if there's a directory
                        // name that follows this convention that contains a build file), but I will fix this at a
                        // later date.
                        if (*maybe_target_name).starts_with("__") {
                            // If it's an anonymous target, then the last part before the target name is actually the
                            // hash, and not part of the cell relative path.
                            let cell_path = if is_anon {
                                CellPath::new(
                                    cell_name,
                                    cell_relative_path
                                        .parent()
                                        .with_context(|| "Invalid path for anonymous target")?
                                        .to_buf(),
                                )
                            } else {
                                CellPath::new(cell_name, cell_relative_path.to_buf())
                            };

                            let anon_hash = if is_anon {
                                // Iterator is pointing to the part right before the target name, aka the attr
                                // hash for the anonymous target.
                                Some(part.to_string())
                            } else {
                                None
                            };

                            let buck_out_path_data = BuckOutPathData {
                                cell_path,
                                config_hash: config_hash.to_string(),
                                anon_hash,
                                raw_path_to_output: raw_path_to_output.to_buf(),
                            };

                            return Ok(buck_out_path_data);
                        }
                    }
                    None => (),
                }
            }

            if is_test {
                let buck_out_path_data = BuckOutPathData {
                    cell_path: CellPath::new(cell_name, cell_relative_path.to_buf()),
                    config_hash: config_hash.to_string(),
                    anon_hash: None,
                    raw_path_to_output: raw_path_to_output.to_buf(),
                };
                Ok(buck_out_path_data)
            } else {
                Err(anyhow::anyhow!("Invalid target name"))
            }
        }
        None => Err(anyhow::anyhow!("Invalid cell name")),
    }
}

fn get_target_name<'v>(
    iter: &mut Peekable<impl Iterator<Item = &'v FileName>>,
) -> anyhow::Result<String> {
    // Get target name, which is prefixed and suffixed with "__"
    match iter.next() {
        Some(raw_target_name) => {
            let mut target_name_with_underscores =
                <&ForwardRelativePath>::from(raw_target_name).to_owned();

            while !target_name_with_underscores.as_str().ends_with("__") {
                match iter.next() {
                    Some(next) => {
                        target_name_with_underscores = target_name_with_underscores.join(next);
                    }
                    None => return Err(anyhow::anyhow!("Invalid target name")),
                }
            }

            let target_name_with_underscores = target_name_with_underscores.as_str();
            let target_name =
                &target_name_with_underscores[2..(target_name_with_underscores.len() - 2)];
            Ok(target_name.replace(EQ_SIGN_SUBST, "="))
        }
        None => Err(anyhow::anyhow!("Invalid target name")),
    }
}

fn get_target_label<'v>(
    iter: &mut Peekable<impl Iterator<Item = &'v FileName>>,
    path: CellPath,
) -> anyhow::Result<TargetLabel> {
    let target_name = get_target_name(iter)?;
    let package = PackageLabel::from_cell_path(path.as_ref());
    let target = TargetNameRef::new(target_name.as_str())?;
    let target_label = TargetLabel::new(package.dupe(), target);
    Ok(target_label)
}

fn get_bxl_function_label<'v>(
    iter: &mut Peekable<impl Iterator<Item = &'v FileName>>,
    path: CellPath,
) -> anyhow::Result<BxlFunctionLabel> {
    let target_name = get_target_name(iter)?;
    let bxl_path = BxlFilePath::new(path)?;
    let bxl_function_label = BxlFunctionLabel {
        bxl_path,
        name: target_name,
    };

    Ok(bxl_function_label)
}

impl<'v> BuckOutPathParser<'v> {
    pub(crate) fn new(cell_resolver: &'v CellResolver) -> BuckOutPathParser {
        BuckOutPathParser { cell_resolver }
    }

    // Validates and parses the buck-out path, returning the `BuckOutPathType`. Assumes
    // that the inputted path is not a symlink.
    pub(crate) fn parse(&self, output_path: &str) -> anyhow::Result<BuckOutPathType> {
        self.parse_inner(output_path)
            .with_context(|| BuckOutPathParserError::MalformedOutputPath(output_path.to_owned()))
    }

    fn parse_inner(&self, output_path: &str) -> anyhow::Result<BuckOutPathType> {
        let path_as_forward_rel_path = ForwardRelativePathBuf::new(output_path.to_owned())?;
        let mut iter = path_as_forward_rel_path.iter().peekable();

        validate_buck_out_and_isolation_prefix(&mut iter)?;

        // Advance the iterator to the prefix (tmp, test, gen, gen-anon, or gen-bxl)
        match iter.next() {
            Some(part) => {
                let result = match part.as_str() {
                    "tmp" => {
                        let buck_out_path_data =
                            get_cell_path(&mut iter, self.cell_resolver, "tmp")?;
                        let target_label =
                            get_target_label(&mut iter, buck_out_path_data.cell_path.clone())?;

                        let common_attrs = BuckOutPathTypeCommon {
                            config_hash: buck_out_path_data.config_hash,
                            raw_path_to_output: buck_out_path_data.raw_path_to_output,
                        };

                        Ok(BuckOutPathType::TmpOutput {
                            path: buck_out_path_data.cell_path,
                            target_label,
                            common_attrs,
                        })
                    }
                    "test" => {
                        let buck_out_path_data =
                            get_cell_path(&mut iter, self.cell_resolver, "test")?;

                        let common_attrs = BuckOutPathTypeCommon {
                            config_hash: buck_out_path_data.config_hash,
                            raw_path_to_output: buck_out_path_data.raw_path_to_output,
                        };

                        Ok(BuckOutPathType::TestOutput {
                            path: buck_out_path_data.cell_path,
                            common_attrs,
                        })
                    }
                    "gen" => {
                        let buck_out_path_data =
                            get_cell_path(&mut iter, self.cell_resolver, "gen")?;
                        let target_label =
                            get_target_label(&mut iter, buck_out_path_data.cell_path.clone())?;
                        let path_after_target_name =
                            ForwardRelativePathBuf::new(iter.clone().join("/"))?;
                        let common_attrs = BuckOutPathTypeCommon {
                            config_hash: buck_out_path_data.config_hash,
                            raw_path_to_output: buck_out_path_data.raw_path_to_output,
                        };

                        Ok(BuckOutPathType::RuleOutput {
                            path: buck_out_path_data.cell_path,
                            target_label,
                            path_after_target_name,
                            common_attrs,
                        })
                    }
                    "gen-anon" => {
                        let buck_out_path_data =
                            get_cell_path(&mut iter, self.cell_resolver, "gen-anon")?;
                        let target_label =
                            get_target_label(&mut iter, buck_out_path_data.cell_path.clone())?;
                        let common_attrs = BuckOutPathTypeCommon {
                            config_hash: buck_out_path_data.config_hash,
                            raw_path_to_output: buck_out_path_data.raw_path_to_output,
                        };

                        Ok(BuckOutPathType::AnonOutput {
                            path: buck_out_path_data.cell_path,
                            target_label,
                            attr_hash: buck_out_path_data
                                .anon_hash
                                .expect("No hash found in anonymous artifact buck-out"),
                            common_attrs,
                        })
                    }
                    "gen-bxl" => {
                        let buck_out_path_data =
                            get_cell_path(&mut iter, self.cell_resolver, "gen-bxl")?;
                        let bxl_function_label =
                            get_bxl_function_label(&mut iter, buck_out_path_data.cell_path)?;
                        let common_attrs = BuckOutPathTypeCommon {
                            config_hash: buck_out_path_data.config_hash,
                            raw_path_to_output: buck_out_path_data.raw_path_to_output,
                        };

                        Ok(BuckOutPathType::BxlOutput {
                            bxl_function_label,
                            common_attrs,
                        })
                    }
                    _ => Err(anyhow::anyhow!(
                        "Directory after isolation dir is invalid (should be gen, gen-bxl, gen-anon, tmp, or test)"
                    )),
                };

                // Validate for non-test outputs that the target name is not the last element in the path
                if part != "test" && iter.peek().is_none() {
                    Err(anyhow::anyhow!("No output artifacts found"))
                } else {
                    result
                }
            }
            None => Err(anyhow::anyhow!("Path is empty")),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use buck2_build_api::bxl::types::BxlFunctionLabel;
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::cells::cell_root_path::CellRootPath;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::cells::CellResolver;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::configuration::data::ConfigurationDataData;
    use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_core::package::PackageLabel;
    use buck2_core::target::label::TargetLabel;
    use buck2_core::target::name::TargetNameRef;
    use buck2_interpreter::paths::bxl::BxlFilePath;

    use crate::output::buck_out_path_parser::BuckOutPathParser;
    use crate::output::buck_out_path_parser::BuckOutPathType;

    fn get_parse_test_cell_resolver() -> anyhow::Result<CellResolver> {
        let cell_path = CellRootPath::new(ProjectRelativePath::new("foo/bar")?);

        let cell_resolver = CellResolver::testing_with_name_and_path(
            CellName::testing_new("bar"),
            cell_path.to_buf(),
        );

        Ok(cell_resolver)
    }

    #[test]
    fn test_buck_path_parser_validation() -> anyhow::Result<()> {
        let configuration = ConfigurationData::from_platform(
            "cfg_for//:testing_exec".to_owned(),
            ConfigurationDataData {
                constraints: BTreeMap::new(),
            },
        )
        .unwrap();
        let cell_resolver = get_parse_test_cell_resolver()?;
        let buck_out_parser = BuckOutPathParser::new(&cell_resolver);

        let malformed_path1 = "does/not/start/with/buck-out/blah/blah";
        let malformed_path2 = "buck-out/v2/invalid_buck_prefix/blah/blah/blah/blah";
        let malformed_path3 = "buck-out/v2/gen/bar/no/target/name/found";
        let malformed_path4 = "buck-out/v2/gen/bar/path/to/target/__but_no_artifacts__";

        let res = buck_out_parser.parse(malformed_path1);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        let res = buck_out_parser.parse(malformed_path2);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        let res = buck_out_parser.parse(malformed_path3);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        let res = buck_out_parser.parse(malformed_path4);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        let cell_does_not_exist =
            "buck-out/v2/gen/nonexistent_cell/cfg_hash/path/to/target/__target_name__/output";

        let res = buck_out_parser.parse(cell_does_not_exist);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        let config_hash = configuration.output_hash();
        let no_artifacts_after_target_name = &format!(
            "buck-out/v2/gen/bar/{}/path/to/target/__target_name__",
            config_hash
        );
        let res = buck_out_parser.parse(no_artifacts_after_target_name);
        assert!(res.err().unwrap().to_string().contains("Malformed"));

        Ok(())
    }

    #[test]
    fn test_buck_path_parser() -> anyhow::Result<()> {
        let configuration = ConfigurationData::from_platform(
            "cfg_for//:testing_exec".to_owned(),
            ConfigurationDataData {
                constraints: BTreeMap::new(),
            },
        )
        .unwrap();
        let cell_resolver = get_parse_test_cell_resolver()?;
        let buck_out_parser = BuckOutPathParser::new(&cell_resolver);

        let pkg = PackageLabel::new(
            CellName::testing_new("bar"),
            CellRelativePath::unchecked_new("path/to/target"),
        );

        let expected_target_label =
            TargetLabel::new(pkg.clone(), TargetNameRef::new("target_name")?);

        let expected_cell_path = CellPath::new(
            CellName::testing_new("bar"),
            CellRelativePath::unchecked_new("path/to/target").to_owned(),
        );

        let expected_config_hash = configuration.output_hash();

        let rule_path = format!(
            "buck-out/v2/gen/bar/{}/path/to/target/__target_name__/output",
            expected_config_hash
        );

        let res = buck_out_parser.parse(&rule_path)?;

        match res {
            BuckOutPathType::RuleOutput {
                path,
                target_label,
                path_after_target_name,
                common_attrs,
            } => {
                assert_eq!(
                    path_after_target_name,
                    ForwardRelativePathBuf::new("output".to_owned())?,
                );
                assert_eq!(target_label, expected_target_label);
                assert_eq!(path, expected_cell_path);
                assert_eq!(common_attrs.config_hash, expected_config_hash.as_str());
                assert_eq!(
                    common_attrs.raw_path_to_output.as_str(),
                    "bar/path/to/target/__target_name__/output"
                )
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        let rule_path_target_label_with_slashes = format!(
            "buck-out/v2/gen/bar/{}/path/to/target/__target_name_start/target_name_end__/output",
            expected_config_hash
        );

        let res = buck_out_parser.parse(&rule_path_target_label_with_slashes)?;

        let expected_target_label_with_slashes = TargetLabel::new(
            pkg.clone(),
            TargetNameRef::new("target_name_start/target_name_end")?,
        );

        match res {
            BuckOutPathType::RuleOutput {
                path,
                target_label,
                path_after_target_name,
                common_attrs,
            } => {
                assert_eq!(
                    path_after_target_name,
                    ForwardRelativePathBuf::new("output".to_owned())?,
                );
                assert_eq!(target_label, expected_target_label_with_slashes);
                assert_eq!(path, expected_cell_path);
                assert_eq!(common_attrs.config_hash, expected_config_hash.as_str());
                assert_eq!(
                    common_attrs.raw_path_to_output.as_str(),
                    "bar/path/to/target/__target_name_start/target_name_end__/output"
                )
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        let rule_path_with_equal_sign = format!(
            "buck-out/v2/gen/bar/{}/path/to/target/__target_name_eqsb_out__/output",
            expected_config_hash
        );

        let res = buck_out_parser.parse(&rule_path_with_equal_sign)?;

        let expected_target_label_with_equal_sign =
            TargetLabel::new(pkg, TargetNameRef::new("target_name=out")?);

        match res {
            BuckOutPathType::RuleOutput {
                path,
                target_label,
                path_after_target_name,
                common_attrs,
            } => {
                assert_eq!(
                    path_after_target_name,
                    ForwardRelativePathBuf::new("output".to_owned())?,
                );
                assert_eq!(target_label, expected_target_label_with_equal_sign);
                assert_eq!(path, expected_cell_path);
                assert_eq!(common_attrs.config_hash, expected_config_hash.as_str());
                assert_eq!(
                    common_attrs.raw_path_to_output.as_str(),
                    "bar/path/to/target/__target_name_eqsb_out__/output"
                )
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        let tmp_path = format!(
            "buck-out/v2/tmp/bar/{}/path/to/target/__target_name__/output",
            expected_config_hash
        );

        let res = buck_out_parser.parse(&tmp_path)?;

        match res {
            BuckOutPathType::TmpOutput {
                path,
                target_label,
                common_attrs,
            } => {
                assert_eq!(path, expected_cell_path);
                assert_eq!(common_attrs.config_hash, expected_config_hash.as_str());
                assert_eq!(target_label, expected_target_label);
                assert_eq!(
                    common_attrs.raw_path_to_output.as_str(),
                    "bar/path/to/target/__target_name__/output"
                )
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        let test_path = format!(
            "buck-out/v2/test/bar/{}/path/to/target/test/output",
            expected_config_hash
        );

        let expected_test_cell_path = CellPath::new(
            CellName::testing_new("bar"),
            CellRelativePath::unchecked_new("path/to/target/test/output").to_owned(),
        );

        let res = buck_out_parser.parse(&test_path)?;

        match res {
            BuckOutPathType::TestOutput { path, common_attrs } => {
                assert_eq!(path, expected_test_cell_path);
                assert_eq!(common_attrs.config_hash, expected_config_hash.as_str());
                assert_eq!(
                    common_attrs.raw_path_to_output.as_str(),
                    "bar/path/to/target/test/output"
                )
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        let anon_path = format!(
            "buck-out/v2/gen-anon/bar/{}/path/to/target/anon_hash/__target_name__/output",
            expected_config_hash
        );

        let res = buck_out_parser.parse(&anon_path)?;

        match res {
            BuckOutPathType::AnonOutput {
                path,
                target_label,
                attr_hash,
                common_attrs,
            } => {
                assert_eq!(target_label, expected_target_label);
                assert_eq!(path, expected_cell_path);
                assert_eq!(attr_hash, "anon_hash");
                assert_eq!(common_attrs.config_hash, expected_config_hash.as_str());
                assert_eq!(
                    common_attrs.raw_path_to_output.as_str(),
                    "bar/path/to/target/anon_hash/__target_name__/output"
                )
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        let path = format!(
            "buck-out/v2/gen-bxl/bar/{}/path/to/function.bxl/__function_name__/output",
            expected_config_hash
        );

        let res = buck_out_parser.parse(&path)?;

        match res {
            BuckOutPathType::BxlOutput {
                bxl_function_label,
                common_attrs,
            } => {
                let path = CellPath::new(
                    CellName::testing_new("bar"),
                    CellRelativePath::unchecked_new("path/to/function.bxl").to_owned(),
                );

                let bxl_path = BxlFilePath::new(path)?;
                let expected_bxl_function_label = BxlFunctionLabel {
                    bxl_path,
                    name: "function_name".to_owned(),
                };

                assert_eq!(bxl_function_label, expected_bxl_function_label);
                assert_eq!(common_attrs.config_hash, expected_config_hash.as_str());
                assert_eq!(
                    common_attrs.raw_path_to_output.as_str(),
                    "bar/path/to/function.bxl/__function_name__/output"
                )
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        Ok(())
    }
}
