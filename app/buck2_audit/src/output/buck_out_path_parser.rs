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
use buck2_interpreter::path::BxlFilePath;
use dupe::Dupe;
use itertools::Itertools;

#[derive(Debug, thiserror::Error)]
enum BuckOutPathParserError {
    #[error(
        "Malformed buck-out path. Expected format: `buck-out/<isolation_prefix>/<gen|tmp|test|gen-anon|gen-bxl>/<cell_name>/<cfg_hash>/<target_path?>/__<target_name>__/<__action__id__?>/<outputs>`. Actual path was: `{0}`"
    )]
    MalformedOutputPath(String),
}

/// The types of the `buck-out` path. Each type contains the configuration hash.
pub(crate) enum BuckOutPathType {
    BxlOutput {
        // `BxlFunctionLabel` contains the `CellPath` to the bxl function.
        _bxl_function_label: BxlFunctionLabel,
        _config_hash: String,
    },
    AnonOutput {
        _path: CellPath,
        _target_label: TargetLabel,
        // Rule attr hash is part of anonymous target buck-outs.
        _attr_hash: String,
        _config_hash: String,
    },
    RuleOutput {
        _path: CellPath,
        target_label: TargetLabel,
        // This is the part of the buck-out after target name. For example, it would `artifact` in  `gen/path/to/__target_name__/artifact`
        path_after_target_name: ForwardRelativePathBuf,
        config_hash: String,
    },
    TestOutput {
        _path: CellPath,
        _config_hash: String,
    },
    TmpOutput {
        _path: CellPath,
        _target_label: TargetLabel,
        _config_hash: String,
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

fn get_cell_path<'v>(
    iter: &mut Peekable<impl Iterator<Item = &'v FileName>>,
    cell_resolver: &'v CellResolver,
    generated_prefix: &'v str,
) -> anyhow::Result<(CellPath, String, Option<String>)> {
    let is_anon = generated_prefix == "gen-anon";
    let is_test = generated_prefix == "test";
    // Get cell name and validate it exists
    match iter.next() {
        Some(cell_name) => {
            let cell_name = CellName::unchecked_new(cell_name.as_str())?;

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

                            return Ok((cell_path, config_hash.to_string(), anon_hash));
                        }
                    }
                    None => (),
                }
            }

            if is_test {
                Ok((
                    CellPath::new(cell_name, cell_relative_path.to_buf()),
                    config_hash.to_string(),
                    None,
                ))
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
            Ok(target_name.to_owned())
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
                        let (path, config_hash, _) =
                            get_cell_path(&mut iter, self.cell_resolver, "tmp")?;
                        let target_label = get_target_label(&mut iter, path.clone())?;

                        Ok(BuckOutPathType::TmpOutput {
                            _path: path,
                            _target_label: target_label,
                            _config_hash: config_hash,
                        })
                    }
                    "test" => {
                        let (path, config_hash, _) =
                            get_cell_path(&mut iter, self.cell_resolver, "test")?;

                        Ok(BuckOutPathType::TestOutput {
                            _path: path,
                            _config_hash: config_hash,
                        })
                    }
                    "gen" => {
                        let (path, config_hash, _) =
                            get_cell_path(&mut iter, self.cell_resolver, "gen")?;
                        let target_label = get_target_label(&mut iter, path.clone())?;
                        let path_after_target_name =
                            ForwardRelativePathBuf::new(iter.clone().join("/"))?;

                        Ok(BuckOutPathType::RuleOutput {
                            _path: path,
                            target_label,
                            path_after_target_name,
                            config_hash,
                        })
                    }
                    "gen-anon" => {
                        let (path, config_hash, anon_hash) =
                            get_cell_path(&mut iter, self.cell_resolver, "gen-anon")?;
                        let target_label = get_target_label(&mut iter, path.clone())?;

                        Ok(BuckOutPathType::AnonOutput {
                            _path: path,
                            _target_label: target_label,
                            _attr_hash: anon_hash
                                .expect("No hash found in anonymous artifact buck-out"),
                            _config_hash: config_hash,
                        })
                    }
                    "gen-bxl" => {
                        let (path, config_hash, _) =
                            get_cell_path(&mut iter, self.cell_resolver, "gen-bxl")?;
                        let bxl_function_label = get_bxl_function_label(&mut iter, path)?;

                        Ok(BuckOutPathType::BxlOutput {
                            _bxl_function_label: bxl_function_label,
                            _config_hash: config_hash,
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
    use std::collections::HashMap;

    use buck2_build_api::bxl::types::BxlFunctionLabel;
    use buck2_core::cells::alias::NonEmptyCellAlias;
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::cells::cell_root_path::CellRootPath;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellResolver;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::configuration::data::ConfigurationDataData;
    use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_core::package::PackageLabel;
    use buck2_core::target::label::TargetLabel;
    use buck2_core::target::name::TargetNameRef;
    use buck2_interpreter::path::BxlFilePath;

    use crate::output::buck_out_path_parser::BuckOutPathParser;
    use crate::output::buck_out_path_parser::BuckOutPathType;

    fn get_parse_test_cell_resolver() -> anyhow::Result<CellResolver> {
        let cell_path = CellRootPath::new(ProjectRelativePath::new("foo/bar")?);

        let mut cell_alias_map = HashMap::new();
        cell_alias_map.insert(
            NonEmptyCellAlias::new("bar".to_owned()).unwrap(),
            CellName::testing_new("bar"),
        );

        let cell_resolver = CellResolver::with_names_and_paths_with_alias(&[(
            CellName::testing_new("bar"),
            cell_path.to_buf(),
            cell_alias_map,
        )]);

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
                _path: path,
                target_label,
                path_after_target_name,
                config_hash,
            } => {
                assert_eq!(
                    path_after_target_name,
                    ForwardRelativePathBuf::new("output".to_owned())?,
                );
                assert_eq!(target_label, expected_target_label,);
                assert_eq!(path, expected_cell_path,);
                assert_eq!(config_hash, expected_config_hash.as_str());
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        let rule_path_target_label_with_slashes = format!(
            "buck-out/v2/gen/bar/{}/path/to/target/__target_name_start/target_name_end__/output",
            expected_config_hash
        );

        let res = buck_out_parser.parse(&rule_path_target_label_with_slashes)?;

        let expected_target_label_with_slashes = TargetLabel::new(
            pkg,
            TargetNameRef::new("target_name_start/target_name_end")?,
        );

        match res {
            BuckOutPathType::RuleOutput {
                _path: path,
                target_label,
                path_after_target_name,
                config_hash,
            } => {
                assert_eq!(
                    path_after_target_name,
                    ForwardRelativePathBuf::new("output".to_owned())?,
                );
                assert_eq!(target_label, expected_target_label_with_slashes,);
                assert_eq!(path, expected_cell_path,);
                assert_eq!(config_hash, expected_config_hash.as_str());
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
                _path: path,
                _target_label: target_label,
                _config_hash: config_hash,
            } => {
                assert_eq!(path, expected_cell_path,);
                assert_eq!(config_hash, expected_config_hash.as_str());
                assert_eq!(target_label, expected_target_label,);
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
            BuckOutPathType::TestOutput {
                _path: path,
                _config_hash: config_hash,
            } => {
                assert_eq!(path, expected_test_cell_path,);
                assert_eq!(config_hash, expected_config_hash.as_str());
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
                _path: path,
                _target_label: target_label,
                _attr_hash: attr_hash,
                _config_hash: config_hash,
            } => {
                assert_eq!(target_label, expected_target_label,);
                assert_eq!(path, expected_cell_path,);
                assert_eq!(attr_hash, "anon_hash",);
                assert_eq!(config_hash, expected_config_hash.as_str());
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
                _bxl_function_label: bxl_function_label,
                _config_hash: config_hash,
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

                assert_eq!(bxl_function_label, expected_bxl_function_label,);
                assert_eq!(config_hash, expected_config_hash.as_str());
            }
            _ => panic!("Should have parsed buck-out path successfully"),
        }

        Ok(())
    }
}
