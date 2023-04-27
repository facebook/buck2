/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_core::cells::CellResolver;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::display_precise_pattern;
use buck2_core::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_core::target::name::TargetName;
use dupe::Dupe;
use gazebo::prelude::VecExt;
use indexmap::IndexMap;

use crate::file_ops::FileOps;
use crate::pattern::package_roots::find_package_roots;

#[derive(Debug, thiserror::Error)]
enum ResolvedPatternError {
    #[error("Expecting {0} pattern, got `{1}`")]
    InvalidPattern(&'static str, String),
}

/// Pattern where `foo/...` is expanded to matching packages.
/// Targets are not validated yet, and `:` is not yet expanded.
#[derive(Debug)]
pub struct ResolvedPattern<T: PatternType> {
    pub specs: IndexMap<PackageLabel, PackageSpec<T>>,
}

impl<T> ResolvedPattern<T>
where
    T: PatternType,
{
    pub fn new() -> Self {
        Self {
            specs: IndexMap::new(),
        }
    }

    pub fn add_package(&mut self, package: PackageLabel) {
        self.specs.insert(package, PackageSpec::All);
    }

    pub fn add_target(&mut self, package: PackageLabel, target_name: TargetName, extra: T) {
        if let Some(s) = self.specs.get_mut(&package) {
            match s {
                PackageSpec::Targets(ref mut t) => t.push((target_name, extra)),
                PackageSpec::All => {}
            }
        } else {
            self.specs
                .insert(package, PackageSpec::Targets(vec![(target_name, extra)]));
        }
    }
}

impl ResolvedPattern<ConfiguredProvidersPatternExtra> {
    pub fn convert_pattern<U: PatternType>(self) -> anyhow::Result<ResolvedPattern<U>> {
        let mut specs = IndexMap::with_capacity(self.specs.len());
        for (package, spec) in self.specs {
            let spec = match spec {
                PackageSpec::Targets(targets) => {
                    PackageSpec::Targets(targets.into_try_map(|(target_name, extra)| {
                        let extra = U::from_configured_providers(extra.clone()).context(
                            ResolvedPatternError::InvalidPattern(
                                U::NAME,
                                display_precise_pattern(&package, target_name.as_ref(), &extra)
                                    .to_string(),
                            ),
                        )?;
                        anyhow::Ok((target_name, extra))
                    })?)
                }
                PackageSpec::All => PackageSpec::All,
            };
            specs.insert(package, spec);
        }
        Ok(ResolvedPattern { specs })
    }
}

/// Resolves a list of [ParsedPattern] to a [ResolvedPattern].
pub async fn resolve_target_patterns<P: PatternType>(
    cell_resolver: &CellResolver,
    patterns: &[ParsedPattern<P>],
    file_ops: &dyn FileOps,
) -> anyhow::Result<ResolvedPattern<P>> {
    let mut resolved = ResolvedPattern::new();
    for pattern in patterns {
        match pattern {
            ParsedPattern::Target(package, target_name, extra) => {
                resolved.add_target(package.dupe(), target_name.clone(), extra.clone());
            }
            ParsedPattern::Package(package) => {
                resolved.add_package(package.dupe());
            }
            ParsedPattern::Recursive(cell_path) => {
                let roots = find_package_roots(cell_path.clone(), file_ops, cell_resolver)
                    .await
                    .context("Error resolving recursive target pattern.")?;
                for package in roots {
                    resolved.add_package(package);
                }
            }
        }
    }
    Ok(resolved)
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::collections::BTreeSet;
    use std::collections::HashMap;
    use std::marker::PhantomData;
    use std::sync::Arc;

    use buck2_core::cells::alias::NonEmptyCellAlias;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::cells::CellsAggregator;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_core::package::PackageLabel;
    use buck2_core::pattern::pattern_type::PatternType;
    use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
    use buck2_core::pattern::pattern_type::TargetPatternExtra;
    use buck2_core::pattern::PackageSpec;
    use buck2_core::pattern::ParsedPattern;
    use buck2_core::provider::label::NonDefaultProvidersName;
    use buck2_core::provider::label::ProviderName;
    use buck2_core::provider::label::ProvidersName;
    use buck2_core::target::name::TargetName;
    use dupe::Dupe;
    use gazebo::prelude::*;
    use test_case::test_case;

    use crate::file_ops::testing::TestFileOps;
    use crate::file_ops::FileOps;
    use crate::pattern::resolve::resolve_target_patterns;
    use crate::pattern::resolve::ResolvedPattern;

    #[derive(Clone)]
    struct TestPatternResolver {
        resolver: CellResolver,
        file_ops: Arc<dyn FileOps>,
    }

    impl TestPatternResolver {
        fn new(cells: &[(&str, &str)], files: &[&str]) -> anyhow::Result<Self> {
            let resolver = {
                let mut agg = CellsAggregator::new();
                let mut cell_paths = HashMap::new();
                for (name, path) in cells {
                    cell_paths.insert(*name, *path);
                }

                for (_, path) in cells {
                    for (alias, alias_path) in &cell_paths {
                        agg.add_cell_entry(
                            CellRootPathBuf::new(ProjectRelativePathBuf::try_from(
                                (*path).to_owned(),
                            )?),
                            NonEmptyCellAlias::new((*alias).to_owned())?,
                            CellRootPathBuf::new(ProjectRelativePathBuf::try_from(
                                (*alias_path).to_owned(),
                            )?),
                        )?;
                    }
                }
                agg.make_cell_resolver()?
            };

            let resolved_files = files
                .iter()
                .map(|p| {
                    (
                        resolver
                            .get_cell_path(&ProjectRelativePathBuf::unchecked_new((*p).to_owned()))
                            .unwrap(),
                        "".to_owned(),
                    )
                })
                .collect();

            let file_ops = Arc::new(TestFileOps::new_with_files(resolved_files));
            Ok(TestPatternResolver { resolver, file_ops })
        }

        async fn resolve<T>(&self, patterns: &[&str]) -> anyhow::Result<ResolvedPattern<T>>
        where
            T: PatternType,
        {
            let patterns: Vec<_> = patterns.map(|p| {
                ParsedPattern::<T>::parse_precise(p, CellName::testing_new("root"), &self.resolver)
                    .unwrap()
            });

            resolve_target_patterns(&self.resolver, &patterns, &*self.file_ops).await
        }
    }

    trait ResolvedTargetPatternTestExt<T: PatternType> {
        fn assert_eq(&self, expected: &[(PackageLabel, PackageSpec<T>)]);
    }

    impl<T> ResolvedTargetPatternTestExt<T> for ResolvedPattern<T>
    where
        T: PatternType,
    {
        fn assert_eq(&self, expected: &[(PackageLabel, PackageSpec<T>)]) {
            let expected: BTreeMap<_, _> = expected.iter().map(|(p, s)| (p.dupe(), s)).collect();

            let expected_keys: BTreeSet<_> = expected.keys().collect();
            let actual_keys: BTreeSet<_> = self.specs.keys().collect();

            let missing_keys: Vec<_> = expected_keys.difference(&actual_keys).collect();
            assert!(
                missing_keys.is_empty(),
                "Expected entries for keys {:?}. Had {:?}",
                missing_keys,
                self.specs
            );

            let extra_keys: Vec<_> = actual_keys.difference(&expected_keys).collect();
            assert!(
                extra_keys.is_empty(),
                "Got unexpected keys {:?}",
                extra_keys
            );

            for (k, v) in expected {
                assert_eq!(v, self.specs.get(&k).unwrap());
            }
        }
    }

    #[tokio::test]
    async fn test_simple_specs_targets() -> anyhow::Result<()> {
        let tester = TestPatternResolver::new(&[("root", ""), ("child", "child/cell")], &[])?;
        tester
            .resolve::<TargetPatternExtra>(&[])
            .await?
            .assert_eq(&[]);
        tester
            .resolve::<TargetPatternExtra>(&[
                "//some:target",
                "//some:other_target",
                "child//a/package:",
            ])
            .await?
            .assert_eq(&[
                (
                    PackageLabel::testing_parse("root//some"),
                    PackageSpec::Targets(vec![
                        (TargetName::unchecked_new("target"), TargetPatternExtra),
                        (
                            TargetName::unchecked_new("other_target"),
                            TargetPatternExtra,
                        ),
                    ]),
                ),
                (
                    PackageLabel::testing_parse("child//a/package"),
                    PackageSpec::All,
                ),
            ]);
        Ok(())
    }

    #[tokio::test]
    async fn test_simple_specs_providers() -> anyhow::Result<()> {
        let tester = TestPatternResolver::new(&[("root", ""), ("child", "child/cell")], &[])?;
        tester
            .resolve::<ProvidersPatternExtra>(&[])
            .await?
            .assert_eq(&[]);
        tester
            .resolve::<ProvidersPatternExtra>(&[
                "//some:target",
                "//some:other_target[my-label]",
                "child//a/package:",
            ])
            .await?
            .assert_eq(&[
                (
                    PackageLabel::testing_parse("root//some"),
                    PackageSpec::Targets(vec![
                        (
                            TargetName::unchecked_new("target"),
                            ProvidersPatternExtra {
                                providers: ProvidersName::Default,
                            },
                        ),
                        (
                            TargetName::unchecked_new("other_target"),
                            ProvidersPatternExtra {
                                providers: ProvidersName::NonDefault(Box::new(
                                    NonDefaultProvidersName::Named(Box::new([ProviderName::new(
                                        "my-label".to_owned(),
                                    )
                                    .unwrap()])),
                                )),
                            },
                        ),
                    ]),
                ),
                (
                    PackageLabel::testing_parse("child//a/package"),
                    PackageSpec::All,
                ),
            ]);
        Ok(())
    }

    #[test_case(PhantomData::< TargetPatternExtra >; "parsing TargetPattern")]
    #[test_case(PhantomData::< ProvidersPatternExtra >; "parsing ProvidersPattern")]
    fn test_recursive_specs<T: PatternType>(_: PhantomData<T>) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let tester = TestPatternResolver::new(
                &[("root", ""), ("child", "child/cell")],
                &[
                    ("BUCK"),
                    ("other/BUCK"),
                    ("other/a/bit/deeper/BUCK"),
                    ("other/a/bit/deeper/and/deeper/BUCK"),
                    ("some/thing/dir/a/BUCK"),
                    ("some/thing/dir/a/b/BUCK"),
                    ("some/thing/extra/BUCK"),
                    ("child/cell/BUCK"),
                    ("child/cell/foo/BUCK"),
                ],
            )
            .unwrap();
            tester.resolve::<T>(&[]).await.unwrap().assert_eq(&[]);
            tester
                .resolve::<T>(&[
                    "//other/...",
                    "//other:target_that_doesnt_matter",
                    "//some/...",
                    "//some/thing/extra/...",
                    // root of cell.
                    "child//...",
                ])
                .await
                .unwrap()
                .assert_eq(&[
                    (PackageLabel::testing_parse("root//other"), PackageSpec::All),
                    (
                        PackageLabel::testing_parse("root//other/a/bit/deeper"),
                        PackageSpec::All,
                    ),
                    (
                        PackageLabel::testing_parse("root//other/a/bit/deeper/and/deeper"),
                        PackageSpec::All,
                    ),
                    (
                        PackageLabel::testing_parse("root//some/thing/dir/a"),
                        PackageSpec::All,
                    ),
                    (
                        PackageLabel::testing_parse("root//some/thing/dir/a/b"),
                        PackageSpec::All,
                    ),
                    (
                        PackageLabel::testing_parse("root//some/thing/extra"),
                        PackageSpec::All,
                    ),
                    (PackageLabel::testing_parse("child//"), PackageSpec::All),
                    (PackageLabel::testing_parse("child//foo"), PackageSpec::All),
                ]);
        })
    }
}
