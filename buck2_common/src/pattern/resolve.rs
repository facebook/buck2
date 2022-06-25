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
use buck2_core::package::Package;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::PatternType;
use gazebo::dupe::Dupe;
use indexmap::IndexMap;

use crate::file_ops::FileOps;
use crate::pattern::package_roots::find_package_roots;

#[derive(Debug)]
pub struct ResolvedPattern<T> {
    pub specs: IndexMap<Package, PackageSpec<T>>,
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

    pub fn add_package(&mut self, package: &Package) {
        self.specs.insert(package.dupe(), PackageSpec::All);
    }

    pub fn add_target(&mut self, package: &Package, target: &T) {
        if let Some(s) = self.specs.get_mut(package) {
            match s {
                PackageSpec::Targets(ref mut t) => t.push(target.clone()),
                PackageSpec::All => {}
            }
        } else {
            self.specs
                .insert(package.dupe(), PackageSpec::Targets(vec![target.clone()]));
        }
    }
}

/// Resolves a list of [ParsedPattern] to a [ResolvedPattern].
pub async fn resolve_target_patterns<
    'a,
    P: 'a + PatternType,
    T: Iterator<Item = &'a ParsedPattern<P>>,
>(
    cell_resolver: &CellResolver,
    patterns: T,
    file_ops: &dyn FileOps,
) -> anyhow::Result<ResolvedPattern<P>> {
    let mut resolved = ResolvedPattern::new();
    for pattern in patterns {
        match pattern {
            ParsedPattern::Target(package, target) => {
                resolved.add_target(package, target);
            }
            ParsedPattern::Package(package) => {
                resolved.add_package(package);
            }
            ParsedPattern::Recursive(cell_path) => {
                let roots = find_package_roots(cell_path.clone(), file_ops, cell_resolver)
                    .await
                    .context("When resolving recursive target pattern.")?;
                for package in roots {
                    resolved.add_package(&package);
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

    use buck2_core::cells::CellAlias;
    use buck2_core::cells::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::cells::CellsAggregator;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_core::package::testing::PackageExt;
    use buck2_core::package::Package;
    use buck2_core::pattern::PackageSpec;
    use buck2_core::pattern::ParsedPattern;
    use buck2_core::pattern::PatternType;
    use buck2_core::pattern::ProvidersPattern;
    use buck2_core::pattern::TargetPattern;
    use buck2_core::provider::label::ProviderName;
    use buck2_core::provider::label::ProvidersName;
    use buck2_core::target::TargetName;
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
                        agg.add_cell_alias_entry(
                            ProjectRelativePathBuf::try_from((*path).to_owned())?,
                            CellAlias::new((*alias).to_owned()),
                            ProjectRelativePathBuf::try_from((*alias_path).to_owned())?,
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
            let aliases = self
                .resolver
                .get(&CellName::unchecked_new("root".to_owned()))
                .unwrap()
                .cell_alias_resolver();

            let patterns: Vec<_> =
                patterns.map(|p| ParsedPattern::<T>::parse_precise(aliases, p).unwrap());

            resolve_target_patterns(&self.resolver, patterns.iter(), &*self.file_ops).await
        }
    }

    trait ResolvedTargetPatternTestExt<T> {
        fn assert_eq(&self, expected: &[(Package, PackageSpec<T>)]);
    }

    impl<T> ResolvedTargetPatternTestExt<T> for ResolvedPattern<T>
    where
        T: PatternType,
    {
        fn assert_eq(&self, expected: &[(Package, PackageSpec<T>)]) {
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
        tester.resolve::<TargetPattern>(&[]).await?.assert_eq(&[]);
        tester
            .resolve::<TargetPattern>(&[
                "//some:target",
                "//some:other_target",
                "child//a/package:",
            ])
            .await?
            .assert_eq(&[
                (
                    Package::testing_new("root", "some"),
                    PackageSpec::Targets(vec![
                        TargetName::unchecked_new("target"),
                        TargetName::unchecked_new("other_target"),
                    ]),
                ),
                (Package::testing_new("child", "a/package"), PackageSpec::All),
            ]);
        Ok(())
    }

    #[tokio::test]
    async fn test_simple_specs_providers() -> anyhow::Result<()> {
        let tester = TestPatternResolver::new(&[("root", ""), ("child", "child/cell")], &[])?;
        tester
            .resolve::<ProvidersPattern>(&[])
            .await?
            .assert_eq(&[]);
        tester
            .resolve::<ProvidersPattern>(&[
                "//some:target",
                "//some:other_target[my-label]",
                "child//a/package:",
            ])
            .await?
            .assert_eq(&[
                (
                    Package::testing_new("root", "some"),
                    PackageSpec::Targets(vec![
                        (TargetName::unchecked_new("target"), ProvidersName::Default),
                        (
                            TargetName::unchecked_new("other_target"),
                            ProvidersName::Named(vec![
                                ProviderName::new("my-label".to_owned()).unwrap(),
                            ]),
                        ),
                    ]),
                ),
                (Package::testing_new("child", "a/package"), PackageSpec::All),
            ]);
        Ok(())
    }

    #[test_case(PhantomData::< TargetPattern >; "parsing TargetPattern")]
    #[test_case(PhantomData::< ProvidersPattern >; "parsing ProvidersPattern")]
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
                    (Package::testing_new("root", "other"), PackageSpec::All),
                    (
                        Package::testing_new("root", "other/a/bit/deeper"),
                        PackageSpec::All,
                    ),
                    (
                        Package::testing_new("root", "other/a/bit/deeper/and/deeper"),
                        PackageSpec::All,
                    ),
                    (
                        Package::testing_new("root", "some/thing/dir/a"),
                        PackageSpec::All,
                    ),
                    (
                        Package::testing_new("root", "some/thing/dir/a/b"),
                        PackageSpec::All,
                    ),
                    (
                        Package::testing_new("root", "some/thing/extra"),
                        PackageSpec::All,
                    ),
                    (Package::testing_new("child", ""), PackageSpec::All),
                    (Package::testing_new("child", "foo"), PackageSpec::All),
                ]);
        })
    }
}
