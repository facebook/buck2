/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::buck_path::path::BuckPathRef;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::package::PackageLabel;
use buck2_core::plugins::PluginKind;
use buck2_core::target::label::TargetLabel;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::traversal::CoercedAttrTraversal;
use buck2_node::visibility::VisibilityPattern;
use buck2_node::visibility::VisibilityPatternList;
use buck2_node::visibility::WithinViewSpecification;
use dupe::Dupe;

fn indented_within_view(spec: &WithinViewSpecification) -> String {
    match &spec.0 {
        VisibilityPatternList::Public => format!("  {}\n", VisibilityPattern::PUBLIC),
        VisibilityPatternList::List(items) => {
            let mut s = String::new();
            for item in items {
                s.push_str(&format!("  {}\n", item));
            }
            s
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum CheckWithinViewError {
    #[error(
        "Dependency `{}` is not within view (as specified by `within_view` attribute):\n{}",
        _0,
        indented_within_view(_1)
    )]
    DepNotWithinView(TargetLabel, WithinViewSpecification),
}

/// Check that dependencies in attribute do not violate `within_view`.
pub(crate) fn check_within_view(
    attr: &CoercedAttr,
    pkg: PackageLabel,
    attr_type: &AttrType,
    within_view: &WithinViewSpecification,
) -> anyhow::Result<()> {
    if within_view == &WithinViewSpecification::PUBLIC {
        // Shortcut.
        return Ok(());
    }

    struct WithinViewCheckTraversal<'x> {
        pkg: PackageLabel,
        within_view: &'x WithinViewSpecification,
    }

    impl<'x> WithinViewCheckTraversal<'x> {
        fn check_dep_within_view(&self, dep: &TargetLabel) -> anyhow::Result<()> {
            if self.pkg == dep.pkg() || self.within_view.0.matches_target(dep) {
                Ok(())
            } else {
                Err(
                    CheckWithinViewError::DepNotWithinView(dep.dupe(), self.within_view.dupe())
                        .into(),
                )
            }
        }
    }

    impl<'a, 'x> CoercedAttrTraversal<'a> for WithinViewCheckTraversal<'x> {
        fn dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
            self.check_dep_within_view(dep)
        }

        fn plugin_dep(&mut self, dep: &'a TargetLabel, _kind: &PluginKind) -> anyhow::Result<()> {
            self.check_dep_within_view(dep)
        }

        fn exec_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
            self.check_dep_within_view(dep)
        }

        fn toolchain_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
            self.check_dep_within_view(dep)
        }

        fn transition_dep(
            &mut self,
            dep: &'a TargetLabel,
            _tr: &Arc<TransitionId>,
        ) -> anyhow::Result<()> {
            self.check_dep_within_view(dep)
        }

        fn split_transition_dep(
            &mut self,
            dep: &'a TargetLabel,
            _tr: &Arc<TransitionId>,
        ) -> anyhow::Result<()> {
            self.check_dep_within_view(dep)
        }

        fn configuration_dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
            // Skip configuration deps.
            Ok(())
        }

        fn platform_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()> {
            self.check_dep_within_view(dep)
        }

        fn input(&mut self, _input: BuckPathRef) -> anyhow::Result<()> {
            Ok(())
        }
    }

    attr.traverse(
        attr_type,
        pkg.dupe(),
        &mut WithinViewCheckTraversal { pkg, within_view },
    )
}
