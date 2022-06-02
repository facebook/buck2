/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use buck2_core::provider::{ConfiguredProvidersLabel, ProvidersLabel, ProvidersName};
use derive_more::Display;
use gazebo::{
    any::{AnyLifetime, ProvidesStaticType},
    coerce::Coerce,
    prelude::*,
};
use serde::{Serialize, Serializer};
use starlark::{
    collections::StarlarkHasher,
    environment::{Methods, MethodsBuilder, MethodsStatic},
    starlark_complex_value, starlark_type,
    values::{AllocValue, Freeze, Heap, StarlarkValue, Trace, Value, ValueLike},
};

use crate::interpreter::rule_defs::{
    cell_root::CellRoot, label_relative_path::LabelRelativePath, target_label::StarlarkTargetLabel,
};

impl<V> LabelGen<V> {
    pub fn label(&self) -> &ConfiguredProvidersLabel {
        &self.label
    }
}

/// Container for `ConfiguredProvidersLabel` that gives users access to things like package, cell, etc. This can also be properly stringified by our forthcoming `CommandLine` object
#[derive(Clone, Debug, Coerce, Display, Trace, Freeze, AnyLifetime)]
#[display(fmt = "{}", label)]
#[repr(C)]
pub struct LabelGen<V> {
    // TODO(nmj): We don't really want to allocate these up front, but we don't
    //                 get the heap during get_attr(), so we have to for now. Revisit
    //                 later, because most people probably don't actually need this.
    package_string: V,
    name_string: V,
    provider_string: V,
    #[trace(unsafe_ignore)]
    #[freeze(identity)]
    label: ConfiguredProvidersLabel,
}

starlark_complex_value!(pub Label);

impl<V> Serialize for LabelGen<V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.label.serialize(serializer)
    }
}

impl<'v> Label<'v> {
    pub(crate) fn new(heap: &'v Heap, label: ConfiguredProvidersLabel) -> Self {
        let package_string = heap.alloc(label.target().pkg().cell_relative_path().as_str());
        let name_string = heap.alloc(label.target().name().as_ref());
        let provider_string = match label.name() {
            ProvidersName::Default => Value::new_none(),
            ProvidersName::Named(s) => {
                heap.alloc_list_iter(s.iter().map(|p| heap.alloc(p.as_str())))
            }
            ProvidersName::UnrecognizedFlavor(_) => unreachable!(
                "This should have been an error when looking up the corresponding analysis (`{}`)",
                label
            ),
        };
        Label {
            package_string,
            name_string,
            provider_string,
            label,
        }
    }
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for LabelGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("label");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configured_label_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        if let Some(other) = Label::from_value(other) {
            Ok(self.package_string.equals(other.package_string)?
                && self.name_string.equals(other.name_string)?
                && self.provider_string.equals(other.provider_string)?
                && self.label == other.label)
        } else {
            Ok(false)
        }
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.label.hash(hasher);
        Ok(())
    }
}

#[starlark_module]
fn configured_label_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn package<'v>(this: &Label) -> anyhow::Result<Value<'v>> {
        Ok(this.package_string.to_value())
    }

    #[starlark(attribute)]
    fn name<'v>(this: &Label) -> anyhow::Result<Value<'v>> {
        Ok(this.name_string.to_value())
    }

    #[starlark(attribute)]
    fn sub_target<'v>(this: &Label) -> anyhow::Result<Value<'v>> {
        Ok(this.provider_string.to_value())
    }

    #[starlark(attribute)]
    fn path<'v>(this: &Label, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let path = LabelRelativePath(this.label.target().pkg().to_cell_path());
        Ok(path.alloc_value(heap))
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &Label, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let cell = this.label.target().pkg().cell_name().as_str();
        Ok(heap.alloc(cell))
    }

    #[starlark(attribute)]
    fn cell_root<'v>(this: &Label, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let cell_root = CellRoot::new(this.label.target().pkg().cell_name().clone());
        Ok(heap.alloc(cell_root))
    }

    /// Returns the unconfigured underlying target label.
    fn raw_target(this: &Label) -> anyhow::Result<StarlarkTargetLabel> {
        Ok(StarlarkTargetLabel::new(
            (*this.label.target().unconfigured()).dupe(),
        ))
    }
}

impl<V> StarlarkProvidersLabelGen<V> {
    pub fn label(&self) -> &ProvidersLabel {
        &self.label
    }
}

/// Container for `ProvidersLabel` that gives users access to things like package, cell, etc.
#[derive(Clone, Debug, Coerce, Display, Trace, Freeze, AnyLifetime, Serialize)]
#[display(fmt = "{}", label)]
#[repr(C)]
pub struct StarlarkProvidersLabelGen<V> {
    #[serde(skip)]
    name_string: V,
    #[serde(skip)]
    provider_string: V,
    #[trace(unsafe_ignore)]
    #[freeze(identity)]
    #[serde(flatten)]
    label: ProvidersLabel,
}

starlark_complex_value!(pub StarlarkProvidersLabel);

impl<'v> StarlarkProvidersLabel<'v> {
    pub(crate) fn new(heap: &'v Heap, label: ProvidersLabel) -> Self {
        let name_string = heap.alloc(label.target().name().as_ref());
        let provider_string = match label.name() {
            ProvidersName::Default => Value::new_none(),
            ProvidersName::Named(s) => {
                heap.alloc_list_iter(s.iter().map(|p| heap.alloc(p.as_str())))
            }
            ProvidersName::UnrecognizedFlavor(_) => unreachable!(
                "This should have been an error when looking up the corresponding analysis (`{}`)",
                label
            ),
        };
        StarlarkProvidersLabel {
            name_string,
            provider_string,
            label,
        }
    }
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkProvidersLabelGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("providers_label");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(label_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        if let Some(other) = StarlarkProvidersLabel::from_value(other) {
            Ok(self.label == other.label)
        } else {
            Ok(false)
        }
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.label.hash(hasher);
        Ok(())
    }
}

#[starlark_module]
fn label_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn name<'v>(this: &StarlarkProvidersLabel) -> anyhow::Result<Value<'v>> {
        Ok(this.name_string.to_value())
    }

    #[starlark(attribute)]
    fn sub_target<'v>(this: &StarlarkProvidersLabel) -> anyhow::Result<Value<'v>> {
        Ok(this.provider_string.to_value())
    }

    #[starlark(attribute)]
    fn path<'v>(this: &StarlarkProvidersLabel, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let path = LabelRelativePath(this.label.target().pkg().to_cell_path());
        Ok(path.alloc_value(heap))
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &StarlarkProvidersLabel) -> anyhow::Result<&'v str> {
        let cell = this.label.target().pkg().cell_name().as_str();
        Ok(cell)
    }

    /// Returns the unconfigured underlying target label.
    fn raw_target(this: &StarlarkProvidersLabel) -> anyhow::Result<StarlarkTargetLabel> {
        Ok(StarlarkTargetLabel::new((*this.label.target()).dupe()))
    }
}

#[cfg(test)]
pub mod testing {
    use buck2_core::{configuration::Configuration, provider::ProvidersLabel, target::TargetLabel};
    use buck2_interpreter::{
        extra::BuildContext,
        pattern::{ParsedPattern, ProvidersPattern},
    };
    use starlark::{environment::GlobalsBuilder, eval::Evaluator};

    use crate::interpreter::rule_defs::label::Label;

    #[starlark_module]
    pub fn label_creator(builder: &mut GlobalsBuilder) {
        fn label<'v>(s: &str, eval: &mut Evaluator) -> anyhow::Result<Label<'v>> {
            let c = BuildContext::from_context(eval)?;
            let target = match ParsedPattern::<ProvidersPattern>::parse_precise(
                c.cell_info().cell_alias_resolver(),
                s,
            ) {
                Ok(ParsedPattern::Target(package, (target_name, providers_name))) => {
                    ProvidersLabel::new(TargetLabel::new(package, target_name), providers_name)
                }
                _ => {
                    eprintln!("Expected a target, not {}", s);
                    panic!();
                }
            };
            Ok(Label::new(
                eval.heap(),
                target.configure(Configuration::testing_new()),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;

    use super::testing::label_creator;
    use crate::interpreter::testing::{expect_error, Tester};

    #[test]
    fn labels_are_usable() -> anyhow::Result<()> {
        fn new_tester() -> anyhow::Result<Tester> {
            let mut tester = Tester::new()?;
            tester.set_additional_globals(Arc::new(label_creator));
            Ok(tester)
        }

        let mut tester = new_tester()?;
        tester.run_starlark_test(indoc!(
            r#"
            frozen_l_default = label("root//foo/bar:baz")
            frozen_l = label("root//foo/bar:baz[something]")
            def test():
                l_default = label("root//foo/bar:baz")
                l = label("root//foo/bar:baz[something]")

                assert_eq("root//foo/bar:baz (<testing>)", repr(frozen_l_default))
                assert_eq("root//foo/bar:baz (<testing>)", str(frozen_l_default))
                assert_eq("foo/bar", frozen_l_default.package)
                assert_eq("baz", frozen_l_default.name)
                assert_eq(None, frozen_l_default.sub_target)
                assert_eq("root", frozen_l_default.cell)

                assert_eq("root//foo/bar:baz[something] (<testing>)", repr(frozen_l))
                assert_eq("root//foo/bar:baz[something] (<testing>)", str(frozen_l))
                assert_eq("foo/bar", frozen_l.package)
                assert_eq("baz", frozen_l.name)
                assert_eq(["something"], frozen_l.sub_target)

                assert_eq("root//foo/bar:baz (<testing>)", repr(l_default))
                assert_eq("root//foo/bar:baz (<testing>)", str(l_default))
                assert_eq("foo/bar", l_default.package)
                assert_eq("baz", l_default.name)
                assert_eq(None, l_default.sub_target)

                assert_eq("root//foo/bar:baz[something] (<testing>)", repr(l))
                assert_eq("root//foo/bar:baz[something] (<testing>)", str(l))
                assert_eq("foo/bar", l.package)
                assert_eq("baz", l.name)
                assert_eq(["something"], l.sub_target)
                assert_eq("root", l.cell)

            "#
        ))?;

        let mut tester = new_tester()?;
        let invalid_fields = indoc!(
            r#"
            l = label("root//foo:bar[baz]")
            def test():
                l.invalid_field
            "#
        );
        expect_error(
            tester.run_starlark_test(invalid_fields),
            invalid_fields,
            "Object of type `label` has no attribute `invalid_field`",
        );
        Ok(())
    }
}
