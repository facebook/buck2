/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    fmt,
    hash::{Hash, Hasher},
    sync::Arc,
};

use gazebo::{any::ProvidesStaticType, prelude::*};
use starlark::{
    collections::StarlarkHasher,
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{Freeze, Heap, NoSerialize, StarlarkValue, Trace, Value, ValueLike},
};

use crate::interpreter::rule_defs::{
    artifact_tagging::TaggedArtifacts, cmd_args::ValueAsCommandLineLike,
};

/// ArtifactTag allows wrapping input and output artifacts in a command line with tags. Those tags
/// will be made visible to artifact visitors. The tags themselves don't have meaning on their own,
/// but they can be compared to each other, which allows grouping inputs and outputs in meaningful
/// categories. This is notably used for dep files to associate inputs tracked by a dep file with
/// the dep file itself.
#[derive(Debug, Clone, Dupe, Freeze, Trace, ProvidesStaticType, NoSerialize)]
pub struct ArtifactTag {
    #[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_arc_on_dupe))]
    #[trace(unsafe_ignore)]
    #[freeze(identity)]
    identity: Arc<()>,
}

impl ArtifactTag {
    pub fn new() -> Self {
        Self {
            identity: Arc::new(()),
        }
    }
}

impl fmt::Display for ArtifactTag {
    fn fmt(&self, w: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(w, "ArtifactTag({:x})", Arc::as_ptr(&self.identity) as usize)
    }
}

impl PartialEq for ArtifactTag {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.identity, &other.identity)
    }
}

impl Eq for ArtifactTag {}

impl Hash for ArtifactTag {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        hasher.write_usize(Arc::as_ptr(&self.identity) as usize);
    }
}

starlark_simple_value!(ArtifactTag);

impl<'v> StarlarkValue<'v> for ArtifactTag {
    starlark_type!("artifact_tag");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(input_tag_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        Ok(match other.downcast_ref::<Self>() {
            Some(other) => self == other,
            None => false,
        })
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        Hash::hash(self, hasher);
        Ok(())
    }
}

#[starlark_module]
fn input_tag_methods(_: &mut MethodsBuilder) {
    fn tag_artifacts<'v>(
        this: &ArtifactTag,
        inner: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        // Check that the inner is actually a command line.
        let _inner = inner.as_command_line_err()?;

        Ok(heap.alloc(TaggedArtifacts::new(inner, this.dupe())))
    }
}

#[cfg(test)]
mod test {
    use indoc::indoc;
    use starlark::environment::GlobalsBuilder;

    use super::*;
    use crate::interpreter::testing::Tester;

    #[starlark_module]
    pub fn artifact_tag_factory(builder: &mut GlobalsBuilder) {
        fn make_tag() -> anyhow::Result<ArtifactTag> {
            Ok(ArtifactTag::new())
        }
    }

    #[test]
    fn test_artifact_tag_eq() {
        let t1 = ArtifactTag::new();
        let t2 = ArtifactTag::new();

        assert_eq!(t1, t1.dupe());
        assert_ne!(t1, t2);
    }

    #[test]
    fn test_artifact_tag_starlark_eq() -> anyhow::Result<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(Arc::new(artifact_tag_factory));

        tester.run_starlark_bzl_test(indoc!(
            r#"
        def test():
            t1 = make_tag()
            t2 = make_tag()

            assert_eq(t1, t1)
            assert_ne(t1, t2)
        "#
        ))?;

        Ok(())
    }
}
