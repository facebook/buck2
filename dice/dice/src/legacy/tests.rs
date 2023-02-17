/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dupe::Dupe;

use crate::api::cycles::DetectCycles;
use crate::api::injected::InjectedKey;
use crate::legacy::ctx::testing::DiceCtxExt;
use crate::legacy::incremental::versions::MinorVersion;
use crate::legacy::DiceLegacy;
use crate::versions::VersionNumber;

#[derive(Clone, Dupe, Debug, Display, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "{:?}", self)]
struct Foo(i32);

#[async_trait]
impl InjectedKey for Foo {
    type Value = i32;

    fn compare(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[tokio::test]
async fn compute_and_update_uses_proper_version_numbers() -> anyhow::Result<()> {
    let dice = DiceLegacy::builder().build(DetectCycles::Enabled);

    {
        let ctx = dice.updater().commit();
        assert_eq!(ctx.0.0.get_version(), VersionNumber::new(0));
        assert_eq!(ctx.0.0.get_minor_version(), MinorVersion::testing_new(0));
    }

    {
        // second context that didn't have any writes should still be the same version
        let ctx = dice.updater().commit();
        assert_eq!(ctx.0.0.get_version(), VersionNumber::new(0));
        assert_eq!(ctx.0.0.get_minor_version(), MinorVersion::testing_new(1));

        // now we write something and commit
        let mut ctx = dice.updater();
        ctx.changed_to(vec![(Foo(1), 1)])?;
        // current version shouldn't be updated
        assert_eq!(
            ctx.existing_state().await.0.get_version(),
            VersionNumber::new(0)
        );
        assert_eq!(
            ctx.existing_state().await.0.get_minor_version(),
            MinorVersion::testing_new(1)
        );

        let mut ctx1 = dice.updater();
        // previous ctx isn't dropped, so versions shouldn't be committed yet.
        assert_eq!(
            ctx1.existing_state().await.0.get_version(),
            VersionNumber::new(0)
        );
        assert_eq!(
            ctx1.existing_state().await.0.get_minor_version(),
            MinorVersion::testing_new(1)
        );

        // if we update on the new context, nothing committed
        ctx1.changed_to(vec![(Foo(2), 2)])?;
        assert_eq!(
            ctx1.existing_state().await.0.get_version(),
            VersionNumber::new(0)
        );
        assert_eq!(
            ctx1.existing_state().await.0.get_minor_version(),
            MinorVersion::testing_new(1)
        );

        // drop a context
        ctx1.commit();
        // we should only have committed once, and in increasing order
        let vg = dice.global_versions.current();
        assert_eq!(
            (vg.version, *vg.minor_version_guard),
            (VersionNumber::new(1), MinorVersion::testing_new(1))
        );

        ctx.commit();
        // both versions finalized.
        let vg = dice.global_versions.current();
        assert_eq!(
            (vg.version, *vg.minor_version_guard),
            (VersionNumber::new(2), MinorVersion::testing_new(1))
        );
        assert!(dice.map.read().engines().iter().all(|engine| {
            engine
                .introspect()
                .versions_currently_running()
                .first()
                .is_none()
        }));
    }

    {
        let mut ctx = dice.updater();
        assert_eq!(
            ctx.existing_state().await.0.get_version(),
            VersionNumber::new(2)
        );
        assert_eq!(
            ctx.existing_state().await.0.get_minor_version(),
            MinorVersion::testing_new(2)
        );

        ctx.changed_to(vec![(Foo(3), 3)])?;
        assert_eq!(
            ctx.existing_state().await.0.get_version(),
            VersionNumber::new(2)
        );
        assert_eq!(
            ctx.existing_state().await.0.get_minor_version(),
            MinorVersion::testing_new(2)
        );

        ctx.commit();
        let vg = dice.global_versions.current();
        assert_eq!(
            (vg.version, *vg.minor_version_guard),
            (VersionNumber::new(3), MinorVersion::testing_new(1))
        );
        assert!(dice.map.read().engines().iter().all(|engine| {
            engine
                .introspect()
                .versions_currently_running()
                .first()
                .is_none()
        }));
    }

    Ok(())
}
