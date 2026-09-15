/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the above-listed licenses.
 */

//! Versioned recovery from lost filesystem notifications. Only providers that
//! enable this at DICE construction add the dependency to filesystem reads.

use allocative::Allocative;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceDataBuilder;
use dice::DiceTransactionUpdater;
use dice::EqualityBehavior;
use dice::InjectedKey;
use dice::PagableValueSerialize;
use dice::ValueSerialize;
use dupe::Dupe;
use pagable::Pagable;
use pagable::pagable_typetag;

#[derive(Allocative)]
struct FileSystemInvalidationEnabled(bool);

#[derive(Clone, Dupe, Debug, Display, Eq, PartialEq, Hash, Allocative, Pagable)]
#[pagable_typetag(dice::DiceKeyDyn)]
struct FileSystemInvalidationKey;

impl InjectedKey for FileSystemInvalidationKey {
    type Value = u64;

    fn equality_behavior() -> EqualityBehavior<u64> {
        EqualityBehavior::Compare(|a, b| a == b)
    }

    fn value_serialize() -> impl ValueSerialize<Value = u64> {
        PagableValueSerialize::<u64>::new()
    }
}

/// Configure once at DICE construction, based on the watcher provider.
pub fn configure_filesystem_invalidation(builder: &mut DiceDataBuilder, enabled: bool) {
    builder.set(FileSystemInvalidationEnabled(enabled));
}

/// Inject the watcher's current token before filesystem computations run.
/// Re-injecting the same token does not dirty inputs.
pub fn set_filesystem_invalidation_token(
    updater: &mut DiceTransactionUpdater,
    token: u64,
) -> buck2_error::Result<()> {
    updater.changed_to([(FileSystemInvalidationKey, token)])?;
    Ok(())
}

pub(super) async fn record_filesystem_dependency(
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<()> {
    if ctx
        .global_data()
        .get::<FileSystemInvalidationEnabled>()
        .is_ok_and(|enabled| enabled.0)
    {
        ctx.compute(&FileSystemInvalidationKey).await?;
    }
    Ok(())
}
