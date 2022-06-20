/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{fmt, fmt::Display, marker::PhantomData, sync::Arc};

use buck2_core::cells::paths::CellPath;

/// A unique identity for a given provider. Allows correlating `ProviderCallable` objects with `UserProvider` objects.
///
/// For example:
/// ```ignore
/// FooInfo = provider(fields=["foo", "bar"])
///
/// def impl(ctx):
///     # We can guarantee when setting up the context that there
///     # is a provider that came from FooInfo
///     ctx.actions.write("out.txt", ctx.attr.dep[FooInfo].bar)
/// foo_binary = rule(implementation=impl, attrs={"dep": attr.dep(providers=[FooInfo])})
/// ```
#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct ProviderId {
    /// This is present for all user-specified providers. This is only None if it is a
    /// native provider, which has no affiliated .bzl file
    pub(crate) path: Option<CellPath>,
    pub(crate) name: String,
}

impl Display for ProviderId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)
    }
}

pub struct ProviderIdWithType<T> {
    id: Arc<ProviderId>,
    t: PhantomData<T>,
}

impl<T> ProviderIdWithType<T> {
    pub fn id(&self) -> &Arc<ProviderId> {
        &self.id
    }

    pub fn new(path: Option<CellPath>, name: String) -> Self {
        Self {
            id: Arc::new(ProviderId { path, name }),
            t: Default::default(),
        }
    }
}

impl ProviderId {
    pub fn name(&self) -> &str {
        &self.name
    }
}

#[cfg(test)]
pub mod testing {
    use buck2_core::cells::paths::CellPath;

    use crate::interpreter::rule_defs::provider::id::ProviderId;

    pub trait ProviderIdExt {
        fn testing_new(path: CellPath, name: &str) -> Self;
    }

    impl ProviderIdExt for ProviderId {
        fn testing_new(path: CellPath, name: &str) -> Self {
            ProviderId {
                path: Some(path),
                name: name.to_owned(),
            }
        }
    }
}
