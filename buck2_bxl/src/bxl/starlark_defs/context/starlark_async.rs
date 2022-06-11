use std::future::Future;

use dice::DiceComputations;

/// Provides a safe blocking calls to async functions for starlark that requires operations to
/// be not async.
///
/// This is not exposed to starlark but rather, used by operations exposed to starlark to run
/// code.
/// This also provides a handle for dice.
pub struct BxlSafeDiceComputations<'a>(pub(crate) &'a DiceComputations);

impl<'a> BxlSafeDiceComputations<'a> {
    pub fn new(dice: &'a DiceComputations) -> Self {
        Self(dice)
    }

    /// runs the async computation over dice as sync
    pub fn via_dice<Fut, R>(&self, f: impl FnOnce(&'a DiceComputations) -> Fut) -> R
    where
        Fut: Future<Output = R>,
    {
        tokio::runtime::Handle::current().block_on(f(self.0))
    }

    /// runs any async computation
    pub fn via<Fut, R>(&self, f: impl FnOnce() -> Fut) -> R
    where
        Fut: Future<Output = R>,
    {
        tokio::runtime::Handle::current().block_on(f())
    }
}
