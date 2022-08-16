use std::future::Future;

use buck2_interpreter::dice::HasEvents;
use dice::DiceComputations;
use events::dispatch::with_dispatcher_async;
use gazebo::prelude::*;

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
        let dispatcher = self.0.per_transaction_data().get_dispatcher().dupe();
        tokio::runtime::Handle::current().block_on(with_dispatcher_async(dispatcher, f(self.0)))
    }

    /// runs any async computation
    pub fn via<Fut, R>(&self, f: impl FnOnce() -> Fut) -> R
    where
        Fut: Future<Output = R>,
    {
        let dispatcher = self.0.per_transaction_data().get_dispatcher().dupe();
        tokio::runtime::Handle::current().block_on(with_dispatcher_async(dispatcher, f()))
    }
}
