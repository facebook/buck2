/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::dice::HasEvents;
use events::dispatch::with_dispatcher_async;
use futures::future::BoxFuture;
use gazebo::dupe::Dupe;
use more_futures::spawner::Spawner;
use tokio::task::JoinHandle;

#[derive(Default)]
pub struct BuckSpawner;

impl<T: HasEvents> Spawner<T> for BuckSpawner {
    fn spawn(&self, ctx: &T, fut: BoxFuture<'static, Option<()>>) -> JoinHandle<Option<()>> {
        let dispatcher = ctx.get_dispatcher().dupe();
        let task = async move { with_dispatcher_async(dispatcher, fut).await };
        tokio::spawn(task)
    }
}

#[cfg(test)]
mod tests {
    use buck2_data::{CommandEnd, CommandStart};
    use dice::{data::DiceData, UserComputationData};
    use events::{
        create_source_sink_pair,
        dispatch::{span, EventDispatcher},
        BuckEvent, EventSource, TraceId,
    };
    use futures::future::FutureExt;

    use super::*;

    async fn next_event<T>(source: &mut T) -> BuckEvent
    where
        T: EventSource,
    {
        source.receive().unwrap().unpack_buck().unwrap().clone()
    }

    #[tokio::test]
    async fn test_spawn() {
        let sp = BuckSpawner::default();

        // Create dispatcher
        let (mut events, sink) = create_source_sink_pair();
        let trace_id = TraceId::new();
        let dispatcher = EventDispatcher::new(trace_id.dupe(), sink);

        // Create UserComputationData
        let dice_data = {
            let mut data = DiceData::new();
            data.set(dispatcher);
            data
        };
        let ctx = UserComputationData {
            data: dice_data,
            ..Default::default()
        };

        let start = CommandStart {
            data: Default::default(),
            metadata: Default::default(),
        };
        let end = CommandEnd {
            data: Default::default(),
            metadata: Default::default(),
            is_success: true,
            error_messages: vec![],
        };

        let task = async {
            span(start, || ((), end));
            Some(())
        }
        .boxed();

        sp.spawn(&ctx, task).await.expect("Task panicked");

        // Check that the events are received successfully & correctly.
        let event = next_event(&mut events).await;
        let span_id = event.span_id.unwrap();
        assert_eq!(event.trace_id, trace_id);
        assert!(event.parent_id.is_none());

        let end = next_event(&mut events).await;
        assert_eq!(end.span_id.unwrap(), span_id);
    }
}
