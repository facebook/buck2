/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;

use buck2_common::events::HasEvents;
use buck2_events::dispatch::with_dispatcher_async;
use dupe::Dupe;
use futures::future::BoxFuture;
use more_futures::spawner::Spawner;
use tokio::task::JoinHandle;

#[derive(Default)]
pub struct BuckSpawner;

impl<T: HasEvents> Spawner<T> for BuckSpawner {
    fn spawn(
        &self,
        ctx: &T,
        fut: BoxFuture<'static, Box<dyn Any + Send + 'static>>,
    ) -> JoinHandle<Box<dyn Any + Send + 'static>> {
        let dispatcher = ctx.get_dispatcher().dupe();
        let task = async move { with_dispatcher_async(dispatcher, fut).await };
        tokio::spawn(task)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_data::CommandEnd;
    use buck2_data::CommandStart;
    use buck2_events::create_source_sink_pair;
    use buck2_events::dispatch::span;
    use buck2_events::dispatch::EventDispatcher;
    use buck2_events::BuckEvent;
    use buck2_events::EventSource;
    use buck2_wrapper_common::invocation_id::TraceId;
    use dice::DiceData;
    use dice::UserComputationData;
    use futures::future::FutureExt;
    use more_futures::spawn::spawn_dropcancel_with_preamble;

    use super::*;

    async fn next_event<T>(source: &mut T) -> BuckEvent
    where
        T: EventSource,
    {
        source.receive().unwrap().unpack_buck().unwrap().clone()
    }

    fn create_ctx(dispatcher: EventDispatcher) -> UserComputationData {
        let dice_data = {
            let mut data = DiceData::new();
            data.set(dispatcher);
            data
        };
        UserComputationData {
            data: dice_data,
            ..Default::default()
        }
    }

    fn create_start_end_events() -> (CommandStart, CommandEnd) {
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

        (start, end)
    }

    #[tokio::test]
    async fn test_spawn() {
        let sp = BuckSpawner;

        // Create dispatcher
        let (mut events, sink) = create_source_sink_pair();
        let trace_id = TraceId::new();
        let dispatcher = EventDispatcher::new(trace_id.dupe(), sink);

        let ctx = create_ctx(dispatcher);
        let (start, end) = create_start_end_events();

        let task = async { span(start, || (Box::new(()) as _, end)) }.boxed();

        sp.spawn(&ctx, task).await.expect("Task panicked");

        // Check that the events are received successfully & correctly.
        let event = next_event(&mut events).await;
        let span_id = event.span_id().unwrap();
        assert_eq!(event.trace_id().unwrap(), trace_id);
        assert!(event.parent_id().is_none());

        let end = next_event(&mut events).await;
        assert_eq!(end.span_id().unwrap(), span_id);
    }

    #[tokio::test]
    async fn test_spawn_task() {
        let sp = Arc::new(BuckSpawner);

        // Create dispatchers
        let (mut events1, sink1) = create_source_sink_pair();
        let trace_id1 = TraceId::new();
        let dispatcher1 = EventDispatcher::new(trace_id1.dupe(), sink1);

        let (mut events2, sink2) = create_source_sink_pair();
        let trace_id2 = TraceId::new();
        let dispatcher2 = EventDispatcher::new(trace_id2.dupe(), sink2);

        let ctx1 = create_ctx(dispatcher1);
        let ctx2 = create_ctx(dispatcher2);
        let (start1, end1) = create_start_end_events();
        let (start2, end2) = create_start_end_events();

        // Create tasks with separate dispatchers
        let task1 = async {
            span(start1, || ((), end1));
            "Hello!"
        }
        .boxed();

        let task2 = async {
            span(start2, || ((), end2));
            "World!"
        }
        .boxed();

        let (_, poll1) = spawn_dropcancel_with_preamble(
            task1,
            futures::future::ready(()),
            sp.as_ref(),
            &ctx1,
            tracing::debug_span!("test"),
        );
        let (_, poll2) = spawn_dropcancel_with_preamble(
            task2,
            futures::future::ready(()),
            sp.as_ref(),
            &ctx2,
            tracing::debug_span!("test"),
        );
        let joins = vec![poll1, poll2];

        assert_eq!(
            futures::future::join_all(joins.into_iter()).await,
            ["Hello!", "World!"]
        );

        // Check that the events are received successfully from the correct event sources.
        let event = next_event(&mut events1).await;
        let span_id = event.span_id().unwrap();
        assert_eq!(event.trace_id().unwrap(), trace_id1);
        assert!(event.parent_id().is_none());

        let end = next_event(&mut events1).await;
        assert_eq!(end.span_id().unwrap(), span_id);

        let event = next_event(&mut events2).await;
        let span_id = event.span_id().unwrap();
        assert_eq!(event.trace_id().unwrap(), trace_id2);
        assert!(event.parent_id().is_none());

        let end = next_event(&mut events2).await;
        assert_eq!(end.span_id().unwrap(), span_id);
    }
}
