use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use buck2_data::SharedTaskEnd;
use buck2_data::SharedTaskStart;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::dispatch::Span;
use buck2_events::TraceId;
use futures::future::Future;
use futures::future::Shared;
use futures::FutureExt;
use gazebo::prelude::*;
use pin_project::pin_project;

/// A custom future type that wraps a Shared future that is possibly polled in different threads with
/// their own distinct dispatchers. The poll behaviour sends a SharedTask span to the thread
/// sharing the future.
#[pin_project]
pub struct SharedEventsFuture<F>
where
    F: Future,
{
    #[pin]
    inner: Shared<F>,
    /// Because poll is called repeatedly, we don't want to send duplicate start events.
    /// `span` is set on first-entry of a cross-thread poll and used on completion for
    /// sending a SpanEnd event.
    span: Option<Span>,
    /// Used to compare against task_local dispatcher, to determine if future is being
    /// polled in the context of another dispatcher.
    owner_trace_id: TraceId,
}

impl<F> SharedEventsFuture<F>
where
    F: Future,
    F::Output: Clone,
{
    pub fn new(fut: F) -> SharedEventsFuture<F> {
        let dispatcher = get_dispatcher().unwrap();
        SharedEventsFuture {
            inner: fut.shared(),
            span: None,
            owner_trace_id: dispatcher.trace_id().dupe(),
        }
    }
}

impl<F> Clone for SharedEventsFuture<F>
where
    F: Future,
{
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            // span is None because it's used for tracking the state of a specific future.
            span: None,
            owner_trace_id: self.owner_trace_id.dupe(),
        }
    }
}

impl<F> Future for SharedEventsFuture<F>
where
    F: Future,
    F::Output: Clone,
{
    type Output = F::Output;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project();

        // Have not yet sent span start
        if this.span.is_none() {
            let dispatcher = get_dispatcher().unwrap();
            // Polling this future in the context of another dispatcher
            if *this.owner_trace_id != *dispatcher.trace_id() {
                // Send start event
                *this.span = Some(Span::start(dispatcher, SharedTaskStart {}));
            }
        }

        match this.inner.poll(cx) {
            Poll::Ready(t) => {
                // Poll is in the context of another dispatcher
                if let Some(span) = this.span.take() {
                    // Send end event
                    span.end(SharedTaskEnd {});
                }
                Poll::Ready(t)
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

pub trait SharedEvents: Future {
    fn instrumented_shared(self) -> SharedEventsFuture<Self>
    where
        Self: Sized,
        Self::Output: Clone,
    {
        SharedEventsFuture::new(self)
    }
}

impl<T> SharedEvents for dyn futures::Future<Output = T> {}

impl<T> SharedEvents for Pin<Box<dyn futures::Future<Output = T> + Send>> {}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;
    use std::sync::Arc;

    use buck2_events::create_source_sink_pair;
    use buck2_events::dispatch::span;
    use buck2_events::dispatch::with_dispatcher_async;
    use buck2_events::dispatch::EventDispatcher;
    use buck2_events::BuckEvent;
    use buck2_events::EventSource;
    use buck2_events::TraceId;
    use futures::future::BoxFuture;
    use futures::FutureExt;
    use tokio::sync::Barrier;
    use tokio::sync::Mutex;

    use super::*;

    fn create_dispatcher() -> (impl EventSource, EventDispatcher, TraceId) {
        let (events, sink) = create_source_sink_pair();
        let trace_id = TraceId::new();
        let dispatcher = EventDispatcher::new(trace_id.dupe(), sink);
        (events, dispatcher, trace_id)
    }

    async fn next_event<E>(source: &mut E) -> BuckEvent
    where
        E: EventSource,
    {
        let event = tokio::time::timeout(std::time::Duration::from_secs(3), async {
            source.receive()
        })
        .await
        .unwrap();
        event.unwrap().unpack_buck().unwrap().clone()
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn poll_shared_future() {
        let mut handles = vec![];
        let n_thread = 2;
        let (mut events1, dispatcher1, _trace_id1) = create_dispatcher();
        let (mut events2, dispatcher2, _trace_id2) = create_dispatcher();

        // Spawned future that is created by task1, shared with task2 - each with their own
        // dispatchers.
        let fut: Arc<Mutex<Option<SharedEventsFuture<BoxFuture<'static, &str>>>>> =
            Arc::new(Mutex::new(None));

        let barrier = Arc::new(Barrier::new(n_thread));

        let fut1 = fut.dupe();
        let b1 = barrier.dupe();
        // task1 - creates shared future
        handles.push(tokio::spawn(async move {
            let shared_future = with_dispatcher_async(dispatcher1, async {
                span(
                    buck2_data::FakeStart {
                        caramba: "".to_owned(),
                    },
                    || ("Hello world!", buck2_data::FakeEnd {}),
                )
            })
            .boxed()
            .instrumented_shared();
            let mut fut = fut1.lock().await;
            *fut = Some(shared_future.clone());
            // Release mutex before waiting on barrier.
            drop(fut);
            // Raise barrier to let task1 and task2 poll shared future concurrently.
            b1.wait().await;
            shared_future.await;
        }));

        let fut2 = fut.dupe();
        let b2 = barrier.dupe();
        // task2 - shares shared future created by task1. Should receive SharedTask span events.
        handles.push(tokio::spawn(with_dispatcher_async(
            dispatcher2,
            async move {
                b2.wait().await;
                // Ensures that the shared future has been created.
                let mut guard = fut2.lock().await;
                let shared_future = guard.as_mut().unwrap();
                shared_future.await;
            },
        )));

        futures::future::join_all(handles).await;

        // The "owning" dispatcher should receive the fake start/end events
        let owned_start_event = next_event(&mut events1).await;
        assert_matches!(
            owned_start_event.data,
            buck2_data::buck_event::Data::SpanStart(buck2_data::SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Fake(_)),
            }),
            "expected FakeStart, got {:?}",
            &owned_start_event,
        );
        let owned_end_event = next_event(&mut events1).await;
        assert_matches!(
            owned_end_event.data,
            buck2_data::buck_event::Data::SpanEnd(buck2_data::SpanEndEvent {
                data: Some(buck2_data::span_end_event::Data::Fake(_)),
                ..
            }),
            "expected FakeEnd, got {:?}",
            &owned_end_event,
        );

        let shared_start_event = next_event(&mut events2).await;
        assert_matches!(
            shared_start_event.data,
            buck2_data::buck_event::Data::SpanStart(buck2_data::SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::SharedTask(_)),
            }),
            "expected SharedTaskStart, got {:?}",
            &shared_start_event,
        );

        let shared_end_event = next_event(&mut events2).await;
        assert_matches!(
            shared_end_event.data,
            buck2_data::buck_event::Data::SpanEnd(buck2_data::SpanEndEvent {
                data: Some(buck2_data::span_end_event::Data::SharedTask(
                    SharedTaskEnd {}
                )),
                ..
            }),
            "expected SharedTaskEnd, got {:?}",
            shared_end_event
        );
    }
}
