/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::pin::Pin;
use std::sync::Arc;
use std::task::Context;
use std::task::Poll;

use anyhow::Context as _;
use http_body::Body;
use http_body::Frame;
use http_body_util::combinators::UnsyncBoxBody;
use prost::bytes::Bytes;
use tokio::sync::Mutex;
use tokio::sync::OwnedSemaphorePermit;
use tokio::sync::Semaphore;
use tokio_util::sync::PollSemaphore;
use tonic::Status;
use tonic::transport::Channel;
use tower::Service;

/// Configuration for the connection pool
#[derive(Debug, Clone)]
pub struct PoolConfig {
    /// Minimum number of connections per host
    pub min_connections: usize,
    /// Maximum number of connections per host
    pub max_connections: usize,
    /// Maximum concurrent streams per connection
    pub max_concurrency_per_connection: usize,
}

/// Opens one more connection to a host. The pool calls it whenever it needs
/// another connection, so the channel should connect lazily.
pub type MakeChannel = Arc<dyn Fn() -> Channel + Send + Sync>;

/// Returns the [`MakeChannel`] for an address, the first time the pool is asked
/// for a channel to it.
pub type MakeHost = Box<dyn Fn(&str) -> anyhow::Result<MakeChannel> + Send + Sync>;

/// A response body wrapper that holds a semaphore permit until the body is fully consumed or dropped.
///
/// This ensures that for streaming RPCs, the stream slot is held for the entire
/// duration of the stream, not just until headers arrive.
pub struct PermitBody<B> {
    inner: B,
    _permit: OwnedSemaphorePermit,
}

impl<B> PermitBody<B> {
    fn new(inner: B, permit: OwnedSemaphorePermit) -> Self {
        Self {
            inner,
            _permit: permit,
        }
    }
}

impl<B> Body for PermitBody<B>
where
    B: Body + Unpin,
{
    type Data = B::Data;
    type Error = B::Error;

    fn poll_frame(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<Frame<Self::Data>, Self::Error>>> {
        Pin::new(&mut self.inner).poll_frame(cx)
    }

    fn is_end_stream(&self) -> bool {
        self.inner.is_end_stream()
    }

    fn size_hint(&self) -> http_body::SizeHint {
        self.inner.size_hint()
    }
}

/// A channel wrapper that provides back-pressure via semaphore permits.
///
/// The permit is held in the response body, so for streaming RPCs the permit
/// is held until the stream is fully consumed or dropped.
pub struct PooledChannel {
    channel: Channel,
    semaphore: PollSemaphore,
    permit: Option<OwnedSemaphorePermit>,
}

impl PooledChannel {
    fn new(channel: Channel, semaphore: Arc<Semaphore>) -> Self {
        Self {
            channel,
            semaphore: PollSemaphore::new(semaphore),
            permit: None,
        }
    }
}

impl Clone for PooledChannel {
    fn clone(&self) -> Self {
        Self {
            channel: self.channel.clone(),
            semaphore: self.semaphore.clone(),
            permit: None,
        }
    }
}

impl Service<http::Request<tonic::body::Body>> for PooledChannel {
    type Response = http::Response<UnsyncBoxBody<Bytes, Status>>;
    type Error = tonic::transport::Error;
    type Future = futures::future::BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        if self.permit.is_none() {
            self.permit = std::task::ready!(self.semaphore.poll_acquire(cx));
        }
        self.channel.poll_ready(cx)
    }

    fn call(&mut self, request: http::Request<tonic::body::Body>) -> Self::Future {
        let permit = self.permit.take();
        let fut = self.channel.call(request);

        Box::pin(async move {
            let response = fut.await?;
            let (parts, body) = response.into_parts();
            match permit {
                Some(permit) => {
                    let permit_body = PermitBody::new(body, permit);
                    Ok(http::Response::from_parts(
                        parts,
                        UnsyncBoxBody::new(permit_body),
                    ))
                }
                None => Ok(http::Response::from_parts(parts, UnsyncBoxBody::new(body))),
            }
        })
    }
}

/// Shared state for a pooled channel (used for channel selection)
struct PooledChannelState {
    channel: Channel,
    semaphore: Arc<Semaphore>,
}

impl PooledChannelState {
    fn new(channel: Channel, max_streams: usize) -> Self {
        Self {
            channel,
            semaphore: Arc::new(Semaphore::new(max_streams)),
        }
    }

    fn has_capacity(&self) -> bool {
        self.semaphore.available_permits() > 0
    }
}

struct HostPoolInner {
    channels: Vec<PooledChannelState>,
    next_index: usize,
}

impl HostPoolInner {
    fn next_channel(&mut self) -> &PooledChannelState {
        let idx = self.next_index % self.channels.len();
        self.next_index = idx + 1;
        &self.channels[idx]
    }
}

/// Pool of channels for a single host
struct HostPool {
    inner: Mutex<HostPoolInner>,
    make_channel: MakeChannel,
    max_connections: usize,
    max_concurrency_per_connection: usize,
}

impl HostPool {
    fn new(
        make_channel: MakeChannel,
        min_connections: usize,
        max_connections: usize,
        max_concurrency_per_connection: usize,
    ) -> Self {
        let channel_states: Vec<_> = (0..min_connections)
            .map(|_| PooledChannelState::new(make_channel(), max_concurrency_per_connection))
            .collect();

        Self {
            inner: Mutex::new(HostPoolInner {
                channels: channel_states,
                next_index: 0,
            }),
            make_channel,
            max_connections,
            max_concurrency_per_connection,
        }
    }

    /// Get a pooled channel from this pool.
    ///
    /// This method:
    /// 1. Tries to find a channel with available capacity (round-robin)
    /// 2. If all channels are at capacity and we haven't hit max, creates a new channel
    /// 3. If at max channels, picks the next channel (back-pressure happens in poll_ready)
    async fn get(&self) -> PooledChannel {
        let mut inner = self.inner.lock().await;
        let num_channels = inner.channels.len();

        // Round-robin through existing channels, prefer one with capacity
        for _ in 0..num_channels {
            let state = inner.next_channel();
            if state.has_capacity() {
                return PooledChannel::new(state.channel.clone(), state.semaphore.clone());
            }
        }

        // All channels at capacity - create new if allowed
        if num_channels < self.max_connections {
            let channel = (self.make_channel)();
            let state = PooledChannelState::new(channel, self.max_concurrency_per_connection);
            let pooled = PooledChannel::new(state.channel.clone(), state.semaphore.clone());
            inner.channels.push(state);

            return pooled;
        }

        // At max channels - just return next one, back-pressure happens in poll_ready
        let state = inner.next_channel();
        PooledChannel::new(state.channel.clone(), state.semaphore.clone())
    }
}

/// A pool of gRPC channels by host/address.
///
/// Each address gets its channels from the [`MakeChannel`] that `make_host`
/// returns for it. The address-keyed cache assumes that is the same for every
/// call: if per-call config variation is ever needed, the cache key must
/// include config identity, not just the address.
pub struct ChannelPool {
    pools: Mutex<HashMap<String, Arc<HostPool>>>,
    config: PoolConfig,
    make_host: MakeHost,
}

impl ChannelPool {
    pub fn new(config: PoolConfig, make_host: MakeHost) -> Self {
        Self {
            pools: Mutex::new(HashMap::new()),
            config,
            make_host,
        }
    }

    /// Get a pooled channel for the given address
    pub async fn get(&self, address: &str) -> anyhow::Result<PooledChannel> {
        use std::collections::hash_map::Entry;

        let host_pool = {
            let mut pools = self.pools.lock().await;

            match pools.entry(address.to_owned()) {
                Entry::Occupied(e) => Arc::clone(e.get()),
                Entry::Vacant(e) => {
                    let make_channel = (self.make_host)(address)
                        .with_context(|| format!("Failed to create endpoint for {}", address))?;
                    let host_pool = HostPool::new(
                        make_channel,
                        self.config.min_connections,
                        self.config.max_connections,
                        self.config.max_concurrency_per_connection,
                    );
                    Arc::clone(e.insert(Arc::new(host_pool)))
                }
            }
        };

        Ok(host_pool.get().await)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;

    use super::*;

    fn counting_pool(config: PoolConfig) -> (ChannelPool, Arc<AtomicUsize>) {
        let opened = Arc::new(AtomicUsize::new(0));
        let make_host: MakeHost = Box::new({
            let opened = opened.clone();
            move |address| {
                let endpoint = Channel::from_shared(format!("http://{address}"))?;
                let opened = opened.clone();
                Ok(Arc::new(move || {
                    opened.fetch_add(1, Ordering::SeqCst);
                    endpoint.connect_lazy()
                }))
            }
        });
        (ChannelPool::new(config, make_host), opened)
    }

    #[tokio::test]
    async fn grows_when_connections_are_full() -> anyhow::Result<()> {
        let (pool, opened) = counting_pool(PoolConfig {
            min_connections: 1,
            max_connections: 2,
            max_concurrency_per_connection: 1,
        });

        let mut first = pool.get("127.0.0.1:1").await?;
        // Take the connection's only permit.
        std::future::poll_fn(|cx| {
            let _ready = first.poll_ready(cx);
            Poll::Ready(())
        })
        .await;
        assert_eq!(opened.load(Ordering::SeqCst), 1);

        // The only connection has no permit left, so the pool opens another.
        let _second = pool.get("127.0.0.1:1").await?;
        assert_eq!(opened.load(Ordering::SeqCst), 2);
        Ok(())
    }
}
