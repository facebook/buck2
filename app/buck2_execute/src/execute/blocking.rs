/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::buck2_env;
use buck2_core::fs::project::ProjectRoot;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_util::threads::thread_spawn;
use crossbeam_channel::unbounded;
use dice::DiceComputations;
use dice::UserComputationData;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::future::FutureExt;
use tokio::sync::Semaphore;
use tokio::sync::oneshot;

#[async_trait]
pub trait BlockingExecutor: Allocative + Send + Sync + 'static {
    /// Execute a blocking I/O operation on the current thread. This should be used sparingly. It
    /// is appropriate to use in cases we are doing a minimal amount of I/O (e.g. writing to just
    /// one file), or where I/O is mixed with other blocking operations.  Those operations run with
    /// fairly high concurrency as they aren't expected to contend with each other.
    async fn execute_dyn_io_inline<'a>(
        &self,
        f: Box<dyn FnOnce() -> buck2_error::Result<()> + Send + 'a>,
    ) -> buck2_error::Result<()>;

    /// Execute a blocking I/O operation, possibly on a dedicated I/O pool. This should be used as
    /// the default for I/O. The operations executed here must perform _only_ I/O (since if they do
    /// something else they might contend for I/O threads with actual I/O).
    fn execute_io<'a>(
        &self,
        io: Box<dyn IoRequest>,
        cancellations: &'a CancellationContext,
    ) -> BoxFuture<'a, buck2_error::Result<()>>;

    /// The size of the queue of pending I/O.
    fn queue_size(&self) -> usize;
}

impl dyn BlockingExecutor {
    pub async fn execute_io_inline<F, T>(&self, f: F) -> buck2_error::Result<T>
    where
        F: FnOnce() -> buck2_error::Result<T> + Send,
        T: Send,
    {
        let mut res = None;
        self.execute_dyn_io_inline(Box::new(|| {
            res = Some(f()?);
            Ok(())
        }))
        .await
        .tag(buck2_error::ErrorTag::IoBlockingExecutor)?;
        res.ok_or_else(|| internal_error!("Inline I/O did not execute"))
    }
}

pub trait IoRequest: Send + Sync + 'static {
    fn execute(self: Box<Self>, project_fs: &ProjectRoot) -> buck2_error::Result<()>;
}

struct ThreadPoolIoRequest {
    io: Box<dyn IoRequest>,
    sender: oneshot::Sender<buck2_error::Result<()>>,
}

#[derive(Allocative)]
pub struct BuckBlockingExecutor {
    #[allocative(skip)]
    io_data_semaphore: Semaphore,
    #[allocative(skip)]
    command_sender: crossbeam_channel::Sender<ThreadPoolIoRequest>,
}

impl BuckBlockingExecutor {
    /// We choose the default concurrency as follows:
    ///
    /// - For operations executed by the thread pool, we choose a fairly low concurrency level.
    ///   This is because those operations do exclusively I/O work, and that work consists of
    ///   modifying the directory structure of the FS, which scales negatively as soon as you add
    ///   more than 4 threads on all systems we care about (sometimes it does so earlier, but for now
    ///   4 is the one-size-fits-all solution we have). D33922298 has benchmark details.
    ///
    /// - For operations that primarily write data, we default to the number of threads on the
    ///   host. This is because those operations often have to do CPU bound work to generate the data
    ///   they are trying to write, and writing to multiple files doesn't have the negative scaling
    ///   issues modifying the directory structure does.
    pub fn default_concurrency(fs: ProjectRoot) -> buck2_error::Result<Self> {
        let io_threads = buck2_env!("BUCK2_IO_THREADS", type=usize, default=4)?;
        let io_semaphore = buck2_env!("BUCK2_IO_SEMAPHORE", type=usize, default=buck2_util::threads::available_parallelism())?;

        let (command_sender, command_receiver) = unbounded();

        for i in 0..io_threads {
            let command_receiver = command_receiver.clone();
            let fs = fs.dupe();
            thread_spawn(&format!("buck-io-{i}"), move || {
                for ThreadPoolIoRequest { sender, io } in command_receiver.iter() {
                    let res = io.execute(&fs);
                    let _ignored = sender.send(res);
                }
            })
            .buck_error_context("Failed to spawn io worker")?;
        }

        Ok(Self {
            io_data_semaphore: Semaphore::new(io_semaphore),
            command_sender,
        })
    }
}

#[async_trait]
impl BlockingExecutor for BuckBlockingExecutor {
    async fn execute_dyn_io_inline<'a>(
        &self,
        f: Box<dyn FnOnce() -> buck2_error::Result<()> + Send + 'a>,
    ) -> buck2_error::Result<()> {
        let _permit = self
            .io_data_semaphore
            .acquire()
            .await
            .expect("This semaphore is never closed");

        tokio::task::block_in_place(f)
    }

    fn execute_io<'a>(
        &self,
        io: Box<dyn IoRequest>,
        cancellations: &'a CancellationContext,
    ) -> BoxFuture<'a, buck2_error::Result<()>> {
        let (sender, receiver) = oneshot::channel();

        // Ignore errors sending as they'll translate to an error receiving once we drop the
        // sender.
        let _ignored = self.command_sender.send(ThreadPoolIoRequest { io, sender });

        cancellations
            .critical_section(
                || async move { receiver.await.buck_error_context("Pool shut down")? },
            )
            .boxed()
    }

    fn queue_size(&self) -> usize {
        self.command_sender.len()
    }
}

/// Executor that bypasses the queue and executes IO directly using Tokio's
/// blocking thread pool.

#[derive(Allocative)]
pub struct DirectIoExecutor {
    #[allocative(skip)]
    project_fs: ProjectRoot,
}

impl DirectIoExecutor {
    pub fn new(project_fs: ProjectRoot) -> buck2_error::Result<Self> {
        Ok(Self { project_fs })
    }
}

#[async_trait]
impl BlockingExecutor for DirectIoExecutor {
    async fn execute_dyn_io_inline<'a>(
        &self,
        f: Box<dyn FnOnce() -> buck2_error::Result<()> + Send + 'a>,
    ) -> buck2_error::Result<()> {
        tokio::task::block_in_place(f)
    }

    fn execute_io<'a>(
        &self,
        io: Box<dyn IoRequest>,
        cancellations: &'a CancellationContext,
    ) -> BoxFuture<'a, buck2_error::Result<()>> {
        let project_fs = self.project_fs.dupe();

        cancellations
            .critical_section(|| async move {
                // Execute IO operation in Tokio's blocking thread pool
                tokio::task::spawn_blocking(move || io.execute(&project_fs))
                    .await
                    .buck_error_context("Direct IO spawn_blocking failed")?
            })
            .boxed()
    }

    fn queue_size(&self) -> usize {
        // This executor does not maintain its own queue. We are logging Tokio
        // IO thread metrics separately.
        0
    }
}

pub trait SetBlockingExecutor {
    fn set_blocking_executor(&mut self, exec: Arc<dyn BlockingExecutor>);
}

pub trait HasBlockingExecutor {
    fn get_blocking_executor(&self) -> Arc<dyn BlockingExecutor>;
}

impl SetBlockingExecutor for UserComputationData {
    fn set_blocking_executor(&mut self, exec: Arc<dyn BlockingExecutor>) {
        self.data.set(exec);
    }
}

impl HasBlockingExecutor for DiceComputations<'_> {
    fn get_blocking_executor(&self) -> Arc<dyn BlockingExecutor> {
        self.per_transaction_data()
            .data
            .get::<Arc<dyn BlockingExecutor>>()
            .expect("BlockingExecutor should be set")
            .dupe()
    }
}

pub mod testing {
    use super::*;

    #[derive(Allocative)]
    pub struct DummyBlockingExecutor {
        pub fs: ProjectRoot,
    }

    #[async_trait]
    impl BlockingExecutor for DummyBlockingExecutor {
        async fn execute_dyn_io_inline<'a>(
            &self,
            f: Box<dyn FnOnce() -> buck2_error::Result<()> + Send + 'a>,
        ) -> buck2_error::Result<()> {
            f()
        }

        fn execute_io<'a>(
            &self,
            io: Box<dyn IoRequest>,
            _cancellations: &'a CancellationContext,
        ) -> BoxFuture<'a, buck2_error::Result<()>> {
            futures::future::ready(io.execute(&self.fs)).boxed()
        }

        fn queue_size(&self) -> usize {
            0
        }
    }
}
