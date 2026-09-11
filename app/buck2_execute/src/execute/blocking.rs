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
use buck2_error::BuckErrorOptionContext;
use buck2_events::dispatch::current_span;
use buck2_events::dispatch::maybe_proxy_current_span;
use buck2_events::span::SpanId;
use buck2_util::threads::directory_mutation_parallelism;
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
        res.internal_error("Inline I/O did not execute")
    }
}

pub trait IoRequest: Send + Sync + 'static {
    fn execute(self: Box<Self>, project_fs: &ProjectRoot) -> buck2_error::Result<()>;
}

struct ThreadPoolIoRequest {
    project_fs: ProjectRoot,
    io: Box<dyn IoRequest>,
    parent_id: Option<SpanId>,
    sender: oneshot::Sender<buck2_error::Result<()>>,
}

#[derive(Allocative)]
struct BuckBlockingExecutorShared {
    #[allocative(skip)]
    io_data_semaphore: Semaphore,
    #[allocative(skip)]
    command_sender: crossbeam_channel::Sender<ThreadPoolIoRequest>,
}

impl BuckBlockingExecutorShared {
    /// We choose the default concurrency as follows:
    ///
    /// - For operations executed by the thread pool, we use `directory_mutation_parallelism()`:
    ///   those operations do exclusively I/O work that modifies the directory structure of
    ///   the FS, which scales negatively past a small number of concurrent workers.
    ///
    /// - For operations that primarily write data, we default to the number of threads on the
    ///   host. This is because those operations often have to do CPU bound work to generate the data
    ///   they are trying to write, and writing to multiple files doesn't have the negative scaling
    ///   issues modifying the directory structure does.
    fn default_concurrency() -> buck2_error::Result<Self> {
        let io_threads =
            buck2_env!("BUCK2_IO_THREADS", type=usize, default=directory_mutation_parallelism())?;
        let io_semaphore = buck2_env!("BUCK2_IO_SEMAPHORE", type=usize, default=buck2_util::threads::available_parallelism())?;

        let (command_sender, command_receiver) = unbounded();

        for i in 0..io_threads {
            let command_receiver = command_receiver.clone();
            thread_spawn(&format!("buck-io-{i}"), move || {
                for ThreadPoolIoRequest {
                    project_fs,
                    sender,
                    parent_id,
                    io,
                } in command_receiver.iter()
                {
                    let res = maybe_proxy_current_span(parent_id, || io.execute(&project_fs));
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

#[derive(Allocative)]
struct BuckBlockingExecutor {
    shared: Arc<BuckBlockingExecutorShared>,
    #[allocative(skip)]
    project_fs: ProjectRoot,
}

#[async_trait]
impl BlockingExecutor for BuckBlockingExecutor {
    async fn execute_dyn_io_inline<'a>(
        &self,
        f: Box<dyn FnOnce() -> buck2_error::Result<()> + Send + 'a>,
    ) -> buck2_error::Result<()> {
        let _permit = self
            .shared
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
        let _ignored = self.shared.command_sender.send(ThreadPoolIoRequest {
            project_fs: self.project_fs.dupe(),
            io,
            parent_id: current_span(),
            sender,
        });

        cancellations
            .critical_section(
                || async move { receiver.await.buck_error_context("Pool shut down")? },
            )
            .boxed()
    }

    fn queue_size(&self) -> usize {
        self.shared.command_sender.len()
    }
}

/// Executor that bypasses the queue and executes IO directly using Tokio's
/// blocking thread pool.

#[derive(Allocative)]
struct DirectIoExecutor {
    #[allocative(skip)]
    project_fs: ProjectRoot,
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

#[derive(Allocative)]
enum BlockingExecutorFactoryKind {
    Pooled(Arc<BuckBlockingExecutorShared>),
    Direct,
}

/// Owns the daemon-wide scheduling resources used to create repo-bound blocking executors.
#[derive(Allocative)]
pub struct BlockingExecutorFactory {
    kind: BlockingExecutorFactoryKind,
}

impl BlockingExecutorFactory {
    pub fn create() -> buck2_error::Result<Self> {
        let kind = if cfg!(any(target_os = "macos", target_os = "windows")) {
            BlockingExecutorFactoryKind::Direct
        } else {
            BlockingExecutorFactoryKind::Pooled(Arc::new(
                BuckBlockingExecutorShared::default_concurrency()?,
            ))
        };

        Ok(Self { kind })
    }

    pub fn for_project(&self, project_fs: ProjectRoot) -> Arc<dyn BlockingExecutor> {
        match &self.kind {
            BlockingExecutorFactoryKind::Pooled(shared) => Arc::new(BuckBlockingExecutor {
                shared: shared.dupe(),
                project_fs,
            }),
            BlockingExecutorFactoryKind::Direct => Arc::new(DirectIoExecutor { project_fs }),
        }
    }

    pub fn queue_size(&self) -> usize {
        match &self.kind {
            BlockingExecutorFactoryKind::Pooled(shared) => shared.command_sender.len(),
            BlockingExecutorFactoryKind::Direct => 0,
        }
    }
}

pub trait SetBlockingExecutor {
    fn set_blocking_executor(&mut self, exec: Arc<dyn BlockingExecutor>);
}

pub trait HasBlockingExecutor<'d> {
    fn get_blocking_executor(&self) -> &'d dyn BlockingExecutor;
}

impl SetBlockingExecutor for UserComputationData {
    fn set_blocking_executor(&mut self, exec: Arc<dyn BlockingExecutor>) {
        self.data.set(exec);
    }
}

impl<'d> HasBlockingExecutor<'d> for DiceComputations<'d> {
    fn get_blocking_executor(&self) -> &'d dyn BlockingExecutor {
        &**self
            .per_transaction_data()
            .data
            .get::<Arc<dyn BlockingExecutor>>()
            .expect("BlockingExecutor should be set")
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

#[cfg(test)]
mod tests {
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_fs::error::IoResultExt;
    use buck2_fs::fs_util;

    use super::*;

    struct WriteMarker(&'static str);

    impl IoRequest for WriteMarker {
        fn execute(self: Box<Self>, project_fs: &ProjectRoot) -> buck2_error::Result<()> {
            project_fs.write_file(ProjectRelativePath::new("marker")?, self.0, false)
        }
    }

    #[tokio::test]
    async fn factory_routes_io_to_each_project_root() -> buck2_error::Result<()> {
        let first_root = ProjectRootTemp::new()?;
        let second_root = ProjectRootTemp::new()?;
        let factory = BlockingExecutorFactory::create()?;
        let first_executor = factory.for_project(first_root.path().dupe());
        let second_executor = factory.for_project(second_root.path().dupe());

        first_executor
            .execute_io(
                Box::new(WriteMarker("first")),
                CancellationContext::never_cancelled(),
            )
            .await?;
        second_executor
            .execute_io(
                Box::new(WriteMarker("second")),
                CancellationContext::never_cancelled(),
            )
            .await?;

        let marker = ProjectRelativePath::new("marker")?;
        assert_eq!(
            fs_util::read_to_string(first_root.path().resolve(marker)).categorize_internal()?,
            "first"
        );
        assert_eq!(
            fs_util::read_to_string(second_root.path().resolve(marker)).categorize_internal()?,
            "second"
        );

        Ok(())
    }
}
