/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::project::ProjectRoot;
use crossbeam_channel::unbounded;
use dice::DiceComputations;
use dice::UserComputationData;
use gazebo::prelude::*;
use more_futures::spawn::dropcancel_critical_section;
use tokio::sync::oneshot;
use tokio::sync::Semaphore;

#[async_trait]
pub trait BlockingExecutor: Send + Sync + 'static {
    /// Execute a blocking I/O operation on the current thread. This should be used sparingly. It
    /// is appropriate to use in cases we are doing a minimal amount of I/O (e.g. writing to just
    /// one file), or where I/O is mixed with other blocking operations.  Those operations run with
    /// fairly high concurrency as they aren't expected to contend with each other.
    async fn execute_dyn_io_inline<'a>(
        &self,
        f: Box<dyn FnOnce() -> anyhow::Result<()> + Send + 'a>,
    ) -> anyhow::Result<()>;

    /// Execute a blocking I/O operation, possibly on a dedicated I/O pool. This should be used as
    /// the default for I/O. The operations executed here must perform _only_ I/O (since if they do
    /// something else they might contend for I/O threads with actual I/O).
    async fn execute_io(&self, io: Box<dyn IoRequest>) -> anyhow::Result<()>;

    /// The size of the queue of pending I/O.
    fn queue_size(&self) -> usize;
}

impl dyn BlockingExecutor {
    pub async fn execute_io_inline<F, T>(&self, f: F) -> anyhow::Result<T>
    where
        F: FnOnce() -> anyhow::Result<T> + Send,
        T: Send,
    {
        let mut res = None;
        self.execute_dyn_io_inline(box || {
            res = Some(f()?);
            Ok(())
        })
        .await?;
        res.context("Inline I/O did not execute")
    }
}

pub trait IoRequest: Send + Sync + 'static {
    fn execute(self: Box<Self>, project_fs: &ProjectRoot) -> anyhow::Result<()>;
}

struct ThreadPoolIoRequest {
    io: Box<dyn IoRequest>,
    sender: oneshot::Sender<anyhow::Result<()>>,
}

pub struct BuckBlockingExecutor {
    io_data_semaphore: Semaphore,
    command_sender: crossbeam_channel::Sender<ThreadPoolIoRequest>,
}

impl BuckBlockingExecutor {
    /// We choose the default concurrency as follows:
    ///
    /// - For operations executed by the thread pool, we choose a fairly low concurrency level.
    /// This is because those operations do exclusively I/O work, and that work consists of
    /// modifying the directory structure of the FS, which scales negatively as soon as you add
    /// more than 4 threads on all systems we care about (sometimes it does so earlier, but for now
    /// 4 is the one-size-fits-all solution we have). D33922298 has benchmark details.
    ///
    /// - For operations that primarily write data, we default to the number of threads on the
    /// host. This is because those operations often have to do CPU bound work to generate the data
    /// they are trying to write, and writing to multiple files doesn't have the negative scaling
    /// issues modifying the directory structure does.
    pub fn default_concurrency(fs: ProjectRoot) -> anyhow::Result<Self> {
        static IO_THREADS: EnvHelper<usize> = EnvHelper::new("BUCK2_IO_THREADS");
        static IO_SEMAPHORE: EnvHelper<usize> = EnvHelper::new("BUCK2_IO_SEMAPHORE");

        let io_threads = IO_THREADS.get_copied()?.unwrap_or(4);
        let io_semaphore = IO_SEMAPHORE.get_copied()?.unwrap_or_else(num_cpus::get);

        let (command_sender, command_receiver) = unbounded();

        for i in 0..io_threads {
            let command_receiver = command_receiver.clone();
            let fs = fs.dupe();
            std::thread::Builder::new()
                .name(format!("buck-io-{}", i))
                .spawn(move || {
                    for ThreadPoolIoRequest { sender, io } in command_receiver.iter() {
                        let res = io.execute(&fs);
                        let _ignored = sender.send(res);
                    }
                })
                .context("Failed to spawn io worker")?;
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
        f: Box<dyn FnOnce() -> anyhow::Result<()> + Send + 'a>,
    ) -> anyhow::Result<()> {
        let _permit = self
            .io_data_semaphore
            .acquire()
            .await
            .expect("This semaphore is never closed");

        tokio::task::block_in_place(f)
    }

    async fn execute_io(&self, io: Box<dyn IoRequest>) -> anyhow::Result<()> {
        dropcancel_critical_section(async move {
            let (sender, receiver) = oneshot::channel();
            self.command_sender
                .send(ThreadPoolIoRequest { io, sender })?;
            receiver.await.context("Pool shut down")?
        })
        .await
    }

    fn queue_size(&self) -> usize {
        self.command_sender.len()
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

impl HasBlockingExecutor for DiceComputations {
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

    pub struct DummyBlockingExecutor {
        pub fs: ProjectRoot,
    }

    #[async_trait]
    impl BlockingExecutor for DummyBlockingExecutor {
        async fn execute_dyn_io_inline<'a>(
            &self,
            f: Box<dyn FnOnce() -> anyhow::Result<()> + Send + 'a>,
        ) -> anyhow::Result<()> {
            f()
        }

        async fn execute_io(&self, io: Box<dyn IoRequest>) -> anyhow::Result<()> {
            io.execute(&self.fs)
        }

        fn queue_size(&self) -> usize {
            0
        }
    }
}
