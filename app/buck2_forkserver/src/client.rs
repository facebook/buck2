/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::io;
use std::process::ExitStatus;
use std::sync::Arc;

use allocative::Allocative;
use arc_swap::ArcSwapOption;
use buck2_core::tag_error;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute_local::CommandResult;
use buck2_execute_local::decode_command_event_stream;
use buck2_resource_control::CommandType;
use buck2_resource_control::action_cgroups::ActionCgroupSession;
use buck2_resource_control::cgroup_info::CGroupInfo;
use buck2_resource_control::memory_tracker::MemoryTrackerHandle;
use buck2_resource_control::pool::CgroupPool;
use dupe::Dupe;
use futures::future;
use futures::future::Future;
use futures::future::FutureExt;
use futures::stream;
use futures::stream::StreamExt;
use tokio::process::Child;
use tonic::Request;
use tonic::transport::Channel;

use crate::convert::decode_event_stream;

#[derive(Clone, Dupe, Allocative)]
pub struct ForkserverClient {
    inner: Arc<ForkserverClientInner>,
    memory_tracker: Option<MemoryTrackerHandle>,
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
enum ForkserverError {
    #[error("Error on Forkserver wait()")]
    WaitError(#[source] io::Error),
    #[error("Forkserver exited {}", _0)]
    Exited(ExitStatus),
}

#[derive(Allocative)]
struct ForkserverClientInner {
    /// Error from the forkserver process, if any.
    #[allocative(skip)]
    error: Arc<ArcSwapOption<buck2_error::Error>>,
    pid: u32,
    #[allocative(skip)]
    rpc: buck2_forkserver_proto::forkserver_client::ForkserverClient<Channel>,
    /// The cgroup info of the forkserver process, if available.
    cgroup_info: Option<CGroupInfoWrapper>,
    #[allocative(skip)]
    cgroup_pool: Option<CgroupPool>,
}

#[derive(Allocative)]
pub struct CGroupInfoWrapper {
    /// The forkserver itself.
    pub scope: CGroupInfo,

    /// The forkserver and the processes it spawned.
    pub slice: CGroupInfo,
}

impl ForkserverClient {
    pub(crate) async fn new(
        mut child: Child,
        channel: Channel,
        memory_tracker: Option<MemoryTrackerHandle>,
        cgroup_pool: Option<CgroupPool>,
    ) -> buck2_error::Result<Self> {
        let rpc = buck2_forkserver_proto::forkserver_client::ForkserverClient::new(channel)
            .max_encoding_message_size(usize::MAX)
            .max_decoding_message_size(usize::MAX);

        let pid = child.id().expect("Child has not been polled");

        let error = Arc::new(ArcSwapOption::empty());

        tokio::task::spawn({
            let error = error.clone();

            async move {
                let err = match child.wait().await {
                    Ok(status) => ForkserverError::Exited(status),
                    Err(e) => ForkserverError::WaitError(e),
                };

                let err = buck2_error::Error::from(err).context("Forkserver is unavailable");
                error.swap(Some(Arc::new(err)));
            }
        });

        // Query the forkserver's Cgroup now that it's started. This might return
        // None if we didn't enable it.
        let response = rpc
            .clone()
            .get_cgroup(tonic::Request::new(
                buck2_forkserver_proto::GetCgroupRequest {},
            ))
            .await
            .buck_error_context("Failed to query forkserver cgroup")?;

        let cgroup_info = response.into_inner().cgroup_path.and_then(|path| {
            let scope = CGroupInfo { path };
            let slice = CGroupInfo {
                path: scope.get_slice()?.to_owned(),
            };
            Some(CGroupInfoWrapper { scope, slice })
        });

        Ok(Self {
            inner: Arc::new(ForkserverClientInner {
                error,
                pid,
                rpc,
                cgroup_info,
                cgroup_pool,
            }),
            memory_tracker,
        })
    }

    pub fn pid(&self) -> u32 {
        self.inner.pid
    }

    pub fn cgroup_info(&self) -> Option<&CGroupInfoWrapper> {
        self.inner.cgroup_info.as_ref()
    }

    pub async fn execute<C>(
        &self,
        mut req: buck2_forkserver_proto::CommandRequest,
        cancel: C,
        command_type: CommandType,
        dispatcher: EventDispatcher,
    ) -> buck2_error::Result<CommandResult>
    where
        C: Future<Output = ()> + Send + 'static,
    {
        if let Some(err) = &*self.inner.error.load() {
            return Err(tag_error!(
                "forkserver_exit",
                err.as_ref().dupe().into(),
                quiet: true,
                task: false,
                daemon_in_memory_state_is_corrupted: true,
            )
            .into());
        }

        let cgroup_state = if let Some(cgroup_pool) = &self.inner.cgroup_pool {
            let (cgroup_id, cgroup_path) = cgroup_pool.acquire()?;
            req.command_cgroup = Some(cgroup_path.to_str()?.to_owned());
            Some((cgroup_id, cgroup_pool))
        } else {
            None
        };

        let res = self
            .execute_with_cgroup(req, cancel, command_type, dispatcher)
            .await;

        if let Some((id, pool)) = cgroup_state {
            pool.release(id);
        }

        res
    }

    async fn execute_with_cgroup<C>(
        &self,
        req: buck2_forkserver_proto::CommandRequest,
        cancel: C,
        command_type: CommandType,
        dispatcher: EventDispatcher,
    ) -> buck2_error::Result<CommandResult>
    where
        C: Future<Output = ()> + Send + 'static,
    {
        let action_digest = req.action_digest.clone();
        let stream = stream::once(future::ready(buck2_forkserver_proto::RequestEvent {
            data: Some(req.into()),
        }))
        .chain(stream::once(cancel.map(|()| {
            buck2_forkserver_proto::RequestEvent {
                data: Some(buck2_forkserver_proto::CancelRequest {}.into()),
            }
        })));

        let stream = self
            .inner
            .rpc
            .clone()
            .run(stream)
            .await
            .buck_error_context("Error dispatching command to Forkserver")?
            .into_inner();
        let stream = decode_event_stream(stream);

        let cgroup_session = ActionCgroupSession::maybe_create(
            &self.memory_tracker,
            dispatcher,
            command_type,
            action_digest,
        );
        decode_command_event_stream(stream, cgroup_session).await
    }

    pub async fn set_log_filter(&self, log_filter: String) -> buck2_error::Result<()> {
        self.inner
            .rpc
            .clone()
            .set_log_filter(Request::new(buck2_forkserver_proto::SetLogFilterRequest {
                log_filter,
            }))
            .await?;

        Ok(())
    }
}
