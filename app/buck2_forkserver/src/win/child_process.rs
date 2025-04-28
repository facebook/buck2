/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::os::windows::io::AsRawHandle;
use std::pin::Pin;
use std::process::Child;
use std::process::ExitStatus;
use std::task::Context;
use std::task::Poll;

use tokio::io;
use tokio::sync::oneshot;
use winapi::um::handleapi;
use winapi::um::threadpoollegacyapiset::UnregisterWaitEx;
use winapi::um::winbase;
use winapi::um::winnt;
use winapi::um::winnt::HANDLE;

pub(crate) struct ChildProcess {
    inner: Child,
    waiting: Option<Waiting>,
}

impl ChildProcess {
    pub(crate) fn new(child: Child) -> Self {
        Self {
            inner: child,
            waiting: None,
        }
    }

    pub(crate) fn as_std(&self) -> &Child {
        &self.inner
    }

    pub(crate) fn as_std_mut(&mut self) -> &mut Child {
        &mut self.inner
    }
}

// This implementation is a copy of tokio internal Future implementation on their Child.
// See https://github.com/tokio-rs/tokio/blob/master/tokio/src/process/windows.rs#L102
impl Future for ChildProcess {
    type Output = io::Result<ExitStatus>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let inner = Pin::get_mut(self);
        loop {
            if let Some(ref mut w) = inner.waiting {
                match Pin::new(&mut w.rx).poll(cx) {
                    Poll::Ready(Ok(())) => {}
                    Poll::Ready(Err(e)) => Err(io::Error::new(io::ErrorKind::Other, e))?,
                    Poll::Pending => return Poll::Pending,
                }
                let status = inner.inner.try_wait()?.ok_or(io::Error::new(
                    io::ErrorKind::Other,
                    "exit status is not available",
                ))?;
                return Poll::Ready(Ok(status));
            }

            if let Some(e) = inner.inner.try_wait()? {
                return Poll::Ready(Ok(e));
            }
            let (tx, rx) = oneshot::channel();
            let ptr = Box::into_raw(Box::new(Some(tx)));
            let mut wait_object = handleapi::INVALID_HANDLE_VALUE;
            let rc = unsafe {
                winbase::RegisterWaitForSingleObject(
                    &mut wait_object,
                    inner.inner.as_raw_handle(),
                    Some(callback),
                    ptr as *mut _,
                    winbase::INFINITE,
                    winnt::WT_EXECUTEINWAITTHREAD | winnt::WT_EXECUTEONLYONCE,
                )
            };
            if rc == 0 {
                let err = io::Error::last_os_error();
                drop(unsafe { Box::from_raw(ptr) });
                return Poll::Ready(Err(err));
            }
            inner.waiting = Some(Waiting {
                rx,
                wait_object,
                tx: ptr,
            });
        }
    }
}

// Waiting for a signal from a `wait_object` handle
struct Waiting {
    rx: oneshot::Receiver<()>,
    wait_object: HANDLE,
    // we're using raw pointer to pass it through ffi to callback
    tx: *mut Option<oneshot::Sender<()>>,
}

unsafe impl Sync for Waiting {}
unsafe impl Send for Waiting {}

impl Drop for Waiting {
    fn drop(&mut self) {
        unsafe {
            let rc = UnregisterWaitEx(self.wait_object, handleapi::INVALID_HANDLE_VALUE);
            if rc == 0 {
                panic!("failed to unregister: {}", io::Error::last_os_error());
            }
            drop(Box::from_raw(self.tx));
        }
    }
}

unsafe extern "system" fn callback(ptr: *mut std::ffi::c_void, _timer_fired: winnt::BOOLEAN) {
    let complete = &mut *(ptr as *mut Option<oneshot::Sender<()>>);
    complete.take().unwrap().send(()).unwrap();
}

#[cfg(test)]
mod tests {
    use buck2_util::process::background_command;

    use crate::win::child_process::ChildProcess;

    #[tokio::test]
    async fn test_child_process() -> buck2_error::Result<()> {
        let mut cmd = background_command("cmd");
        let cmd = cmd.arg("/c").arg("exit 2");

        let child = cmd.spawn().unwrap();
        let proc = ChildProcess::new(child);

        let status = proc.await?;
        assert_eq!(status.code(), Some(2));
        Ok(())
    }
}
