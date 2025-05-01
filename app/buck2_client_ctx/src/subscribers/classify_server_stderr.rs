/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::ControlFlow;

use buck2_data::error::ErrorTag;
use once_cell::sync::Lazy;

pub(crate) fn classify_server_stderr(
    error: buck2_error::Error,
    stderr: &str,
) -> buck2_error::Error {
    let tag = if stderr.is_empty() {
        ErrorTag::ServerStderrEmpty
    } else if stderr.contains("<jemalloc>: size mismatch detected") {
        // P1181704561
        ErrorTag::ServerJemallocAssert
    } else if stderr.contains("panicked at") {
        // Sample output of `buck2 debug crash`: P1159041719
        ErrorTag::ServerPanicked
    } else if stderr.contains("has overflowed its stack") {
        // Stderr looks like this:
        // ```
        // thread 'buck2-dm' has overflowed its stack
        // ```
        ErrorTag::ServerStackOverflow
    } else if stderr.contains("Signal 11 (SIGSEGV)") {
        // P1180289404
        ErrorTag::ServerSegv
    } else if stderr.contains("Signal 15 (SIGTERM)") {
        ErrorTag::ServerSigterm
    } else if stderr.contains("Signal 6 (SIGABRT)") {
        ErrorTag::ServerSigabrt
    } else if stderr.contains("(SIGBUS)") {
        // Signal 7 or Signal 10 depending on OS
        ErrorTag::ServerSigbus
    } else {
        ErrorTag::ServerStderrUnknown
    };
    let mut tags = vec![tag];

    let error = if let Some(trace) = extract_trace(stderr) {
        if tag != ErrorTag::ServerSigterm {
            if trace
                .stack_trace_lines
                .iter()
                .any(|line| line.contains("remote_execution"))
            {
                tags.push(ErrorTag::ReClientCrash);
            }

            error.string_tag(&format!("crash({})", trace.trace_key()))
        } else if let Some(signal_line) = trace.signal_line {
            // Keep this because the PID that (might have) sent it could be useful.
            // *** Signal 15 (SIGTERM) (0x2b08100000ab5) received by PID 1762297 (pthread TID 0x7f6650339640) (linux TID 1762297) (maybe from PID 2741, UID 176257) (code: 0), stack trace: ***
            error.context(signal_line)
        } else {
            error
        }
    } else {
        error
    };

    error.tag(tags)
}

//    0: rust_begin_unwind
//       at ./xplat/rust/toolchain/sysroot/1.80.1/library/std/src/panicking.rs:652:5
//    1: <buck2_server::daemon::server::BuckdServer as buck2_cli_proto::daemon_api_server::DaemonApi>::unstable_crash::{closure#0}
//       at ./fbcode/buck2/app/buck2_server/src/daemon/crash.rs:18:13
static RUST_STACK_FRAME: Lazy<regex::Regex> =
    Lazy::new(|| regex::Regex::new(r"^\s*\d*:\s*(.*)$").unwrap());
static RUST_CONTEXT: Lazy<regex::Regex> = Lazy::new(|| regex::Regex::new(r"^\s*at \S*$").unwrap());

fn extract_rust_frame(line: &str) -> ControlFlow<(), Option<String>> {
    if let Some(capture) = RUST_STACK_FRAME
        .captures(line)
        .map(|captures| captures.get(1))
        .flatten()
    {
        ControlFlow::Continue(Some(capture.as_str().to_owned()))
    } else if RUST_CONTEXT.is_match(line) {
        ControlFlow::Continue(None)
    } else {
        ControlFlow::Break(())
    }
}

// 0   buck2                0x0000000107f548c0 _ZN5folly10symbolizer17getStackTraceSafeEPmm + 12
static FOLLY_MAC_STACK_FRAME: Lazy<regex::Regex> =
    Lazy::new(|| regex::Regex::new(r"^\d*\s*\S*\s*0x\w*\s*(\S*) \+ \d*$").unwrap());

// @ 000000000004455f (unknown)
//                    /home/engshare/third-party2/glibc/2.34/src/glibc-2.34/signal/../sysdeps/unix/sysv/linux/libc_sigaction.c:8
//                    -> /home/engshare/third-party2/glibc/2.34/src/glibc-2.34/signal/../sysdeps/unix/sysv/linux/x86_64/libc_sigaction.c
static FOLLY_LINUX_STACK_FRAME: Lazy<regex::Regex> =
    Lazy::new(|| regex::Regex::new(r"^\s*@\s*\w*\s*(.*)$").unwrap());
static FOLLY_LINUX_CONTEXT: Lazy<regex::Regex> =
    Lazy::new(|| regex::Regex::new(r"^\s*(-> )?.*$").unwrap());

fn extract_folly_frame(line: &str) -> ControlFlow<(), Option<String>> {
    if let Some(capture) = FOLLY_MAC_STACK_FRAME
        .captures(line)
        .map(|captures| captures.get(1))
        .flatten()
    {
        ControlFlow::Continue(Some(capture.as_str().to_owned()))
    } else if let Some(capture) = FOLLY_LINUX_STACK_FRAME
        .captures(line)
        .map(|captures| captures.get(1))
        .flatten()
    {
        ControlFlow::Continue(Some(capture.as_str().to_owned()))
    } else if FOLLY_LINUX_CONTEXT.is_match(line) {
        ControlFlow::Continue(None)
    } else {
        ControlFlow::Break(())
    }
}

enum TraceType {
    Rust,
    Folly,
}

const UNINTERESTING_SEGMENTS: [&str; 7] = [
    "rust_begin_unwind",
    "core::panicking::panic_fmt",
    "(unknown)",
    "folly::symbolizer",
    "folly10symbolizer",
    "__GI_",
    "_sigtramp",
];

struct StackTraceInfo {
    signal_line: Option<String>,
    stack_trace_lines: Vec<String>,
}

impl StackTraceInfo {
    fn sanitized_trace(self) -> Vec<String> {
        // Exclude some lines to reduce churn in trace keys.
        // Changes higher up the stack are less likely to be related to the crash.
        if self.stack_trace_lines.len() > 5 {
            // 'uninteresting' lines shouldn't cause churn but exclude them if we are shortening the stack
            // to get enough unique data.

            self.stack_trace_lines
                .into_iter()
                .filter(|line| !UNINTERESTING_SEGMENTS.iter().any(|s| line.contains(s)))
                .take(5)
                .collect()
        } else {
            self.stack_trace_lines.clone()
        }
    }

    fn trace_key(self) -> String {
        // blake3 just because the default rust hasher isn't intended to be stable.
        let mut hasher = blake3::Hasher::new();
        for s in self.sanitized_trace() {
            hasher.update(s.as_bytes());
        }
        let mut digest = hasher.finalize().to_string();
        // Truncate to keep category_key relatively short and readable.
        // This should be enough to avoid collisions since there should be very few unique crashes.
        digest.truncate(6);
        digest
    }
}

fn extract_trace(stderr: &str) -> Option<StackTraceInfo> {
    let mut stack_trace_lines: Vec<String> = vec![];
    let mut signal_line = None;
    let mut stack_trace_type = None;
    for line in stderr.split('\n') {
        if line.starts_with("*** Signal") {
            signal_line = Some(line.to_owned());
            stack_trace_type = Some(TraceType::Folly);
        } else if line.starts_with("stack backtrace:") {
            stack_trace_type = Some(TraceType::Rust);
        } else if let Some(ref trace_type) = stack_trace_type {
            let res = match trace_type {
                TraceType::Rust => extract_rust_frame(line),
                TraceType::Folly => extract_folly_frame(line),
            };
            match res {
                ControlFlow::Continue(Some(line)) => stack_trace_lines.push(line),
                ControlFlow::Continue(None) => (),
                ControlFlow::Break(_) => break,
            }
        }
    }

    stack_trace_type.map(|_| StackTraceInfo {
        signal_line,
        stack_trace_lines,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_trace_lines(trace: &str) -> Vec<String> {
        trace
            .split('\n')
            .map(|s| s.trim().to_owned())
            .filter(|s| !s.is_empty())
            .collect()
    }

    #[test]
    fn test_generated_stack_trace() {
        let backtrace = std::backtrace::Backtrace::force_capture();
        let stderr = format!("stack backtrace:\n{}", backtrace);
        assert!(extract_trace(&stderr).is_some());
    }

    #[test]
    fn test_rust_stack_trace_hash() {
        // from `buck2 debug crash panic`
        let panic_trace = "
stack backtrace:
   0: rust_begin_unwind
             at ./xplat/rust/toolchain/sysroot/1.80.1/library/std/src/panicking.rs:652:5
   1: core::panicking::panic_fmt
             at ./xplat/rust/toolchain/sysroot/1.80.1/library/core/src/panicking.rs:72:14
   2: <buck2_server::daemon::server::BuckdServer as buck2_cli_proto::daemon_api_server::DaemonApi>::unstable_crash::{closure#0}
             at ./fbcode/buck2/app/buck2_server/src/daemon/crash.rs:18:13
   3: <<buck2_cli_proto::daemon_api_server::DaemonApiServer<_> as tower_service::Service<http::request::Request<_>>>::call::Unstable_CrashSvc<buck2_server::daemon::server::BuckdServer> as tonic::server::service::UnaryService<buck2_cli_proto::UnstableCrashRequest>>::call::{closure#0}
             at ./xplat/rust/toolchain/sysroot/1.80.1/library/core/src/future/future.rs:123:9
   4: <buck2_cli_proto::daemon_api_server::DaemonApiServer<buck2_server::daemon::server::BuckdServer> as tower_service::Service<http::request::Request<hyper::body::body::Body>>>::call::{closure#20}
             at ./xplat/rust/toolchain/sysroot/1.80.1/library/core/src/future/future.rs:123:9
   5: <futures_util::future::future::map::Map<futures_util::future::try_future::into_future::IntoFuture<core::pin::Pin<alloc::boxed::Box<dyn core::future::future::Future<Output = core::result::Result<http::response::Response<http_body::combinators::box_body::UnsyncBoxBody<bytes::bytes::Bytes, tonic::status::Status>>, core::convert::Infallible>> + core::marker::Send>>>, futures_util::fns::MapOkFn<<tonic::transport::service::router::Routes>::add_service<buck2_test_proto::test_executor_server::TestExecutorServer<buck2_test_api::grpc::executor::Service<buck2_test_runner::executor::Buck2TestExecutor>>>::{closure#0}>> as core::future::future::Future>::poll
             at ./xplat/rust/toolchain/sysroot/1.80.1/library/core/src/future/future.rs:123:9
   6: <futures_util::future::future::map::Map<futures_util::future::try_future::into_future::IntoFuture<tower::util::map_response::MapResponseFuture<core::pin::Pin<alloc::boxed::Box<dyn core::future::future::Future<Output = core::result::Result<http::response::Response<http_body::combinators::box_body::UnsyncBoxBody<bytes::bytes::Bytes, tonic::status::Status>>, core::convert::Infallible>> + core::marker::Send>>, <tonic::transport::service::router::Routes>::add_service<buck2_forkserver_proto::forkserver_server::ForkserverServer<buck2_forkserver::unix::service::UnixForkserverService>>::{closure#0}>>, futures_util::fns::MapOkFn<<http::response::Response<http_body::combinators::box_body::UnsyncBoxBody<bytes::bytes::Bytes, axum_core::error::Error>> as axum_core::response::into_response::IntoResponse>::into_response>> as core::future::future::Future>::poll
             at ./third-party/rust/vendor/futures-util-0.3.30/src/lib.rs:91:13
   7: <tower::util::map_response::MapResponseFuture<tower::util::map_response::MapResponseFuture<core::pin::Pin<alloc::boxed::Box<dyn core::future::future::Future<Output = core::result::Result<http::response::Response<http_body::combinators::box_body::UnsyncBoxBody<bytes::bytes::Bytes, tonic::status::Status>>, core::convert::Infallible>> + core::marker::Send>>, <tonic::transport::service::router::Routes>::add_service<buck2_cli_proto::daemon_api_server::DaemonApiServer<buck2_server::daemon::server::BuckdServer>>::{closure#0}>, <http::response::Response<http_body::combinators::box_body::UnsyncBoxBody<bytes::bytes::Bytes, axum_core::error::Error>> as axum_core::response::into_response::IntoResponse>::into_response> as core::future::future::Future>::poll
             at ./third-party/rust/vendor/futures-util-0.3.30/src/lib.rs:91:13
   8: <tower::util::oneshot::Oneshot<tower::util::boxed_clone::BoxCloneService<http::request::Request<hyper::body::body::Body>, http::response::Response<http_body::combinators::box_body::UnsyncBoxBody<bytes::bytes::Bytes, axum_core::error::Error>>, core::convert::Infallible>, http::request::Request<hyper::body::body::Body>> as core::future::future::Future>::poll
             at ./xplat/rust/toolchain/sysroot/1.80.1/library/core/src/future/future.rs:123:9
             ";

        let panic_sanitized_trace = test_trace_lines("
        <buck2_server::daemon::server::BuckdServer as buck2_cli_proto::daemon_api_server::DaemonApi>::unstable_crash::{closure#0}
        <<buck2_cli_proto::daemon_api_server::DaemonApiServer<_> as tower_service::Service<http::request::Request<_>>>::call::Unstable_CrashSvc<buck2_server::daemon::server::BuckdServer> as tonic::server::service::UnaryService<buck2_cli_proto::UnstableCrashRequest>>::call::{closure#0}
        <buck2_cli_proto::daemon_api_server::DaemonApiServer<buck2_server::daemon::server::BuckdServer> as tower_service::Service<http::request::Request<hyper::body::body::Body>>>::call::{closure#20}
        <futures_util::future::future::map::Map<futures_util::future::try_future::into_future::IntoFuture<core::pin::Pin<alloc::boxed::Box<dyn core::future::future::Future<Output = core::result::Result<http::response::Response<http_body::combinators::box_body::UnsyncBoxBody<bytes::bytes::Bytes, tonic::status::Status>>, core::convert::Infallible>> + core::marker::Send>>>, futures_util::fns::MapOkFn<<tonic::transport::service::router::Routes>::add_service<buck2_test_proto::test_executor_server::TestExecutorServer<buck2_test_api::grpc::executor::Service<buck2_test_runner::executor::Buck2TestExecutor>>>::{closure#0}>> as core::future::future::Future>::poll
        <futures_util::future::future::map::Map<futures_util::future::try_future::into_future::IntoFuture<tower::util::map_response::MapResponseFuture<core::pin::Pin<alloc::boxed::Box<dyn core::future::future::Future<Output = core::result::Result<http::response::Response<http_body::combinators::box_body::UnsyncBoxBody<bytes::bytes::Bytes, tonic::status::Status>>, core::convert::Infallible>> + core::marker::Send>>, <tonic::transport::service::router::Routes>::add_service<buck2_forkserver_proto::forkserver_server::ForkserverServer<buck2_forkserver::unix::service::UnixForkserverService>>::{closure#0}>>, futures_util::fns::MapOkFn<<http::response::Response<http_body::combinators::box_body::UnsyncBoxBody<bytes::bytes::Bytes, axum_core::error::Error>> as axum_core::response::into_response::IntoResponse>::into_response>> as core::future::future::Future>::poll
        ");
        assert_eq!(
            extract_trace(panic_trace)
                .expect("no trace found")
                .sanitized_trace(),
            panic_sanitized_trace
        );
    }

    #[test]
    fn test_folly_stack_trace() {
        // from `buck2 debug crash abort`
        let linux_folly_trace = "
*** Aborted at 1724968759 (Unix time, try 'date -d @1724968759') ***
*** Signal 6 (SIGABRT) (0x261c500150b2c) received by PID 1379116 (pthread TID 0x7f211b600640) (linux TID 1379195) (maybe from PID 1379116, UID 156101) (code: -6), stack trace: ***
    @ 00000000081eca3f folly::symbolizer::(anonymous namespace)::signalHandler(int, siginfo_t*, void*)
                       ./fbcode/folly/debugging/symbolizer/SignalHandler.cpp:453
    @ 000000000004455f (unknown)
                       /home/engshare/third-party2/glibc/2.34/src/glibc-2.34/signal/../sysdeps/unix/sysv/linux/libc_sigaction.c:8
                       -> /home/engshare/third-party2/glibc/2.34/src/glibc-2.34/signal/../sysdeps/unix/sysv/linux/x86_64/libc_sigaction.c
    @ 000000000009c993 __GI___pthread_kill
                       /home/engshare/third-party2/glibc/2.34/src/glibc-2.34/nptl/pthread_kill.c:46
    @ 00000000000444ac __GI_raise
                       /home/engshare/third-party2/glibc/2.34/src/glibc-2.34/signal/../sysdeps/posix/raise.c:26
    @ 000000000002c432 __GI_abort
                       /home/engshare/third-party2/glibc/2.34/src/glibc-2.34/stdlib/abort.c:79
    @ 0000000007f217e8 std::sys::pal::unix::abort_internal
                       xplat/rust/toolchain/sysroot/1.80.1/library/std/src/sys/pal/unix/mod.rs:366
    @ 0000000007edad48 std::process::abort
                       xplat/rust/toolchain/sysroot/1.80.1/library/std/src/process.rs:2369
    @ 000000001875604b <buck2_server::daemon::server::BuckdServer as buck2_cli_proto::daemon_api_server::DaemonApi>::unstable_crash::{closure#0}
                       fbcode/buck2/app/buck2_server/src/daemon/crash.rs:27
    @ 0000000018e12834 <<buck2_cli_proto::daemon_api_server::DaemonApiServer<_> as tower_service::Service<http::request::Request<_>>>::call::Unstable_CrashSvc<buck2_server::daemon::server::BuckdServer> as tonic::server::service::UnaryService<buck2_cli_proto::UnstableCrashRequest>>::call::{closure#0}
                       xplat/rust/toolchain/sysroot/1.80.1/library/core/src/future/future.rs:123
    @ 0000000018dff24b <buck2_cli_proto::daemon_api_server::DaemonApiServer<buck2_server::daemon::server::BuckdServer> as tower_service::Service<http::request::Request<hyper::body::body::Body>>>::call::{closure#20}
                       xplat/rust/toolchain/sysroot/1.80.1/library/core/src/future/future.rs:123
    @ 0000000019a82766 <futures_util::future::future::map::Map<futures_util::future::try_future::into_future::IntoFuture<core::pin::Pin<alloc::boxed::Box<dyn core::future::future::Future<Output = core::result::Result<http::response::Response<http_body::combinators::box_body::UnsyncBoxBody<bytes::bytes::Bytes, tonic::status::Status>>, core::convert::Infallible>> + core::marker::Send>>>, futures_util::fns::MapOkFn<<tonic::transport::service::router::Routes>::add_service<buck2_cli_proto::daemon_api_server::DaemonApiServer<buck2_server::daemon::server::BuckdServer>>::{closure#0}>> as core::future::future::Future>::poll
                       xplat/rust/toolchain/sysroot/1.80.1/library/core/src/future/future.rs:123
    ";

        let linux_sanitized_trace = test_trace_lines("
        std::sys::pal::unix::abort_internal
        std::process::abort
        <buck2_server::daemon::server::BuckdServer as buck2_cli_proto::daemon_api_server::DaemonApi>::unstable_crash::{closure#0}
        <<buck2_cli_proto::daemon_api_server::DaemonApiServer<_> as tower_service::Service<http::request::Request<_>>>::call::Unstable_CrashSvc<buck2_server::daemon::server::BuckdServer> as tonic::server::service::UnaryService<buck2_cli_proto::UnstableCrashRequest>>::call::{closure#0}
        <buck2_cli_proto::daemon_api_server::DaemonApiServer<buck2_server::daemon::server::BuckdServer> as tower_service::Service<http::request::Request<hyper::body::body::Body>>>::call::{closure#20}
        ");
        assert_eq!(
            extract_trace(linux_folly_trace)
                .expect("no trace found")
                .sanitized_trace(),
            linux_sanitized_trace
        );

        let mac_folly_trace = "
*** Aborted at 1709157873 (Unix time, try 'date -d @1709157873') ***
*** Signal 11 (SIGSEGV) (0x0) received by PID 43312 (pthread TID 0x9026bf000) (code: invalid permissions for mapped object), stack trace: ***
0   buck2                               0x0000000108c7b940 _ZN5folly10symbolizer17getStackTraceSafeEPmm + 12
1   buck2                               0x0000000108c6f8b8 _ZN5folly10symbolizer21SafeStackTracePrinter15printStackTraceEb + 72
2   buck2                               0x000000010a55f0f8 _ZN5folly10symbolizer12_GLOBAL__N_113signalHandlerEiP9__siginfoPv + 1536
3   libsystem_platform.dylib            0x00000001810c5a24 _sigtramp + 56
4   buck2                               0x0000000109ff94c4 _ZN8facebook16remote_execution3cas18ManifoldHttpClient14co_materializeENSt3__110shared_ptrINS1_14CASCallContextEEENS4_INS1_20IDigestStatusTrackerEEENS3_6vectorINS3_4pairIN5build5bazel6remote9execution2v26DigestENS4_INS1_11FileWrapperEEEEENS3_9allocatorISJ_EEEENS0_6crypto8HashAlgoE.resume + 648
5   buck2                               0x0000000109ff54e4 _ZZN5folly4coro18collectAllTryRangeINS0_6detail9MoveRangeINSt3__16vectorINS0_16TaskWithExecutorIvEENS4_9allocatorIS7_EEEEEEEENS0_4TaskINS5_INS_3TryINS2_22decay_rvalue_referenceINS2_21lift_lvalue_referenceINS0_12await_resultIDTclL_ZNS0_11folly_cpo__13co_viaIfAsyncEEclL_ZNS4_7declvalINS_8Executor9KeepAliveISJ_EEEEDTcl9__declvalIT_ELi0EEEvEEclsr3stdE7declvalINS4_15iterator_traitsIDTclL_ZNS_6access5beginEEclsr3stdE7declvalIRSM_EEEEE9referenceEEEEEvE4typeEE4typeEE4typeEEENS8_IS11_EEEEEESM_ENKUlS7_mE_clES7_m.resume + 560
6   buck2                               0x0000000108c7b588 _ZN5folly36resumeCoroutineWithNewAsyncStackRootENSt12experimental13coroutines_v116coroutine_handleIvEERNS_15AsyncStackFrameE + 84
7   buck2                               0x0000000108c4edd4 _ZN5folly6detail8function14FunctionTraitsIFvvEE9callSmallIZNS_4coro6detail23ViaCoroutinePromiseBase20scheduleContinuationEvEUlvE_EEvRNS1_4DataE + 48
8   buck2                               0x0000000108caabdc _ZN5folly18ThreadPoolExecutor7runTaskERKNSt3__110shared_ptrINS0_6ThreadEEEONS0_4TaskE + 304
9   buck2                               0x0000000108e129f4 _ZN5folly21CPUThreadPoolExecutor9threadRunENSt3__110shared_ptrINS_18ThreadPoolExecutor6ThreadEEE + 484
(safe mode, symbolizer not available)
    ";

        let mac_sanitized_trace = test_trace_lines("
        _ZN8facebook16remote_execution3cas18ManifoldHttpClient14co_materializeENSt3__110shared_ptrINS1_14CASCallContextEEENS4_INS1_20IDigestStatusTrackerEEENS3_6vectorINS3_4pairIN5build5bazel6remote9execution2v26DigestENS4_INS1_11FileWrapperEEEEENS3_9allocatorISJ_EEEENS0_6crypto8HashAlgoE.resume
        _ZZN5folly4coro18collectAllTryRangeINS0_6detail9MoveRangeINSt3__16vectorINS0_16TaskWithExecutorIvEENS4_9allocatorIS7_EEEEEEEENS0_4TaskINS5_INS_3TryINS2_22decay_rvalue_referenceINS2_21lift_lvalue_referenceINS0_12await_resultIDTclL_ZNS0_11folly_cpo__13co_viaIfAsyncEEclL_ZNS4_7declvalINS_8Executor9KeepAliveISJ_EEEEDTcl9__declvalIT_ELi0EEEvEEclsr3stdE7declvalINS4_15iterator_traitsIDTclL_ZNS_6access5beginEEclsr3stdE7declvalIRSM_EEEEE9referenceEEEEEvE4typeEE4typeEE4typeEEENS8_IS11_EEEEEESM_ENKUlS7_mE_clES7_m.resume
        _ZN5folly36resumeCoroutineWithNewAsyncStackRootENSt12experimental13coroutines_v116coroutine_handleIvEERNS_15AsyncStackFrameE
        _ZN5folly6detail8function14FunctionTraitsIFvvEE9callSmallIZNS_4coro6detail23ViaCoroutinePromiseBase20scheduleContinuationEvEUlvE_EEvRNS1_4DataE
        _ZN5folly18ThreadPoolExecutor7runTaskERKNSt3__110shared_ptrINS0_6ThreadEEEONS0_4TaskE
        ");

        assert_eq!(
            extract_trace(mac_folly_trace)
                .expect("no trace found")
                .sanitized_trace(),
            mac_sanitized_trace
        );
    }
}
