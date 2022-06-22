/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The panic hook, shared by the buck2 CLI and daemon.
//!
//! Buck2 is compiled with -C panic=abort, which causes the default hook to abort as soon as the panic runtime is
//! invoked. This module sets up a panic hook to run just before that to do some crash reporting.

use std::panic;
use std::panic::PanicInfo;

use fbinit::FacebookInit;

/// Initializes the panic hook.
pub fn initialize(fb: FacebookInit) {
    let hook = panic::take_hook();
    panic::set_hook(box move |info| {
        the_panic_hook(fb, info);
        hook(info);
    });
    buck2_core::error::initialize(box move |err, loc| {
        imp::write_soft_error_to_scribe(
            fb,
            err,
            buck2_data::Location {
                file: loc.0.to_owned(),
                line: loc.1,
                column: loc.2,
            },
        );
    });
}

/// The panic hook, initialized during `initialize`. Invoked immediately on a panic, but prior to the process being
/// aborted.
///
/// The panic hook is called on the same thread that the panic occurred. It is possible to perform a backtrace here
/// to collect additional information.
fn the_panic_hook(fb: FacebookInit, info: &PanicInfo) {
    imp::write_panic_to_scribe(fb, info);
}

#[cfg(fbcode_build)]
mod imp {
    use std::collections::HashMap;
    use std::panic::PanicInfo;
    use std::thread;

    use backtrace::Backtrace;
    use buck2_data::Location;
    use fbinit::FacebookInit;
    use tokio::runtime::Builder;

    use crate::metadata;

    fn get_stack() -> Vec<buck2_data::panic::StackFrame> {
        fn ptr_to_string<T>(ptr: *mut T) -> String {
            format!("0x{:x}", ptr as usize)
        }

        let trace = Backtrace::new();
        trace
            .frames()
            .iter()
            .map(|frame| {
                let symbols = frame
                    .symbols()
                    .iter()
                    .map(|symbol| buck2_data::panic::Symbol {
                        name: symbol
                            .name()
                            .map(|s| s.to_string())
                            .unwrap_or_else(|| "".to_string()),
                        address: symbol
                            .addr()
                            .map(ptr_to_string)
                            .unwrap_or_else(|| "".to_string()),
                        file: symbol
                            .filename()
                            .map(|p| p.display().to_string())
                            .unwrap_or_else(|| "".to_string()),
                        line: symbol.lineno().unwrap_or(0),
                        column: symbol.colno().unwrap_or(0),
                    })
                    .collect::<Vec<_>>();

                buck2_data::panic::StackFrame {
                    instruction_pointer: ptr_to_string(frame.ip()),
                    symbol_address: ptr_to_string(frame.symbol_address()),
                    module_base_address: frame
                        .module_base_address()
                        .map(ptr_to_string)
                        .unwrap_or_else(|| "".to_string()),
                    symbols,
                }
            })
            .collect()
    }

    /// Extracts a stringly-formatted payload from the given PanicInfo - usually the argument to `panic!`.
    fn get_message_for_panic(info: &PanicInfo) -> String {
        // Rust panic payloads can be anything, but they generally take one of two forms:
        //  1. &'static str, for panics with a constant string parameter,
        //  2. String, for panics that use the format `{}` syntax to construct a message.
        //
        // Since PanicInfo's Display implementation is implemented in libcore, it can't cover the String case (which is)
        // a liballoc exclusive), so this code here checks for formatted messages and uses that as the message if present.
        if let Some(literal_msg) = info.payload().downcast_ref::<&str>() {
            (*literal_msg).to_owned()
        } else if let Some(format_msg) = info.payload().downcast_ref::<String>() {
            format_msg.clone()
        } else {
            "explicit panic with no message".to_owned()
        }
    }

    /// Collects metadata from the current environment for use in LogView.
    fn get_metadata_for_panic() -> HashMap<String, String> {
        let mut map = metadata::collect();
        if let Ok(commands) = crate::daemon::server::ACTIVE_COMMANDS.lock() {
            let commands = commands.iter().map(|id| id.to_string()).collect::<Vec<_>>();
            map.insert("active_commands".to_owned(), commands.join(","));
        }
        map
    }

    /// Writes a representation of the given `PanicInfo` to Scribe, via the `Panic` event.
    pub(crate) fn write_panic_to_scribe(fb: FacebookInit, info: &PanicInfo) {
        let message = get_message_for_panic(info);
        let location = info.location().map(|loc| Location {
            file: loc.file().to_owned(),
            line: loc.line(),
            column: loc.column(),
        });
        write_to_scribe(fb, location, message);
    }

    pub(crate) fn write_soft_error_to_scribe(
        fb: FacebookInit,
        err: &anyhow::Error,
        location: Location,
    ) {
        write_to_scribe(fb, Some(location), format!("Soft Error: {:#}", err));
    }

    /// Writes a representation of the given error (hard or soft) to Scribe
    fn write_to_scribe(fb: FacebookInit, location: Option<Location>, message: String) {
        use std::time::SystemTime;

        use buck2_core::facebook_only;
        use buck2_data::InstantEvent;
        use events::sink::scribe;
        use events::sink::scribe::ThriftScribeSink;
        use events::BuckEvent;
        use events::EventSink;
        use events::TraceId;

        facebook_only();
        if !scribe::is_enabled() {
            return;
        }

        let sink =
            match ThriftScribeSink::new(fb, "buck2_events".to_owned(), /* buffer size */ 100) {
                Ok(sink) => sink,
                Err(_) => {
                    // We're already panicking and we can't connect to the scribe daemon? Things are bad and we're SOL.
                    return;
                }
            };

        let metadata = get_metadata_for_panic();
        let panic_payload: buck2_data::instant_event::Data = buck2_data::Panic {
            location,
            payload: message,
            metadata,
            backtrace: get_stack(),
        }
        .into();
        let event = BuckEvent {
            timestamp: SystemTime::now(),
            trace_id: TraceId::new(),
            span_id: None,
            parent_id: None,
            data: InstantEvent {
                data: Some(panic_payload),
            }
            .into(),
        };
        let _err = sink.send(event);

        // There are some dubious ways of acquiring a handle to the current Tokio runtime, if one exists, such as this
        // one: https://docs.rs/tokio/latest/tokio/runtime/struct.Handle.html#method.current. However, there doesn't
        // appear to be a good way to figure out if the thread we're running on is a tokio thread at all.
        //
        // Since this is the panic handler, and the panic handler runs in the context of the thread that panicked, we
        // can make no assumptions about whether the thread we're on is a tokio thread. To flush the Scribe client,
        // which is normally an async operation, we spawn a new thread, spawn a runtime on that, and await the flush
        // on that thread.
        //
        // Note that if we fail to spawn a writer thread, then we just won't log.
        let _err = thread::Builder::new()
            .spawn(move || {
                let runtime = Builder::new_current_thread().enable_all().build().unwrap();
                runtime.block_on(sink.flush_blocking());
            })
            .map_err(|_| ())
            .and_then(|t| t.join().map_err(|_| ()));
    }
}

#[cfg(not(fbcode_build))]
mod imp {
    use std::panic::PanicInfo;

    use fbinit::FacebookInit;

    pub(crate) fn write_panic_to_scribe(_: FacebookInit, _: &PanicInfo) {}

    pub(crate) fn write_soft_error_to_scribe(
        _: FacebookInit,
        _: &anyhow::Error,
        _: buck2_data::Location,
    ) {
    }
}
