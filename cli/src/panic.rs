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
//! The Buck2 CLI unwinds the stack on a panic, while the Buck2 daemon terminates without unwinding.
//! This module sets up a shared panic hook to run before unwinding/termination for crash reporting.

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
    buck2_core::error::initialize(box move |category, err, loc, quiet| {
        imp::write_soft_error(
            fb,
            category,
            err,
            buck2_data::Location {
                file: loc.0.to_owned(),
                line: loc.1,
                column: loc.2,
            },
            quiet,
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

mod imp {
    use std::collections::HashMap;
    use std::panic::PanicInfo;
    use std::thread;
    use std::time::Duration;

    use backtrace::Backtrace;
    use buck2_data::Location;
    use buck2_events::metadata;
    use buck2_events::sink::scribe::new_thrift_scribe_sink_if_enabled;
    use buck2_events::BuckEvent;
    use fbinit::FacebookInit;
    use tokio::runtime::Builder;

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
                            .map_or_else(|| "".to_owned(), |s| s.to_string()),
                        address: symbol.addr().map_or_else(|| "".to_owned(), ptr_to_string),
                        file: symbol
                            .filename()
                            .map_or_else(|| "".to_owned(), |p| p.display().to_string()),
                        line: symbol.lineno().unwrap_or(0),
                        column: symbol.colno().unwrap_or(0),
                    })
                    .collect::<Vec<_>>();

                buck2_data::panic::StackFrame {
                    instruction_pointer: ptr_to_string(frame.ip()),
                    symbol_address: ptr_to_string(frame.symbol_address()),
                    module_base_address: frame
                        .module_base_address()
                        .map_or_else(|| "".to_owned(), ptr_to_string),
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
        if let Some(commands) = buck2_server::active_commands::active_commands() {
            let commands = commands.keys().map(|id| id.to_string()).collect::<Vec<_>>();
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
        write_to_scribe(fb, panic_payload(location, message, get_stack(), false));
    }

    pub(crate) fn write_soft_error(
        fb: FacebookInit,
        category: &'static str,
        err: &anyhow::Error,
        location: Location,
        quiet: bool,
    ) {
        let event = panic_payload(
            Some(location),
            format!("Soft Error: {}: {:#}", category, err),
            Vec::new(),
            quiet,
        );

        // If the soft error was fired in a context with an ambient dispatcher, then we only send
        // it there, but some contexts don't have one, and in that case, we notify all running
        // commands.
        match buck2_events::dispatch::get_dispatcher_opt() {
            Some(dispatcher) => {
                dispatcher.instant_event(event.clone());
            }
            None => {
                buck2_server::active_commands::broadcast_instant_event(&event);
            }
        }

        write_to_scribe(fb, event);
    }

    fn panic_payload(
        location: Option<Location>,
        message: String,
        backtrace: Vec<buck2_data::panic::StackFrame>,
        quiet: bool,
    ) -> buck2_data::Panic {
        let metadata = get_metadata_for_panic();
        buck2_data::Panic {
            location,
            payload: message,
            metadata,
            backtrace,
            quiet,
        }
    }

    /// Writes a representation of the given error (hard or soft) to Scribe
    fn write_to_scribe(fb: FacebookInit, data: buck2_data::Panic) {
        use std::time::SystemTime;

        use buck2_core::facebook_only;
        use buck2_data::InstantEvent;
        use buck2_events::sink::scribe;
        use buck2_events::trace::TraceId;

        facebook_only();
        if !scribe::is_enabled() {
            return;
        }

        let sink = match new_thrift_scribe_sink_if_enabled(
            fb,
            /* buffer size */ 100,
            /* retry_backoff */ Duration::from_millis(500),
            /* retry_attempts */ 5,
            /* message_batch_size */ None,
        ) {
            Ok(Some(sink)) => sink,
            _ => {
                // We're already panicking and we can't connect to the scribe daemon? Things are bad and we're SOL.
                return;
            }
        };

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
                runtime.block_on(
                    sink.send_now(BuckEvent::new(
                        SystemTime::now(),
                        TraceId::new(),
                        None,
                        None,
                        InstantEvent {
                            data: Some(data.into()),
                        }
                        .into(),
                    )),
                );
            })
            .map_err(|_| ())
            .and_then(|t| t.join().map_err(|_| ()));
    }
}
