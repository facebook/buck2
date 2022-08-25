/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Utilities for interacting with the jemalloc heap used by buck2.
//!
//! In order to make use of jemalloc's heap dump or profiling utilities, you must set the MALLOC_CONF environment
//! variable to a suitable value prior to launching the daemon, such as:
//!  `export MALLOC_CONF=prof:true,prof_final:false,prof_prefix:/tmp/jeprof`
//! This turns on the profiler (`prof:true`), tells the profile to not take heap dump on process exit,
//! (`prof_final:false`), and write dumps to `/tmp/jeprof` if a file argument isn't given to `mallctl`
//! (`prof_prefix:/tmp/jeprof`).

#[cfg(fbcode_build)]
mod imp {
    use std::env;

    /// Output the current state of the heap to the filename specified.
    /// Intended to be used for debugging purposes.
    /// Requires MALLOC_CONF=prof:true to be set in environment variables
    /// when run, though must be built without MALLOC_CONF=prof:true.
    pub fn write_heap_to_file(filename: &str) -> anyhow::Result<()> {
        if !memory::is_using_jemalloc() {
            return Err(anyhow::anyhow!(
                "not using jemalloc; are you building with @mode/dev or @mode/dbgo?"
            ));
        }

        let prof_enabled: bool = memory::mallctl_read("opt.prof")?;
        if !prof_enabled {
            if env::var_os("MALLOC_CONF").is_some() {
                return Err(anyhow::anyhow!(
                    "the environment variable MALLOC_CONF is set, but profiling is not enabled. MALLOC_CONF must contain prof:true to enable the profiler"
                ));
            }

            return Err(anyhow::anyhow!(
                "profiling is not enabled for this process; you must set the environment variable MALLOC_CONF to contain at least prof:true in order to profile"
            ));
        }

        eprintln!("dumping heap to: {:?}", filename);
        memory::mallctl_write("prof.dump", filename)?;
        Ok(())
    }

    /// Dump allocator stats from JEMalloc. Intended for debug purposes
    pub fn allocator_stats(options: &str) -> anyhow::Result<String> {
        allocator_stats::malloc_stats(options)
    }

    /// Enables background threads for jemalloc. See [here](http://jemalloc.net/jemalloc.3.html#background_thread) for
    /// jemalloc documentation. When set, jemalloc will use background threads to purge unused dirty pages instead of
    /// doing it synchronously.
    ///
    /// This function has no effect if not using jemalloc.
    pub fn enable_background_threads() -> anyhow::Result<()> {
        if memory::is_using_jemalloc() {
            memory::mallctl_write("background_thread", true)?;
        }
        Ok(())
    }
}

#[cfg(not(fbcode_build))]
mod imp {

    pub fn write_heap_to_file(_filename: &str) -> anyhow::Result<()> {
        // TODO(swgillespie) the `jemalloc_ctl` crate is probably capable of doing this
        // and we already link against it
        Err(anyhow::anyhow!(
            "not implemented: heap dump for Cargo builds"
        ))
    }

    pub fn allocator_stats(_: &str) -> anyhow::Result<String> {
        Err(anyhow::anyhow!(
            "not implemented: alloctor stats  for Cargo builds"
        ))
    }

    pub fn enable_background_threads() -> anyhow::Result<()> {
        Ok(())
    }
}

pub use imp::allocator_stats;
pub use imp::enable_background_threads;
pub use imp::write_heap_to_file;
