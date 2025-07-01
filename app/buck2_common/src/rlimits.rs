/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#[cfg(unix)]
pub fn raise_file_descriptor_limits() -> buck2_error::Result<()> {
    use buck2_error::BuckErrorContext;
    use nix::sys::resource;
    use nix::sys::resource::Resource;

    let (soft_limit, hard_limit) = resource::getrlimit(Resource::RLIMIT_NOFILE)?;

    // If the soft limit is already maxxed out or "big enough", don't mess with it.
    //
    // The "big enough" value is the default value on Linux.
    if soft_limit == hard_limit || soft_limit >= 0x80_000 {
        return Ok(());
    }

    // Set the maximum number of open file descriptors to the hard limit.
    //
    // The `systemd.exec` man page says this about setting the limits with `ulimit -n` directly:
    //
    // > Don't use. Be careful when raising the soft limit above 1024, since `select(2)` cannot
    // > function with file descriptors above 1023 on Linux. Nowadays, the hard limit defaults to
    // > 524288, a very high value compared to historical defaults. Typically applications should
    // > increase their soft limit to the hard limit on their own, if they are OK with working with
    // > file descriptors above 1023, i.e. do not use `select(2)`. Note that file descriptors are
    // > nowadays accounted like any other form of memory, thus there should not be any need to
    // > lower the hard limit.
    //
    // However, this is practically not an issue for Buck for a number of reasons, most importantly
    // it is in fact a program that needs a lot of open files. But also, it doesn't matter what the
    // systemd man page says in reality, because if the user hits the soft limit, they're just going
    // to run `ulimit -n 10000000` and move on, so we might as well just do effectively that for
    // them. Therefore, we don't reset rlimits anywhere and just let all subprocesses inherit this.
    //
    // See: https://www.freedesktop.org/software/systemd/man/devel/systemd.exec.html
    //
    // And: https://github.com/facebook/buck2/pull/986
    resource::setrlimit(Resource::RLIMIT_NOFILE, hard_limit, hard_limit)
        .with_buck_error_context(|| format!(
            "Open file descriptor limit is lower than 80,000. Buck needs a lot of FDs. Raising open FD limit from {} to hard limit {}",
            soft_limit,
            hard_limit,
        ))?;

    Ok(())
}

#[cfg(not(unix))]
pub fn raise_file_descriptor_limits() -> buck2_error::Result<()> {
    Ok(())
}
