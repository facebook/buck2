#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# `cat x | cut` is stylistically fine
# shellcheck disable=SC2002

set -e
set -o pipefail

# `argv[1]` to specify the time until the scope should be automatically cleaned up
scope_ttl=${1:-10m}

# Systemd destroys and garbage collects scopes when they no longer contain any processes
#
# Control the time until the scope dies by spawning a process into it that sleeps for the
# appropriate amount of time
create_keepalive_proc='{ sleep '"$scope_ttl"' >/dev/null & disown; echo $!; } 2>/dev/null'

keep_alive_pid=$(systemd-run \
    --user \
    --quiet \
    --unit testpcg$RANDOM \
    --scope \
    --property=Delegate=yes \
    --slice-inherit \
    --expand-environment=no \
    --property=MemoryAccounting=yes \
    -- \
    bash -c "$create_keepalive_proc")
echo "Keep alive PID: $keep_alive_pid (kill to clean up)" >&2
cgd=/sys/fs/cgroup$(cat /proc/"$keep_alive_pid"/cgroup | cut -c '4-')
mkdir "$cgd"/_keep_alive
echo "$keep_alive_pid" > "$cgd"/_keep_alive/cgroup.procs
mkdir "$cgd"/action
echo +memory > "$cgd"/cgroup.subtree_control
echo "$cgd"/action
