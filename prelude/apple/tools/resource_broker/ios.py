# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os

from distutils.version import StrictVersion
from typing import List, Optional

from .idb_companion import IdbCompanion

from .idb_target import IdbTarget, managed_simulators_from_stdout, SimState

from .simctl_runtime import list_ios_runtimes, XCSimRuntime

from .timeouts import SIMULATOR_BOOT_TIMEOUT

from .utils import (
    execute_generic_text_producing_command,
    spawn_companion,
    wait_for_idb_companions,
)


def _device_set_path() -> str:
    return os.path.expanduser("~/Library/Developer/Buck2IdbDeviceSet")


def _list_managed_simulators_command(simulator_manager: str) -> List[str]:
    return [
        simulator_manager,
        "list",
        "--device-set-path",
        _device_set_path(),
        "--only",
        "simulator",
    ]


def _create_simulator_command(simulator_manager: str, sim_spec: str) -> List[str]:
    return [
        simulator_manager,
        "create",
        "--device-set-path",
        _device_set_path(),
        "--configuration",
        sim_spec,
    ]


def _boot_simulator_command(simulator_manager: str, udid: str) -> List[str]:
    return [
        simulator_manager,
        "boot",
        "--device-set-path",
        _device_set_path(),
        udid,
    ]


def _compatible_device_type_from_runtime(runtime: XCSimRuntime) -> Optional[str]:
    iphones = filter(
        lambda t: t.product_family == "iPhone", runtime.supported_device_types
    )
    if not iphones:
        return None
    default = next(iphones)
    return next(
        (device_type.name for device_type in iphones if device_type.name == "iPhone 8"),
        default.name,
    )


def _select_latest_simulator_spec(runtimes: List[XCSimRuntime]) -> str:
    runtimes.sort(key=lambda x: StrictVersion(x.version), reverse=True)
    for runtime in runtimes:
        device_type = _compatible_device_type_from_runtime(runtime)
        if device_type:
            return f"{device_type},{runtime.name}"
    raise RuntimeError(
        "No XCode simctl compatible iOS runtime and device available. Try to `sudo xcode-select -s <path_to_xcode>` and *open Xcode to install all required components*."
    )


def _spawn_companion_for_simulator_command(
    udid: str, grpc_domain_sock: str
) -> List[str]:
    return [
        "idb_companion",
        "--device-set-path",
        _device_set_path(),
        "--udid",
        udid,
        "--only",
        "simulator",
        "--grpc-domain-sock",
        grpc_domain_sock,
    ]


async def _generic_managed_simulators_command(
    name: str, cmd: List[str]
) -> List[IdbTarget]:
    stdout = await execute_generic_text_producing_command(name=name, cmd=cmd)
    return managed_simulators_from_stdout(stdout)


async def _list_managed_simulators(simulator_manager: str) -> List[IdbTarget]:
    list_cmd = _list_managed_simulators_command(simulator_manager=simulator_manager)
    return await _generic_managed_simulators_command(
        name="list managed simulators", cmd=list_cmd
    )


async def _create_simulator(simulator_manager: str) -> List[IdbTarget]:
    runtimes = await list_ios_runtimes()
    spec = _select_latest_simulator_spec(runtimes)
    create_cmd = _create_simulator_command(
        simulator_manager=simulator_manager, sim_spec=spec
    )
    return await _generic_managed_simulators_command(
        name="create simulators", cmd=create_cmd
    )


async def _get_managed_simulators_create_if_needed(
    simulator_manager: str,
) -> List[IdbTarget]:
    managed_simulators = await _list_managed_simulators(
        simulator_manager=simulator_manager
    )
    if managed_simulators:
        return managed_simulators

    managed_simulators = await _create_simulator(simulator_manager=simulator_manager)
    if managed_simulators:
        return managed_simulators

    raise RuntimeError(
        "Failed to create an iOS simulator. Try to `sudo xcode-select -s <path_to_xcode>` and *open Xcode to install all required components*."
    )


def _select_simulator(
    only_booted: bool, all_simulators: List[IdbTarget]
) -> Optional[IdbTarget]:
    return next(
        filter(
            lambda s: s.state == SimState.booted if only_booted else True,
            iter(all_simulators),
        ),
        None,
    )


def _select_simulator_with_preference(
    prefer_booted: bool, all_simulators: List[IdbTarget]
) -> IdbTarget:
    simulator = _select_simulator(
        only_booted=prefer_booted, all_simulators=all_simulators
    )
    if not simulator and prefer_booted:
        simulator = _select_simulator(only_booted=False, all_simulators=all_simulators)
    if not simulator:
        raise RuntimeError("Expected at least unbooted simulator entity to be selected")
    return simulator


async def _ios_simulator(simulator_manager: str, booted: bool) -> List[IdbCompanion]:
    managed_simulators = await _get_managed_simulators_create_if_needed(
        simulator_manager=simulator_manager
    )
    simulator = _select_simulator_with_preference(
        prefer_booted=booted, all_simulators=managed_simulators
    )
    if simulator.state != SimState.booted and booted:
        boot_cmd = _boot_simulator_command(
            simulator_manager=simulator_manager, udid=simulator.udid
        )
        await execute_generic_text_producing_command(
            name="boot simulator",
            cmd=boot_cmd,
            timeout=SIMULATOR_BOOT_TIMEOUT,
        )

    grpc_domain_sock = f"/tmp/buck2_idb_companion_{simulator.udid}"
    process = await spawn_companion(
        command=_spawn_companion_for_simulator_command(
            simulator.udid, grpc_domain_sock
        ),
        log_file_suffix=f"companion_launch_logs_for_{simulator.udid}.log",
    )
    return await wait_for_idb_companions([process])


async def ios_unbooted_simulator(simulator_manager: str) -> List[IdbCompanion]:
    return await _ios_simulator(simulator_manager=simulator_manager, booted=False)


async def ios_booted_simulator(simulator_manager: str) -> List[IdbCompanion]:
    return await _ios_simulator(simulator_manager=simulator_manager, booted=True)
