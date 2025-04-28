# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import asyncio
import importlib.resources
import logging
import os
import shutil
import subprocess
import tempfile
import uuid
from contextlib import ExitStack
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any, cast, Dict, List, Optional, Union

from apple.tools.plistlib_utils import detect_format_and_load

from .apple_platform import ApplePlatform
from .codesign_command_factory import (
    DefaultCodesignCommandFactory,
    DryRunCodesignCommandFactory,
    ICodesignCommandFactory,
)
from .fast_adhoc import is_fast_adhoc_codesign_allowed, should_skip_adhoc_signing_path
from .identity import CodeSigningIdentity
from .info_plist_metadata import InfoPlistMetadata
from .list_codesign_identities import IListCodesignIdentities
from .prepare_code_signing_entitlements import prepare_code_signing_entitlements
from .prepare_info_plist import prepare_info_plist
from .provisioning_profile_diagnostics import (
    interpret_provisioning_profile_diagnostics,
    META_IOS_BUILD_AND_RUN_ON_DEVICE_LINK,
    META_IOS_PROVISIONING_PROFILES_COMMAND,
    META_IOS_PROVISIONING_PROFILES_LINK,
)
from .provisioning_profile_metadata import ProvisioningProfileMetadata
from .provisioning_profile_selection import (
    CodeSignProvisioningError,
    select_best_provisioning_profile,
    SelectedProvisioningProfileInfo,
)
from .read_provisioning_profile_command_factory import (
    DefaultReadProvisioningProfileCommandFactory,
    IReadProvisioningProfileCommandFactory,
)

_default_read_provisioning_profile_command_factory = (
    DefaultReadProvisioningProfileCommandFactory()
)

_LOGGER: logging.Logger = logging.getLogger(__name__)


@dataclass
class CodesignedPath:
    path: Path
    """
    Path relative to bundle root which needs to be codesigned
    """
    entitlements: Optional[Path]
    """
    Path to entitlements to be used when codesigning, relative to buck project
    """
    flags: List[str]
    """
    Flags to be passed to codesign command when codesigning this particular path
    """
    extra_file_paths: Optional[List[Path]]
    """
    Extra paths to be codesign. Applicable to dry-run codesigning only.
    """


def _log_codesign_identities(
    list_codesign_identities: IListCodesignIdentities,
    identities: List[CodeSigningIdentity],
) -> None:
    if len(identities) == 0:
        _LOGGER.warning("ZERO codesign identities available")
        _LOGGER.warning(
            f"Identities were retrieved by command: {list_codesign_identities.raw_command()}"
        )
    else:
        _LOGGER.info("Listing available codesign identities")
        for identity in identities:
            _LOGGER.info(
                f"    Subject Common Name: {identity.subject_common_name}, Fingerprint: {identity.fingerprint}"
            )


def _select_provisioning_profile(
    info_plist_metadata: InfoPlistMetadata,
    provisioning_profiles_dir: Path,
    entitlements_path: Optional[Path],
    platform: ApplePlatform,
    list_codesign_identities: IListCodesignIdentities,
    should_use_fast_provisioning_profile_parsing: bool,
    strict_provisioning_profile_search: bool,
    provisioning_profile_filter: Optional[str],
    log_file_path: Optional[Path] = None,
) -> SelectedProvisioningProfileInfo:
    read_provisioning_profile_command_factory = (
        _default_read_provisioning_profile_command_factory
    )
    identities = list_codesign_identities.list_codesign_identities()
    _log_codesign_identities(list_codesign_identities, identities)
    _LOGGER.info(
        f"Fast provisioning profile parsing enabled: {should_use_fast_provisioning_profile_parsing}"
    )
    provisioning_profiles = []
    if should_use_fast_provisioning_profile_parsing:
        provisioning_profiles = asyncio.run(
            _fast_read_provisioning_profiles_async(
                provisioning_profiles_dir,
                read_provisioning_profile_command_factory,
            )
        )
    else:
        provisioning_profiles = _read_provisioning_profiles(
            provisioning_profiles_dir,
            read_provisioning_profile_command_factory,
        )
    if not provisioning_profiles:
        raise CodeSignProvisioningError(
            (
                f"\n\nFailed to find any provisioning profiles. Please make sure to install required provisioning profiles and make sure they are located at '{provisioning_profiles_dir}'.\n\n"
                f"Execute `{META_IOS_PROVISIONING_PROFILES_COMMAND}` to download the profiles.\n"
                f"Please follow the wiki to build & run on device: {META_IOS_BUILD_AND_RUN_ON_DEVICE_LINK}.\n"
                f"Provisioning profiles for your app can also be downloaded from {META_IOS_PROVISIONING_PROFILES_LINK}.\n"
            )
        )
    entitlements = _read_entitlements_file(entitlements_path)
    selected_profile_info, mismatches = select_best_provisioning_profile(
        info_plist_metadata,
        identities,
        provisioning_profiles,
        entitlements,
        platform,
        strict_provisioning_profile_search,
        provisioning_profile_filter,
    )
    if selected_profile_info is None:
        if not mismatches:
            raise RuntimeError(
                f"Expected diagnostics information for at least one mismatching provisioning profile when `{provisioning_profiles_dir}` directory is not empty."
            )
        raise CodeSignProvisioningError(
            interpret_provisioning_profile_diagnostics(
                diagnostics=mismatches,
                bundle_id=info_plist_metadata.bundle_id,
                provisioning_profiles_dir=provisioning_profiles_dir,
                identities=identities,
                log_file_path=log_file_path,
            )
        )
    return selected_profile_info


@dataclass
class SigningContextWithProfileSelection:
    info_plist_source: Path
    info_plist_destination: Path
    info_plist_metadata: InfoPlistMetadata
    selected_profile_info: SelectedProvisioningProfileInfo


@dataclass
class AdhocSigningContext:
    codesign_identity: str
    profile_selection_context: Optional[SigningContextWithProfileSelection]

    def __init__(
        self,
        codesign_identity: Optional[str] = None,
        profile_selection_context: Optional[SigningContextWithProfileSelection] = None,
    ) -> None:
        self.codesign_identity = codesign_identity or "-"
        self.profile_selection_context = profile_selection_context


def signing_context_with_profile_selection(
    info_plist_source: Path,
    info_plist_destination: Path,
    provisioning_profiles_dir: Path,
    entitlements_path: Optional[Path],
    platform: ApplePlatform,
    list_codesign_identities: IListCodesignIdentities,
    log_file_path: Optional[Path] = None,
    should_use_fast_provisioning_profile_parsing: bool = False,
    strict_provisioning_profile_search: bool = False,
    provisioning_profile_filter: Optional[str] = None,
) -> SigningContextWithProfileSelection:
    with open(info_plist_source, mode="rb") as info_plist_file:
        info_plist_metadata = InfoPlistMetadata.from_file(info_plist_file)
    selected_profile_info = _select_provisioning_profile(
        info_plist_metadata=info_plist_metadata,
        provisioning_profiles_dir=provisioning_profiles_dir,
        entitlements_path=entitlements_path,
        platform=platform,
        list_codesign_identities=list_codesign_identities,
        log_file_path=log_file_path,
        should_use_fast_provisioning_profile_parsing=should_use_fast_provisioning_profile_parsing,
        strict_provisioning_profile_search=strict_provisioning_profile_search,
        provisioning_profile_filter=provisioning_profile_filter,
    )

    return SigningContextWithProfileSelection(
        info_plist_source,
        info_plist_destination,
        info_plist_metadata,
        selected_profile_info,
    )


# IMPORTANT: This enum is a part of incremental API, amend carefully.
class CodesignConfiguration(str, Enum):
    fastAdhoc = "fast-adhoc"
    dryRun = "dry-run"


def codesign_bundle(
    bundle_path: CodesignedPath,
    signing_context: Union[AdhocSigningContext, SigningContextWithProfileSelection],
    platform: ApplePlatform,
    codesign_on_copy_paths: List[CodesignedPath],
    codesign_tool: Optional[Path] = None,
    codesign_configuration: Optional[CodesignConfiguration] = None,
) -> None:
    codesign_on_copy_paths = sorted(
        codesign_on_copy_paths,
        key=lambda codesigned_path: codesigned_path.path,
        # Paths must be signed inside out (i.e., deepest first)
        reverse=True,
    )

    with tempfile.TemporaryDirectory() as tmp_dir:
        if isinstance(signing_context, SigningContextWithProfileSelection):
            selection_profile_context = signing_context
        elif isinstance(signing_context, AdhocSigningContext):
            selection_profile_context = signing_context.profile_selection_context
        else:
            raise RuntimeError(
                f"Unexpected type of signing context `{type(signing_context)}`"
            )

        if selection_profile_context:
            bundle_path_with_prepared_entitlements = (
                _prepare_entitlements_and_info_plist(
                    bundle_path=bundle_path,
                    platform=platform,
                    signing_context=selection_profile_context,
                    tmp_dir=tmp_dir,
                )
            )
            selected_identity_fingerprint = (
                selection_profile_context.selected_profile_info.identity.fingerprint
            )
        else:
            if not isinstance(signing_context, AdhocSigningContext):
                raise AssertionError(
                    f"Expected `AdhocSigningContext`, got `{type(signing_context)}` instead."
                )
            if signing_context.profile_selection_context:
                raise AssertionError(
                    "Expected no profile selection context in `AdhocSigningContext` when `selection_profile_context` is `None`."
                )
            bundle_path_with_prepared_entitlements = bundle_path
            selected_identity_fingerprint = signing_context.codesign_identity

        if codesign_configuration is CodesignConfiguration.dryRun:
            if codesign_tool is None:
                raise RuntimeError(
                    "Expected codesign tool not to be the default one when dry run codesigning is requested."
                )
            _dry_codesign_everything(
                root=bundle_path_with_prepared_entitlements,
                codesign_on_copy_paths=codesign_on_copy_paths,
                identity_fingerprint=selected_identity_fingerprint,
                tmp_dir=tmp_dir,
                codesign_tool=codesign_tool,
                platform=platform,
            )
        else:
            fast_adhoc_signing_enabled = (
                codesign_configuration is CodesignConfiguration.fastAdhoc
                and is_fast_adhoc_codesign_allowed()
            )
            _LOGGER.info(f"Fast adhoc signing enabled: {fast_adhoc_signing_enabled}")
            _codesign_everything(
                root=bundle_path_with_prepared_entitlements,
                codesign_on_copy_paths=codesign_on_copy_paths,
                identity_fingerprint=selected_identity_fingerprint,
                tmp_dir=tmp_dir,
                codesign_command_factory=DefaultCodesignCommandFactory(codesign_tool),
                platform=platform,
                fast_adhoc_signing=fast_adhoc_signing_enabled,
            )


def _prepare_entitlements_and_info_plist(
    bundle_path: CodesignedPath,
    platform: ApplePlatform,
    signing_context: SigningContextWithProfileSelection,
    tmp_dir: str,
) -> CodesignedPath:
    info_plist_metadata = signing_context.info_plist_metadata
    selected_profile = signing_context.selected_profile_info.profile
    prepared_entitlements_path = prepare_code_signing_entitlements(
        bundle_path.entitlements,
        info_plist_metadata.bundle_id,
        selected_profile,
        tmp_dir,
    )
    prepared_info_plist_path = prepare_info_plist(
        signing_context.info_plist_source,
        info_plist_metadata,
        selected_profile,
        tmp_dir,
    )
    os.replace(
        prepared_info_plist_path,
        bundle_path.path / signing_context.info_plist_destination,
    )
    shutil.copy2(
        selected_profile.file_path,
        bundle_path.path / platform.embedded_provisioning_profile_path(),
    )
    return CodesignedPath(
        path=bundle_path.path,
        entitlements=prepared_entitlements_path,
        flags=bundle_path.flags,
        extra_file_paths=None,
    )


async def _fast_read_provisioning_profiles_async(
    dirpath: Path,
    read_provisioning_profile_command_factory: IReadProvisioningProfileCommandFactory,
) -> List[ProvisioningProfileMetadata]:
    tasks = []
    for f in os.listdir(dirpath):
        if f.endswith(".mobileprovision") or f.endswith(".provisionprofile"):
            filepath = dirpath / f
            tasks.append(
                _provisioning_profile_from_file_path_async(
                    filepath,
                    read_provisioning_profile_command_factory,
                    should_use_fast_provisioning_profile_parsing=True,
                )
            )
    results = await asyncio.gather(*tasks)
    return cast(List[ProvisioningProfileMetadata], results)


async def _provisioning_profile_from_file_path_async(
    path: Path,
    read_provisioning_profile_command_factory: IReadProvisioningProfileCommandFactory,
    should_use_fast_provisioning_profile_parsing: bool,
) -> ProvisioningProfileMetadata:
    loop = asyncio.get_running_loop()
    return await loop.run_in_executor(
        None,
        _provisioning_profile_from_file_path,
        path,
        read_provisioning_profile_command_factory,
        should_use_fast_provisioning_profile_parsing,
    )


def _read_provisioning_profiles(
    dirpath: Path,
    read_provisioning_profile_command_factory: IReadProvisioningProfileCommandFactory,
) -> List[ProvisioningProfileMetadata]:
    return [
        _provisioning_profile_from_file_path(
            dirpath / f,
            read_provisioning_profile_command_factory,
            should_use_fast_provisioning_profile_parsing=False,
        )
        for f in os.listdir(dirpath)
        if (f.endswith(".mobileprovision") or f.endswith(".provisionprofile"))
    ]


def _provisioning_profile_from_file_path(
    path: Path,
    read_provisioning_profile_command_factory: IReadProvisioningProfileCommandFactory,
    should_use_fast_provisioning_profile_parsing: bool,
) -> ProvisioningProfileMetadata:
    if should_use_fast_provisioning_profile_parsing:
        # Provisioning profiles have a plist embedded in them that we can extract directly.
        # This is much faster than calling an external command like openssl.
        with open(path, "rb") as f:
            content = f.read()
        start_index = content.find(b"<plist")
        end_index = content.find(b"</plist>", start_index) + len(b"</plist>")
        if start_index >= 0 and end_index >= 0:
            plist_data = content[start_index:end_index]
            return ProvisioningProfileMetadata.from_provisioning_profile_file_content(
                path, plist_data
            )
        else:
            _LOGGER.warning(
                f"Failed to find plist in provisioning profile at {path}. Falling back to slow parsing."
            )

    # Fallback to slow parsing if fast parsing is disabled or fails
    return _provisioning_profile_from_file_path_using_factory(
        path, read_provisioning_profile_command_factory
    )


def _provisioning_profile_from_file_path_using_factory(
    path: Path,
    read_provisioning_profile_command_factory: IReadProvisioningProfileCommandFactory,
) -> ProvisioningProfileMetadata:
    output: bytes = subprocess.check_output(
        read_provisioning_profile_command_factory.read_provisioning_profile_command(
            path
        ),
        stderr=subprocess.DEVNULL,
    )
    return ProvisioningProfileMetadata.from_provisioning_profile_file_content(
        path, output
    )


def _read_entitlements_file(path: Optional[Path]) -> Optional[Dict[str, Any]]:
    if not path:
        return None
    with open(path, mode="rb") as f:
        return detect_format_and_load(f)


def _dry_codesign_everything(
    root: CodesignedPath,
    codesign_on_copy_paths: List[CodesignedPath],
    identity_fingerprint: str,
    tmp_dir: str,
    codesign_tool: Path,
    platform: ApplePlatform,
) -> None:
    codesign_command_factory = DryRunCodesignCommandFactory(codesign_tool)

    codesign_on_copy_directory_paths = [
        p for p in codesign_on_copy_paths if p.path.is_dir()
    ]

    # First sign codesign-on-copy directory paths
    _codesign_paths(
        paths=codesign_on_copy_directory_paths,
        identity_fingerprint=identity_fingerprint,
        tmp_dir=tmp_dir,
        codesign_command_factory=codesign_command_factory,
        platform=platform,
    )

    # Dry codesigning creates a .plist inside every directory it signs.
    # That approach doesn't work for files so those files are written into .plist for root bundle.
    codesign_on_copy_file_paths = [
        p.path.relative_to(root.path)
        for p in codesign_on_copy_paths
        if p.path.is_file()
    ]

    if root.extra_file_paths:
        raise RuntimeError(
            f"Root path contains extra file paths: `{root.extra_file_paths}`"
        )

    root_with_extra_paths = CodesignedPath(
        path=root.path,
        entitlements=root.entitlements,
        flags=root.flags,
        extra_file_paths=codesign_on_copy_file_paths,
    )

    # Lastly sign whole bundle
    _codesign_paths(
        paths=[root_with_extra_paths],
        identity_fingerprint=identity_fingerprint,
        tmp_dir=tmp_dir,
        codesign_command_factory=codesign_command_factory,
        platform=platform,
    )


def _codesign_everything(
    root: CodesignedPath,
    codesign_on_copy_paths: List[CodesignedPath],
    identity_fingerprint: str,
    tmp_dir: str,
    codesign_command_factory: ICodesignCommandFactory,
    platform: ApplePlatform,
    fast_adhoc_signing: bool,
) -> None:
    # First sign codesign-on-copy paths
    codesign_on_copy_filtered_paths = _filter_out_fast_adhoc_paths(
        paths=codesign_on_copy_paths,
        identity_fingerprint=identity_fingerprint,
        platform=platform,
        fast_adhoc_signing=fast_adhoc_signing,
    )
    # If we have > 1 paths to sign (including root bundle), access keychain first to avoid user playing whack-a-mole
    # with permission grant dialog windows.
    if codesign_on_copy_filtered_paths:
        obtain_keychain_permissions(
            identity_fingerprint, tmp_dir, codesign_command_factory
        )
    _codesign_paths(
        codesign_on_copy_filtered_paths,
        identity_fingerprint,
        tmp_dir,
        codesign_command_factory,
        platform,
    )
    # Lastly sign whole bundle
    root_filtered_paths = _filter_out_fast_adhoc_paths(
        paths=[root],
        identity_fingerprint=identity_fingerprint,
        platform=platform,
        fast_adhoc_signing=fast_adhoc_signing,
    )
    _codesign_paths(
        root_filtered_paths,
        identity_fingerprint,
        tmp_dir,
        codesign_command_factory,
        platform,
    )


@dataclass
class ParallelProcess:
    process: subprocess.Popen[bytes]
    stdout_path: Optional[str]
    stderr_path: str

    def check_result(self) -> None:
        if self.process.returncode == 0:
            return
        with ExitStack() as stack:
            stderr = stack.enter_context(open(self.stderr_path, encoding="utf8"))
            stderr_string = f"\nstderr:\n{stderr.read()}\n"
            stdout = (
                stack.enter_context(open(self.stdout_path, encoding="utf8"))
                if self.stdout_path
                else None
            )
            stdout_string = f"\nstdout:\n{stdout.read()}\n" if stdout else ""
            raise RuntimeError(f"{stdout_string}{stderr_string}")


def _spawn_process(
    command: List[Union[str, Path]],
    tmp_dir: str,
    stack: ExitStack,
    pipe_stdout: bool = False,
) -> ParallelProcess:
    if pipe_stdout:
        stdout_path = None
        stdout = subprocess.PIPE
    else:
        stdout_path = os.path.join(tmp_dir, uuid.uuid4().hex)
        stdout = stack.enter_context(open(stdout_path, "w"))
    stderr_path = os.path.join(tmp_dir, uuid.uuid4().hex)
    stderr = stack.enter_context(open(stderr_path, "w"))
    _LOGGER.info(f"Executing command: {command}")
    process = subprocess.Popen(command, stdout=stdout, stderr=stderr)
    return ParallelProcess(
        process,
        stdout_path,
        stderr_path,
    )


def _spawn_codesign_process(
    path: CodesignedPath,
    identity_fingerprint: str,
    tmp_dir: str,
    codesign_command_factory: ICodesignCommandFactory,
    stack: ExitStack,
) -> ParallelProcess:
    command = codesign_command_factory.codesign_command(
        path.path,
        identity_fingerprint,
        path.entitlements,
        path.flags,
        path.extra_file_paths,
    )
    return _spawn_process(command=command, tmp_dir=tmp_dir, stack=stack)


def _codesign_paths_serially(
    paths: List[CodesignedPath],
    identity_fingerprint: str,
    tmp_dir: str,
    codesign_command_factory: ICodesignCommandFactory,
    platform: ApplePlatform,
) -> None:
    with ExitStack() as stack:
        for path in paths:
            p = _spawn_codesign_process(
                path=path,
                identity_fingerprint=identity_fingerprint,
                tmp_dir=tmp_dir,
                codesign_command_factory=codesign_command_factory,
                stack=stack,
            )
            p.process.wait()
            p.check_result()


def _codesign_paths_in_parallel(
    paths: List[CodesignedPath],
    identity_fingerprint: str,
    tmp_dir: str,
    codesign_command_factory: ICodesignCommandFactory,
    platform: ApplePlatform,
) -> None:
    """Codesigns several paths in parallel."""
    processes: List[ParallelProcess] = []
    with ExitStack() as stack:
        for path in paths:
            process = _spawn_codesign_process(
                path=path,
                identity_fingerprint=identity_fingerprint,
                tmp_dir=tmp_dir,
                codesign_command_factory=codesign_command_factory,
                stack=stack,
            )
            processes.append(process)
        for p in processes:
            p.process.wait()
    for p in processes:
        p.check_result()


def _can_codesign_paths_in_parallel(codesigned_paths: List[CodesignedPath]) -> bool:
    # To enable parallel signing, there must be no nesting of any codesigned paths,
    # as codesigning must be performed "inside out" - deeper items signed first,
    # as parent items need to seal the contained items as part of their signature.
    #
    # To detect nesting, we reverse sort all paths and only need to check
    # neighboring elements. For example, imagine the following elements:
    # `a/b/c`
    # `a/b`
    # `b`
    # `c`
    #
    # For each element, check if the element is a prefix of the previous element.
    # In the example above, checking if `a/b` is a prefix of `a/b/c` means its
    # unsafe to codesign in parallel.
    paths = sorted([str(path.path) for path in codesigned_paths], reverse=True)
    for index, current_path in enumerate(paths):
        if index == 0:
            continue
        previous_path = paths[index - 1]
        if previous_path.startswith(current_path):
            _LOGGER.warn(
                f"Found overlapping codesigned paths: {previous_path}, {current_path}"
            )
            return False
    return True


def _codesign_paths(
    paths: List[CodesignedPath],
    identity_fingerprint: str,
    tmp_dir: str,
    codesign_command_factory: ICodesignCommandFactory,
    platform: ApplePlatform,
) -> None:
    can_codesign_in_parallel = _can_codesign_paths_in_parallel(paths)
    signing_function = (
        _codesign_paths_in_parallel
        if can_codesign_in_parallel
        else _codesign_paths_serially
    )
    signing_function(
        paths=paths,
        identity_fingerprint=identity_fingerprint,
        tmp_dir=tmp_dir,
        codesign_command_factory=codesign_command_factory,
        platform=platform,
    )


def _filter_out_fast_adhoc_paths(
    paths: List[CodesignedPath],
    identity_fingerprint: str,
    platform: ApplePlatform,
    fast_adhoc_signing: bool,
) -> List[CodesignedPath]:
    if not fast_adhoc_signing:
        return paths
    # TODO(T149863217): Make skip checks run in parallel, they're usually fast (~15ms)
    # but if we have many of them (e.g., 30+ frameworks), it can add about ~0.5s.'
    return [
        p
        for p in paths
        if not should_skip_adhoc_signing_path(
            p.path, identity_fingerprint, p.entitlements, platform
        )
    ]


def obtain_keychain_permissions(
    identity_fingerprint: str,
    tmp_dir: str,
    codesign_command_factory: ICodesignCommandFactory,
) -> None:
    with ExitStack() as stack, importlib.resources.path(
        __package__, "dummy_binary_for_signing"
    ) as dummy_binary_path:
        # Copy the binary to avoid races vs other bundling actions
        dummy_binary_copied = os.path.join(tmp_dir, "dummy_binary_for_signing")
        shutil.copyfile(dummy_binary_path, dummy_binary_copied, follow_symlinks=True)
        p = _spawn_codesign_process(
            path=CodesignedPath(
                path=Path(dummy_binary_copied),
                entitlements=None,
                flags=[],
                extra_file_paths=None,
            ),
            identity_fingerprint=identity_fingerprint,
            tmp_dir=tmp_dir,
            codesign_command_factory=codesign_command_factory,
            stack=stack,
        )
        p.process.wait()
    p.check_result()
