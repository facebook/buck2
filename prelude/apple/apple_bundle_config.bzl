# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":apple_code_signing_types.bzl", "CodeSignConfiguration")

def _maybe_get_bool(config: str, default: [None, bool]) -> [None, bool]:
    result = read_root_config("apple", config, None)
    if result == None:
        return default
    return result.lower() == "true"

def _get_code_signing_configuration() -> str:
    is_dry_run = _maybe_get_bool("dry_run_code_signing", False)

    # This is a kill switch for the feature, it can also be disabled by setting
    # `apple.fast_adhoc_signing_enabled=false` in a global buckconfig file.
    is_fast_adhoc_signing_enabled = _maybe_get_bool("fast_adhoc_signing_enabled", True)

    if is_dry_run:
        return CodeSignConfiguration("dry-run").value
    elif is_fast_adhoc_signing_enabled:
        return CodeSignConfiguration("fast-adhoc").value
    else:
        return CodeSignConfiguration("none").value

def apple_bundle_config() -> dict[str, typing.Any]:
    return {
        "_bundling_cache_buster": read_root_config("apple", "bundling_cache_buster", None),
        "_bundling_log_file_enabled": _maybe_get_bool("bundling_log_file_enabled", True),
        "_bundling_log_file_level": read_root_config("apple", "bundling_log_file_level", None),
        "_code_signing_configuration": _get_code_signing_configuration(),
        "_codesign_identities_command_override": read_root_config("apple", "codesign_identities_command_override", None),
        "_codesign_type": read_root_config("apple", "codesign_type_override", None),
        "_compile_resources_locally_override": _maybe_get_bool("compile_resources_locally_override", None),
        "_embed_provisioning_profile_when_adhoc_code_signing": _maybe_get_bool("embed_provisioning_profile_when_adhoc_code_signing", None),
        "_fast_provisioning_profile_parsing_enabled": _maybe_get_bool("fast_provisioning_profile_parsing_enabled", False),
        "_incremental_bundling_enabled": _maybe_get_bool("incremental_bundling_enabled", True),
        "_info_plist_identify_build_system_default": _maybe_get_bool("info_plist_identify_build_system", True),
        "_profile_bundling_enabled": _maybe_get_bool("profile_bundling_enabled", False),
        "_use_entitlements_when_adhoc_code_signing": _maybe_get_bool("use_entitlements_when_adhoc_code_signing", None),
    }
