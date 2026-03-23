# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import copy
import os
import plistlib
import tempfile
from pathlib import Path
from typing import Dict, Optional

from apple.tools.info_plist_processor.process import process as process_info_plist

from .provisioning_profile_metadata import ProvisioningProfileMetadata


# Buck v1 corresponding code is in `ProvisioningProfileCopyStep::execute` in `ProvisioningProfileCopyStep.java`
def prepare_code_signing_entitlements(
    entitlements_path: Optional[Path],
    bundle_id: str,
    profile: ProvisioningProfileMetadata,
    tmp_dir: str,
    entitlements_suffixed_key_map: Optional[Dict[str, str]] = None,
) -> Path:
    fd, output_path = tempfile.mkstemp(dir=tmp_dir)
    with os.fdopen(fd, mode="wb") as output:
        if entitlements_path:
            with open(entitlements_path, "rb") as entitlements_file:
                process_info_plist(
                    input_file=entitlements_file,
                    output_file=output,
                    additional_keys=profile.get_mergeable_entitlements(),
                    output_format=plistlib.FMT_XML,
                )
        else:
            app_id = profile.get_app_id().team_id + "." + bundle_id
            entitlements = profile.get_mergeable_entitlements()
            entitlements["application-identifier"] = app_id
            entitlements["keychain-access-groups"] = [app_id]
            plistlib.dump(entitlements, output, fmt=plistlib.FMT_XML)

    if entitlements_suffixed_key_map:
        with open(output_path, "rb") as f:
            entitlements = plistlib.load(f)
        original = copy.deepcopy(entitlements)
        for key, suffix in entitlements_suffixed_key_map.items():
            if key in entitlements:
                value = entitlements[key]
                if isinstance(value, str):
                    entitlements[key] = value + suffix
                elif isinstance(value, list):
                    entitlements[key] = [v + suffix for v in value]
        if entitlements != original:
            with open(output_path, "wb") as f:
                plistlib.dump(entitlements, f, fmt=plistlib.FMT_XML)

    return Path(output_path)
