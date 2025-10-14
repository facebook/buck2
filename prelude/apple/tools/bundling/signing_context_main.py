# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import argparse
import base64
import json
from pathlib import Path

from apple.tools.code_signing.codesign_bundle import (
    selection_profile_context_from_signing_context,
)

from .signing_context import (
    add_args_for_signing_context,
    signing_context_and_selected_identity_from_args,
)


def _main() -> None:
    parser = argparse.ArgumentParser(
        description="Tool which outputs the signing context for an apple_bundle().",
    )
    parser.add_argument(
        "--output",
        required=True,
        type=Path,
        help="Path to the output JSON file.",
    )
    add_args_for_signing_context(parser)

    args = parser.parse_args()
    signing_context, selected_identity = (
        signing_context_and_selected_identity_from_args(args)
    )

    with open(args.output, "w") as output_file:
        signing_context_json_obj = {
            "version": 1,
        }

        if selected_identity:
            # Adhoc and Developer ID builds will only have `codesign_identity`
            # (in which case, it would the human readable identity).
            #
            # For provisioned builds (i.e., with a prov profile), it would be the
            # signing cert fingerprint (i.e., SHA1 hash of cert in DER format)
            signing_context_json_obj["codesign_identity"] = selected_identity

        selection_profile_context = selection_profile_context_from_signing_context(
            signing_context
        )
        if selection_profile_context:
            selected_profile_info = selection_profile_context.selected_profile_info
            profile_metadata = selected_profile_info.profile
            with open(profile_metadata.file_path, "rb") as prov_profile_file:
                prov_profile_as_base64_utf8 = base64.standard_b64encode(
                    prov_profile_file.read()
                ).decode()

            signing_context_json_obj["provisioning_profile"] = {
                "uuid": profile_metadata.uuid,
                "identity": {
                    "fingerprint": selected_profile_info.identity.fingerprint,
                    "subject_common_name": selected_profile_info.identity.subject_common_name,
                },
                "file_name": profile_metadata.file_path.name,
                "file_data_base64": prov_profile_as_base64_utf8,
            }

        json.dump(
            signing_context_json_obj,
            output_file,
            indent=4,
        )


if __name__ == "__main__":
    _main()
