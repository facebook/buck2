# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import shlex

from typing import Optional, Union

from apple.tools.code_signing.codesign_bundle import (
    AdhocSigningContext,
    signing_context_with_profile_selection,
    SigningContextWithProfileSelection,
)
from apple.tools.code_signing.list_codesign_identities import (
    AdHocListCodesignIdentities,
    ListCodesignIdentities,
)


def signing_context_and_selected_identity_from_args(
    args: argparse.Namespace,
) -> (
    Optional[Union[AdhocSigningContext, SigningContextWithProfileSelection]],
    Optional[str],
):
    if args.codesign:
        if not args.info_plist_source:
            raise RuntimeError(
                "Paths to Info.plist source file should be set when code signing is required."
            )
        if not args.info_plist_destination:
            raise RuntimeError(
                "Info.plist destination path should be set when code signing is required."
            )
        if not args.platform:
            raise RuntimeError(
                "Apple platform should be set when code signing is required."
            )
        list_codesign_identities = (
            ListCodesignIdentities.override(
                shlex.split(args.codesign_identities_command)
            )
            if args.codesign_identities_command
            else ListCodesignIdentities.default()
        )
        if args.ad_hoc:
            if args.embed_provisioning_profile_when_signing_ad_hoc:
                if not args.profiles_dir:
                    raise RuntimeError(
                        "Path to directory with provisioning profile files should be set when selection of provisioining profile is enabled for ad-hoc code signing."
                    )
                if not args.ad_hoc_codesign_identity:
                    raise RuntimeError(
                        "Code signing identity should be set when selection of provisioining profile is enabled for ad-hoc code signing."
                    )
                profile_selection_context = signing_context_with_profile_selection(
                    info_plist_source=args.info_plist_source,
                    info_plist_destination=args.info_plist_destination,
                    provisioning_profiles_dir=args.profiles_dir,
                    entitlements_path=args.entitlements,
                    platform=args.platform,
                    list_codesign_identities=AdHocListCodesignIdentities(
                        original=list_codesign_identities,
                        subject_common_name=args.ad_hoc_codesign_identity,
                    ),
                    log_file_path=args.log_file,
                    should_use_fast_provisioning_profile_parsing=args.fast_provisioning_profile_parsing,
                    strict_provisioning_profile_search=args.strict_provisioning_profile_search,
                    provisioning_profile_filter=args.provisioning_profile_filter,
                )
            else:
                profile_selection_context = None
            signing_context = AdhocSigningContext(
                codesign_identity=args.ad_hoc_codesign_identity,
                profile_selection_context=profile_selection_context,
            )
            selected_identity_argument = args.ad_hoc_codesign_identity
        else:
            if not args.profiles_dir:
                raise RuntimeError(
                    "Path to directory with provisioning profile files should be set when signing is not ad-hoc."
                )
            signing_context = signing_context_with_profile_selection(
                info_plist_source=args.info_plist_source,
                info_plist_destination=args.info_plist_destination,
                provisioning_profiles_dir=args.profiles_dir,
                entitlements_path=args.entitlements,
                platform=args.platform,
                list_codesign_identities=list_codesign_identities,
                log_file_path=args.log_file,
                should_use_fast_provisioning_profile_parsing=args.fast_provisioning_profile_parsing,
                strict_provisioning_profile_search=args.strict_provisioning_profile_search,
                provisioning_profile_filter=args.provisioning_profile_filter,
            )
            selected_identity_argument = (
                signing_context.selected_profile_info.identity.fingerprint
            )
    else:
        signing_context = None
        selected_identity_argument = None

    return signing_context, selected_identity_argument
