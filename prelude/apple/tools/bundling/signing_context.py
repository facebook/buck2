# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import shlex
from pathlib import Path
from typing import Optional, Union

from apple.tools.code_signing.apple_platform import ApplePlatform
from apple.tools.code_signing.codesign_bundle import (
    AdhocSigningContext,
    signing_context_with_profile_selection,
    SigningContextWithProfileSelection,
)
from apple.tools.code_signing.list_codesign_identities import (
    AdHocListCodesignIdentities,
    ListCodesignIdentities,
)


def add_args_for_signing_context(parser: argparse.ArgumentParser):
    parser.add_argument(
        "--codesign",
        action="store_true",
        help="Should the final bundle be codesigned.",
    )
    parser.add_argument(
        "--ad-hoc",
        action="store_true",
        help="Perform ad-hoc signing if set.",
    )
    parser.add_argument(
        "--ad-hoc-codesign-identity",
        metavar="<identity>",
        type=str,
        required=False,
        help="Codesign identity to use when ad-hoc signing is performed. Should be present when selection of provisioining profile is requested for ad-hoc signing.",
    )
    parser.add_argument(
        "--codesign-identities-command",
        metavar='<"/signing/identities --available">',
        type=str,
        required=False,
        help="Command listing available code signing identities. If it's not provided `security` utility is assumed to be available and is used.",
    )
    parser.add_argument(
        "--profiles-dir",
        metavar="</provisioning/profiles/directory>",
        type=Path,
        required=False,
        help="Required if non-ad-hoc code signing is requested. Path to directory with provisioning profile files.",
    )
    parser.add_argument(
        "--embed-provisioning-profile-when-signing-ad-hoc",
        action="store_true",
        help="Perform selection of provisioining profile and embed it into final bundle when ad-hoc signing if set.",
    )
    parser.add_argument(
        "--fast-provisioning-profile-parsing",
        action="store_true",
        help="Uses experimental faster provisioning profile parsing.",
    )
    parser.add_argument(
        "--strict-provisioning-profile-search",
        action="store_true",
        required=False,
        help="Fail code signing if more than one matching profile found.",
    )
    parser.add_argument(
        "--provisioning-profile-filter",
        metavar="<regex>",
        type=str,
        required=False,
        help="Regex to disambiguate multiple matching profiles, evaluated against provisioning profile filename.",
    )
    parser.add_argument(
        "--entitlements",
        metavar="<Entitlements.plist>",
        type=Path,
        required=False,
        help="Path to file with entitlements to be used during code signing. If it's not provided the minimal entitlements are going to be generated.",
    )
    parser.add_argument(
        "--info-plist-source",
        metavar="</prepared/Info.plist>",
        type=Path,
        required=False,
        help="Path to Info.plist source file which is used only to make code signing decisions (to be bundled `Info.plist` should be present in spec parameter). Required if code signing is requested.",
    )
    parser.add_argument(
        "--info-plist-destination",
        metavar="<Info.plist>",
        type=Path,
        required=False,
        help="Required if code signing is requested. Bundle relative destination path to Info.plist file if it is present in bundle.",
    )
    parser.add_argument(
        "--platform",
        metavar="<apple platform>",
        type=ApplePlatform,
        required=False,
        help="Required if code signing or Swift support is requested. Apple platform for which the bundle is built.",
    )
    parser.add_argument(
        "--log-file",
        type=Path,
        required=False,
        help="Path to a log file. If present logging will be directed to this file in addition to stderr.",
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
                    should_verify_entitlements=args.verify_entitlements,
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
                should_verify_entitlements=args.verify_entitlements,
            )
            selected_identity_argument = (
                signing_context.selected_profile_info.identity.fingerprint
            )
    else:
        signing_context = None
        selected_identity_argument = None

    return signing_context, selected_identity_argument
