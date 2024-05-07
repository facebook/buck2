# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import pathlib
import sys
from typing import List, Optional

from tap import Tap

from .apple_platform import ApplePlatform
from .codesign_bundle import (
    AdhocSigningContext,
    codesign_bundle,
    CodesignedPath,
    signing_context_with_profile_selection,
)
from .list_codesign_identities import ListCodesignIdentities
from .provisioning_profile_selection import CodeSignProvisioningError


class Arguments(Tap):  # pyre-ignore[13] ignore uninitialized attributes for typed argument parser
    """
    Tool which code signs the Apple bundle. `Info.plist` file is amended as a part of it.
    """

    bundle_path: pathlib.Path
    info_plist: pathlib.Path
    entitlements: Optional[pathlib.Path] = None
    profiles_dir: Optional[pathlib.Path] = None
    ad_hoc: bool = False
    ad_hoc_codesign_identity: Optional[str] = None
    platform: ApplePlatform
    codesign_on_copy: Optional[List[pathlib.Path]] = None
    fast_provisioning_profile_parsing: bool = False
    strict_provisioning_profile_search: bool = False

    def configure(self) -> None:
        """
        Configure the arguments.
        """
        self.add_argument(
            "--bundle-path",
            metavar="</path/to/app.bundle>",
            type=pathlib.Path,
            required=True,
            help="Absolute path to Apple bundle result.",
        )
        self.add_argument(
            "--info-plist",
            metavar="<Info.plist>",
            type=pathlib.Path,
            required=True,
            help="Bundle relative destination path to Info.plist file if it is present in bundle.",
        )
        self.add_argument(
            "--entitlements",
            metavar="<Entitlements.plist>",
            type=pathlib.Path,
            required=False,
            help="Path to file with entitlements to be used during code signing. If it's not provided the minimal entitlements are going to be generated.",
        )
        self.add_argument(
            "--profiles-dir",
            metavar="</provisioning/profiles/directory>",
            type=pathlib.Path,
            required=False,
            help="Path to directory with provisioning profile files. Required if code signing is not ad-hoc.",
        )
        self.add_argument(
            "--ad-hoc",
            action="store_true",
            required=False,
            help="Perform ad-hoc signing if set.",
        )
        self.add_argument(
            "--ad-hoc-codesign-identity",
            metavar="<identity>",
            type=str,
            required=False,
            help="Codesign identity to use when ad-hoc signing is performed.",
        )
        self.add_argument(
            "--platform",
            metavar="<apple platform>",
            type=ApplePlatform,
            required=True,
            help="Apple platform for which the bundle was built.",
        )
        self.add_argument(
            "--codesign-on-copy",
            metavar="<codesign/this/path>",
            type=pathlib.Path,
            action="append",
            required=False,
            help="Bundle relative path that should be codesigned prior to result bundle.",
        )
        self.add_argument(
            "--fast-provisioning-profile-parsing",
            action="store_true",
            required=False,
            help="Uses experimental faster provisioning profile parsing.",
        )
        self.add_argument(
            "--strict-provisioning-profile-search",
            action="store_true",
            required=False,
            help="Fail code signing if more than one matching profile found.",
        )


# Add emoji to beginning of actionable error message so it stands out more.
def decorate_error_message(message: str) -> str:
    return " ".join(["❗️", message])


def _main() -> None:
    args = Arguments().parse_args()
    try:
        if args.ad_hoc:
            signing_context = AdhocSigningContext(
                codesign_identity=args.ad_hoc_codesign_identity
            )
        else:
            assert (
                args.profiles_dir
            ), "Path to directory with provisioning profile files should be set when signing is not ad-hoc."
            non_optional_profiles_dir = args.profiles_dir
            signing_context = signing_context_with_profile_selection(
                info_plist_source=args.bundle_path / args.info_plist,
                info_plist_destination=args.info_plist,
                provisioning_profiles_dir=non_optional_profiles_dir,
                entitlements_path=args.entitlements,
                list_codesign_identities=ListCodesignIdentities.default(),
                platform=args.platform,
                should_use_fast_provisioning_profile_parsing=args.fast_provisioning_profile_parsing,
                strict_provisioning_profile_search=args.strict_provisioning_profile_search,
            )

        bundle_path = CodesignedPath(
            path=args.bundle_path, entitlements=args.entitlements, flags=[]
        )
        codesign_on_copy_paths = (
            [
                CodesignedPath(
                    path=bundle_path.path / path, entitlements=None, flags=[]
                )
                for path in args.codesign_on_copy
            ]
            if args.codesign_on_copy
            else []
        )

        codesign_bundle(
            bundle_path=bundle_path,
            signing_context=signing_context,
            platform=args.platform,
            codesign_on_copy_paths=codesign_on_copy_paths,
        )
    except CodeSignProvisioningError as e:
        print(decorate_error_message(str(e)), file=sys.stderr)
        exit(1)


if __name__ == "__main__":
    _main()
