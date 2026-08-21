# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Optional, Union

from .identity import CodeSigningIdentity
from .info_plist_metadata import InfoPlistMetadata
from .provisioning_profile_selection import SelectedProvisioningProfileInfo


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

    def identity(self) -> CodeSigningIdentity:
        if self.profile_selection_context:
            return self.profile_selection_context.selected_profile_info.identity
        return CodeSigningIdentity(
            fingerprint=self.codesign_identity,
            subject_common_name="",
        )


def selection_profile_context_from_signing_context(
    signing_context: Optional[
        Union[AdhocSigningContext, SigningContextWithProfileSelection]
    ],
) -> Optional[SigningContextWithProfileSelection]:
    if signing_context:
        if isinstance(signing_context, SigningContextWithProfileSelection):
            selection_profile_context = signing_context
        elif isinstance(signing_context, AdhocSigningContext):
            selection_profile_context = signing_context.profile_selection_context
        else:
            raise RuntimeError(
                f"Unexpected type of signing context `{type(signing_context)}`"
            )
        return selection_profile_context
    return None
