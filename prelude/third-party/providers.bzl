# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Work-around for buck2 bug causing "transitive values must be of the same
# transitive set type" errors:
# https://fb.prod.workplace.com/groups/buck2users/posts/3637287806527574/
ThirdPartyBuildTSet = transitive_set()
ThirdPartyBuildInfo = provider(fields = {
    "_tset": provider_field(ThirdPartyBuildTSet),
})
