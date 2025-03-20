# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.


class ExternalError(Exception):
    """
    Exception that indicates an error outside make_par, e.g.
    a problem with one of the Python files to be inserted into the archive.
    """

    pass


class ParStyleError(Exception):
    """Exception to throw if the user specifies an uncaught invalid par type"""

    pass


class MakeArchiveError(Exception):
    pass


class MainModuleError(MakeArchiveError):
    pass
