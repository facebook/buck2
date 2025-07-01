# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _basename_start_index(p: str) -> int:
    last_sep = p.rfind("/")

    # If there is no slash, then the basename starts at the beginning of the string,
    # which doesn't require special handling, since we get an index of -1, add one, and
    # get 0, i.e. the beginning of the string.
    return last_sep + 1

def strip_extension(p: str) -> str:
    """Removes the extension of the file at the end of a path.

    Args:
      p: The path whose extension should be removed.
    Returns:
      The path without the extension
    """
    basename_start_index = _basename_start_index(p)
    last_dot_in_basename = p.rfind(".", basename_start_index, None)

    # If there is no dot or the only dot in the basename is at the front, then
    # there is no extension.
    if last_dot_in_basename <= 0:
        return p

    dot_distance_from_end = len(p) - last_dot_in_basename
    return p[:-dot_distance_from_end]

def basename_without_extension(p: str) -> str:
    """
    Args:
      p: The path whose basename should be extracted.
    Returns:
      The path's basename without the extension
    """
    basename_start_index = _basename_start_index(p)
    last_dot_in_basename = p.rfind(".", basename_start_index, None)

    # If there is no dot or the only dot in the basename is at the front, then
    # there is no extension.
    if last_dot_in_basename <= 0:
        return p

    dot_distance_from_end = len(p) - last_dot_in_basename
    return p[basename_start_index:-dot_distance_from_end]

def split_basename(p: str) -> (str, str):
    """Extracts the basename of a path, and returns the extension and the basename without the extension.
    Args:
      p: The path whose basename should be extracted.
    Returns:
      The path's basename without the extension, and the extension itself (including the leading dot)
    """
    basename_start_index = _basename_start_index(p)
    last_dot_in_basename = p.rfind(".", basename_start_index, None)

    # If there is no dot or the only dot in the basename is at the front, then
    # there is no extension.
    if last_dot_in_basename <= 0:
        return (p, "")

    dot_distance_from_end = len(p) - last_dot_in_basename
    return (p[basename_start_index:-dot_distance_from_end], p[-dot_distance_from_end:])

def get_extension(p: str) -> str:
    """Gets the extension of the file at the end of a path.

    Args:
      p: The path whose extension should be found.
    Returns:
      Just the extension of file at the end of the path.
    """
    basename_start_index = _basename_start_index(p)
    last_dot_in_basename = p.rfind(".", basename_start_index, None)

    # If there is no dot or the only dot in the basename is at the front, then
    # there is no extension.
    if last_dot_in_basename <= 0:
        return p

    dot_distance_from_end = len(p) - last_dot_in_basename
    return p[-dot_distance_from_end:]

def has_extension(p: str, ext: str) -> bool:
    """Determines whether the given path's file has the given extension.

    Args:
      p: The path whose extension should be checked.
      ext: The extension to check for (including the leading '.').
    Returns:
      True, if the file at the end of a path has the given extension.
    """
    if ext.startswith(".") and (ext.find("/") == -1):
        return p.endswith(ext)
    else:
        fail("Extension must start with a dot and not contain a slash: '{}'".format(ext))
