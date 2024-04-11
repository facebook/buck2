# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""Provides macros for working with .buckconfig."""

load(":expect.bzl", "expect")
load(":lazy.bzl", "lazy")

def _decode_raw_word(val, start, delimiter = None):
    """
    Read characters up to the given delimiter in a string supporting and
    stripping quoting (i.e. `"`) and escape characters (i.e. `\\`).

    Args:
      val: Input string to evaluate.
      start: Where to start in the input string.
      delimiter: An optional character to terminate at.  If omitted will
                 continue to the end of the string.
    """

    quotes = ['"', "'"]
    word = ""
    current_quote_char = None
    escaped = False
    idx = -1

    for idx in range(start, len(val)):
        c = val[idx]

        if current_quote_char == None and c == delimiter:
            break

        if current_quote_char == None and c in quotes:  # quote start
            current_quote_char = c
        elif c == current_quote_char and not escaped:  # quote end
            current_quote_char = None
        elif c == "\\" and not escaped:  # handle escape char
            expect(
                current_quote_char != None,
                "escape char outside of quotes at char %d in: %s" % (idx + 1, val),
            )
            escaped = True
        else:
            word += c
            escaped = False

    expect(current_quote_char == None, "quote not closed in: %s" % val)

    return idx, word

def _next_word(val, start, delimiter):
    """
    Advance past delimiter characters.
    """

    for idx in range(start, len(val)):
        c = val[idx]
        if c != delimiter:
            return idx

    return -1

def read(section, field, default = None, root_cell = False):
    """Read a `string` from `.buckconfig`."""

    read_config_func = read_root_config if root_cell else read_config
    return read_config_func(section, field, default)

# Alias for `read` that's explicit about the type being returned.
read_string = read

def read_choice(section, field, choices, default = None, required = True, root_cell = False):
    """Read a string from `.buckconfig` that must be one `choices`."""

    val = read(section, field, root_cell = root_cell)
    if val != None:
        if val in choices:
            return val
        else:
            fail(
                "`{}:{}`: must be one of ({}), but was {}".format(section, field, ", ".join(choices), repr(val)),
            )
    elif default != None:
        return default
    elif not required:
        return None
    else:
        fail("`{}:{}`: no value set".format(section, field))

def read_bool(section, field, default = None, required = True, root_cell = False):
    """Read a `boolean` from `.buckconfig`."""

    # Treat the empty string as "unset".  This allows the user to "override" a
    # previous setting by "clearing" it out.
    val = read(section, field, root_cell = root_cell)
    if val != None and val != "":
        # Fast-path string check
        if val == "True" or val == "true":
            return True
        elif val == "False" or val == "false":
            return False

        # Else fall back to lower casing
        if val.lower() == "true":
            return True
        elif val.lower() == "false":
            return False
        else:
            fail(
                "`{}:{}`: cannot coerce {!r} to bool".format(section, field, val),
            )
    elif default != None:
        return default
    elif not required:
        return None
    else:
        fail("`{}:{}`: no value set".format(section, field))

def read_int(section, field, default = None, required = True, root_cell = False):
    """Read an `int` from `.buckconfig`."""

    val = read(section, field, root_cell = root_cell)
    if val != None:
        if val.isdigit():
            return int(val)
        else:
            fail(
                "`{}:{}`: cannot coerce {!r} to int".format(section, field, val),
            )
    elif default != None:
        return default
    elif not required:
        return None
    else:
        fail("`{}:{}`: no value set".format(section, field))

def read_list(section, field, delimiter = ",", default = None, required = True, root_cell = False):
    """Read a `list` from `.buckconfig`."""
    val = read(section, field, root_cell = root_cell)
    if val != None:
        quotes = ["\\", '"', "'"]
        if lazy.is_any(lambda x: x in val, quotes):
            words = []
            idx = 0
            for _ in range(len(val)):
                idx = _next_word(val, idx, delimiter)
                if idx == -1:
                    break
                idx, word = _decode_raw_word(val, idx, delimiter)
                words.append(word.strip())
                if idx == -1 or idx >= len(val) - 1:
                    break
            return words
        else:
            return [v.strip() for v in val.split(delimiter) if v]
    elif default != None:
        return default
    elif not required:
        return None
    else:
        fail("`{}:{}`: no value set".format(section, field))

def resolve_alias(alias):
    """Resolves an alias into a target (recursively). `fail`s if the alias does
    not exist.

    Args:
        alias (str): The alias or target to resolve.

    Returns:
        The target pointed to by the alias (or the input if the caller lied
        to us and `alias` is a target)
    """
    if "//" in alias:
        return alias

    # Starlark doesn't have while loops or allow recursion, so if we want to
    # resolve aliases that we find we need to iterate somehow
    for _ in range(1000):
        # TODO: set root_cell to true when all aliases come from root cell?
        target = read("alias", alias, root_cell = False)
        expect(target != None, "Alias {} does not exist".format(alias))
        if "//" in target:
            return target
        else:
            alias = target
    fail("This should never happen - either the alias exists or it doesn't")
