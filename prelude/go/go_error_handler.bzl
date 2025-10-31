# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def go_build_error_handler(ctx: ActionErrorCtx) -> list[ActionSubError]:
    structured_errors = ctx.parse_with_errorformat(
        category = "go_compile_error",
        error = ctx.stdout,
        # reference: https://github.com/fatih/vim-go/blob/06ac99359b0b1a7de1e213447d92fd0a46cb4cd0/compiler/go.vim#L34-L40
        errorformats = [
            "%-G#\\ %.%#",  # Ignore lines beginning with '#' ('# command-line-arguments' line sometimes appears?)
            "%-G%.%#panic:\\ %m",  # Ignore lines containing 'panic: message'
            "%Ecan't\\ load\\ package:\\ %m",  # Start of multiline error string is 'can\'t load package'
            "%A\\(\\[\\^:]\\+:\\ \\)\\?%f:%l:%c:\\ %m",  # Start of multiline unspecified string is 'filename:linenumber:columnnumber:'
            "%A\\(\\[\\^:]\\+:\\ \\)\\?%f:%l:\\ %m",  # Start of multiline unspecified string is 'filename:linenumber:'
            "%C%*\\s%m",  # Continuation of multiline error message is indented
            "%-G%.%#",  # All lines not matching any of the above patterns are ignored
        ],
    )
    return structured_errors
