# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


import os
import pathlib
import platform
import re
import shlex
import subprocess
import sys
import tempfile
import zipfile
from enum import Enum
from shutil import copyfile
from typing import Callable, List, Match, Pattern
from urllib.parse import urlencode

# ANSI color codes
RED = "\033[91m"
YELLOW = "\033[93m"
GREEN = "\033[92m"
MAGENTA = "\033[95m"
BLUE = "\033[94m"
BOLD = "\033[1m"
RESET = "\033[0m"

ERROR_PATTERN = re.compile(
    r"\berror\b(:.*?\n)((?:.*?\n)+?\s*)(\^+)\n", flags=re.IGNORECASE
)
WARNING_PATTERN = re.compile(r"\bwarning\b(:.*?)\n", flags=re.IGNORECASE)

JAVA_FILE_PATTERN = re.compile(
    r"(?:\x1b\[[0-9;]*m)*(/?(?:[\w-]+/)*[\w-]+\.java)\b:(\d+):"
)

KOTLIN_FILE_PATTERN = re.compile(
    r"(?:\x1b\[[0-9;]*m)*(/?(?:[\w-]+/)*[\w-]+\.kt)\b:(\d+):(\d+):"
)

# Source: https://docs.oracle.com/javase/tutorial/java/nutsandbolts/_keywords.html
JAVA_KEYWORDS = [
    "abstract",
    "assert",
    "boolean",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extends",
    "final",
    "finally",
    "float",
    "for",
    "goto",
    "if",
    "implements",
    "import",
    "instanceof",
    "int",
    "interface",
    "long",
    "native",
    "new",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "short",
    "static",
    "strictfp",
    "super",
    "switch",
    "synchronized",
    "this",
    "throw",
    "throws",
    "transient",
    "try",
    "void",
    "volatile",
    "while",
    "null",
    "true",
    "false",
    "record",
    "sealed",
    "permits",
    "non-sealed",
    "var",
]

# Source: https://kotlinlang.org/docs/keyword-reference.html
KOTLIN_KEYWORDS = [
    "as",
    "as?",
    "break",
    "class",
    "continue",
    "do",
    "else",
    "false",
    "for",
    "fun",
    "if",
    "in",
    "!in",
    "interface",
    "is",
    "!is",
    "null",
    "object",
    "package",
    "return",
    "super",
    "this",
    "throw",
    "true",
    "try",
    "typealias",
    "val",
    "var",
    "when",
    "while",
    "by",
    "catch",
    "constructor",
    "delegate",
    "dynamic",
    "field",
    "file",
    "finally",
    "get",
    "import",
    "init",
    "param",
    "property",
    "receiver",
    "set",
    "setparam",
    "where",
    "actual",
    "abstract",
    "annotation",
    "companion",
    "const",
    "crossinline",
    "data",
    "enum",
    "expect",
    "external",
    "final",
    "infix",
    "inline",
    "inner",
    "internal",
    "lateinit",
    "noinline",
    "open",
    "operator",
    "out",
    "override",
    "private",
    "protected",
    "public",
    "reified",
    "sealed",
    "suspend",
    "tailrec",
    "vararg",
    "field",
    "it",
]


class FileType(Enum):
    """Enum representing supported file types with their associated patterns and keywords."""

    JAVA = (
        JAVA_KEYWORDS,
        # File pattern for Java files
        JAVA_FILE_PATTERN,
        "JAVA",
    )

    KOTLIN = (
        KOTLIN_KEYWORDS,
        # File pattern for Kotlin files
        KOTLIN_FILE_PATTERN,
        "KOTLIN",
    )

    def __init__(self, keywords: List[str], file_pattern: Pattern, lang: str):
        self.keywords = keywords
        self.file_pattern = file_pattern
        self.lang = lang


def _hyperlink(file: str, line: int, text: str) -> str:
    """
    Creates a clickable hyperlink in the terminal using OSC 8 escape sequences.
    Supports both VS Code and Android Studio links based on ANDROID_EDITOR environment variable.

    Args:
        file: The file path to link to
        line: The line number to link to
        text: The text to display as the hyperlink

    Returns:
        A string containing the hyperlinked text with terminal escape sequences
    """
    # Keep in sync with fbcode/buck2/prelude/toolchains/android/src/com/facebook/buck/jvm/cd/ErrorInterceptor.java
    isVsCode = (
        os.environ.get("FBVSCODE_REMOTE_ENV_NAME") == "od"
        or os.environ.get("TERM_PROGRAM") == "vscode"
    )

    isHyperlinkDisabled = os.path.exists(
        os.path.expanduser("~/.disable_buck_jvm_path_hyperlink")
    )

    if isVsCode or isHyperlinkDisabled:
        return text

    OSC = "\033]"
    ST = "\033\\"

    is_jetbrains = (
        "ANDROID_EDITOR" in os.environ
        or pathlib.Path("~/.jetbrains-fb/.buck_path_hyperlink_uses_jetbrains")
        .expanduser()
        .is_file()
    )

    if is_jetbrains:
        params = {
            "ide": "intellij",
            "filepath": f"/fbsource/{file}",
            "line": line,
        }
        encoded_params = "&".join(
            [
                f"{key}={urlencode({key: str(value)}).split('=', 1)[1]}"
                for key, value in params.items()
            ]
        )

        uri = f"fb-ide-opener://open/?{encoded_params}"

    else:
        params = {
            "project": "fbsource",
            "paths[0]": file,
            "lines[0]": line,
        }

        encoded_params = urlencode(params)

        uri = f"https://www.internalfb.com/intern/nuclide/open/arc/?{encoded_params}"

    return f"{OSC}8;;{uri}{ST}{text}{OSC}8;;{ST}"


def _highlight_code(s: str, keywords: List[str]) -> str:
    if not keywords:
        return s
    return re.sub(
        rf"\b({'|'.join(keywords)})\b",
        lambda match: f"{BLUE}{match.group(1)}{RESET}",
        s,
    )


def _highlight_error(match: Match, keywords: List[str]) -> str:
    return (
        f"{RED}error{RESET}{BOLD}{match.group(1)}{RESET}"
        f"{_highlight_code(match.group(2), keywords)}{RED}{match.group(3)}{RESET}\n"
    )


def _highlight_warning(match: Match) -> str:
    return f"{YELLOW}warning{RESET}{BOLD}{match.group(1)}{RESET}\n"


def _highlight_java_file(match: Match) -> str:
    file = match.group(1)
    line = int(match.group(2))

    return f"{GREEN}{_hyperlink(file, line, file)}{RESET}:{MAGENTA}{match.group(2)}{RESET}:"


def _highlight_kotlin_file(match: Match) -> str:
    file = match.group(1)
    line = int(match.group(2))

    return (
        f"{GREEN}{_hyperlink(file, line, file)}{RESET}:"
        f"{MAGENTA}{match.group(2)}{RESET}:{MAGENTA}{match.group(3)}{RESET}:"
    )


def _colorize_pattern(
    pattern: Pattern, message: str, colorizer: Callable[[Match], str]
) -> str:
    """
    Apply a colorizer function to all matches of a pattern in a string.

    Args:
        pattern: The pattern to match
        message: The string to search
        colorizer: The function to apply to each match

    Returns:
        The colorized string
    """
    if not pattern:
        return message

    result = ""
    last_end = 0
    for match in pattern.finditer(message):
        result += message[last_end : match.start()]
        result += colorizer(match)
        last_end = match.end()
    result += message[last_end:]
    return result


def pretty_exception(e: str, file_type: FileType = None) -> str:
    """
    Format compiler error messages with syntax highlighting and clickable file links.

    Args:
        e: The error message to format
        lang_type: The language type (if known, otherwise will be auto-detected)

    Returns:
        A formatted error message with ANSI color codes
    """

    if "NO_COLOR" in os.environ or (
        file_type.name and f"NO_{file_type.name}C_COLOR" in os.environ
    ):
        return e

    # Process error messages
    e = _colorize_pattern(
        ERROR_PATTERN, e, lambda match: _highlight_error(match, file_type.keywords)
    )

    # Process warning messages
    e = _colorize_pattern(WARNING_PATTERN, e, _highlight_warning)

    # Process file references
    if file_type == FileType.JAVA:
        e = _colorize_pattern(file_type.file_pattern, e, _highlight_java_file)
    elif file_type == FileType.KOTLIN:
        e = _colorize_pattern(file_type.file_pattern, e, _highlight_kotlin_file)

    return e


def pretty_exception_j(e: str) -> str:
    return pretty_exception(e, FileType.JAVA)


def pretty_exception_k(e: str) -> str:
    return pretty_exception(e, FileType.KOTLIN)


def file_name_matches(file_name: str, extensions: List[str]) -> bool:
    for extension in extensions:
        if file_name.endswith(extension):
            return True
    return False


def extract_source_files(
    zipped_sources_file: pathlib.Path,
    args_file: pathlib.Path,
    file_name_extensions: List[str],
    temp_dir: tempfile.TemporaryDirectory,
) -> pathlib.Path:
    extracted_zip_dir = os.path.join(temp_dir, "extracted_srcs")
    all_extracted_files = []

    with open(zipped_sources_file) as file:
        zip_file_paths = [line.rstrip() for line in file.readlines()]
        for zip_file_path in zip_file_paths:
            with zipfile.ZipFile(zip_file_path, "r") as zip_file:
                files_to_extract = []
                for file_name in zip_file.namelist():
                    if file_name_matches(file_name, file_name_extensions):
                        files_to_extract.append(file_name)
                zip_file.extractall(path=extracted_zip_dir, members=files_to_extract)
                all_extracted_files += files_to_extract

    # append args file with new extracted sources
    merged_args_file = os.path.join(temp_dir, "merged_args_file")
    # copy content from args file
    copyfile(args_file, merged_args_file)

    with open(merged_args_file, "a") as merged_file:
        # append with extracted paths
        for path in all_extracted_files:
            merged_file.write(
                "{}{}".format(os.linesep, os.path.join(extracted_zip_dir, path))
            )
    return merged_args_file


def _to_class_name(path: pathlib.Path) -> str:
    return str(path).replace(os.sep, ".").replace(".class", "")


def sources_are_present(
    args_file: pathlib.Path, permitted_extensions: List[str]
) -> bool:
    with open(args_file, "r") as file:
        for line in file.readlines():
            for extension in permitted_extensions:
                if line.strip().endswith(extension):
                    return True
    return False


def shlex_split(cmd: str) -> List[str]:
    if platform.system() == "Windows":
        # Windows shlex.split removes backslashes.
        return cmd.split()
    else:
        return shlex.split(cmd)


def log_message(message: str):
    level = "debug"

    main_file = os.path.realpath(sys.argv[0]) if sys.argv[0] else None
    if main_file:
        program_name = os.path.basename(main_file)
    else:
        program_name = ""

    print(
        "{}[{}] {}".format(
            "[{}] ".format(program_name) if program_name else "", level, message
        ),
        file=sys.stderr,
    )


def execute_command(command: List):
    log_message(
        "executing command = '{}'".format(
            " ".join([shlex.quote(str(s)) for s in command])
        )
    )
    exit_code = subprocess.call(command)
    if exit_code != 0:
        sys.exit(exit_code)


def execute_command_ignore_exit_codes(command: List, exit_codes_to_ignore: List):
    log_message(
        "executing command = '{}'".format(
            " ".join([shlex.quote(str(s)) for s in command])
        )
    )
    exit_code = subprocess.call(command)
    if exit_code != 0 and exit_code not in exit_codes_to_ignore:
        sys.exit(exit_code)
