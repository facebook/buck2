# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup")
load("@prelude//utils:expect.bzl", "expect")
load("@prelude//utils:utils.bzl", "value_or")
load(":exec_deps.bzl", "HttpArchiveExecDeps")

# Flags to apply to decompress the various types of archives.
_TAR_FLAGS = {
    "tar": [],
    "tar.bz2": ["-j"],
    "tar.gz": ["-z"],
    "tar.xz": ["-J"],
    "tar.zst": ["--use-compress-program=unzstd"],
}

_ARCHIVE_EXTS = _TAR_FLAGS.keys() + [
    "zip",
]

def _url_path(url: str) -> str:
    if "?" in url:
        return url.split("?")[0]
    else:
        return url

def _type_from_url(url: str) -> [str, None]:
    url_path = _url_path(url)
    for filename_ext in _ARCHIVE_EXTS:
        if url_path.endswith("." + filename_ext):
            return filename_ext
    return None

def archive_type(url_or_path: str, typ: str | None) -> str:
    if typ == None:
        typ = value_or(_type_from_url(url_or_path), "tar.gz")
    if typ not in _ARCHIVE_EXTS:
        fail("unsupported archive type: {}".format(typ))
    return typ

# Returns a two-element tuple:
#
# 1. The cmd_args with the unarchive command
# 2. A bool indicating whether the prefix still needs to be stripped (in cases where the tool used to uncompress does not support this feature).
def _unarchive_cmd(
        ext_type: str,
        exec_is_windows: bool,
        archive: Artifact,
        strip_prefix: [str, None]) -> (cmd_args, bool):
    if exec_is_windows:
        # So many hacks.
        if ext_type == "tar.zst":
            # tar that ships with windows is bsdtar
            # bsdtar seems to not properly interop with zstd and hangs instead of
            # exiting with an error. Manually decompressing with zstd and piping to
            # tar seems to work fine though.
            return cmd_args(
                "zstd",
                "-d",
                archive,
                "--stdout",
                "|",
                "%WINDIR%\\System32\\tar.exe",
                "-x",
                "-P",
                "-f",
                "-",
                _tar_strip_prefix_flags(strip_prefix),
            ), False
        elif ext_type == "zip":
            # unzip and zip are not cli commands available on windows. however, the
            # bsdtar that ships with windows has builtin support for zip
            return cmd_args(
                "%WINDIR%\\System32\\tar.exe",
                "-x",
                "-P",
                "-f",
                archive,
                _tar_strip_prefix_flags(strip_prefix),
            ), False

        # Else hope for the best

    if ext_type in _TAR_FLAGS:
        os_flags = [
            # buck-out is a symlink with EdenFS, and tar on Windows doesn't like it,
            # and needs -P flag to allow operations with symlinks
            "-P",
        ] if exec_is_windows else []
        return cmd_args(
            "tar",
            _TAR_FLAGS[ext_type],
            os_flags,
            "-x",
            "-f",
            archive,
            _tar_strip_prefix_flags(strip_prefix),
        ), False
    elif ext_type == "zip":
        # gnutar does not intrinsically support zip
        return cmd_args(archive, format = "unzip {}"), bool(strip_prefix)
    else:
        fail()

def _tar_strip_prefix_flags(strip_prefix: [str, None]) -> list[str]:
    if strip_prefix:
        # count nonempty path components in the prefix
        count = len(filter(lambda c: c != "", strip_prefix.split("/")))
        return ["--strip-components=" + str(count), strip_prefix]
    return []

def unarchive(
        ctx: AnalysisContext,
        archive: Artifact,
        output_name: str,
        ext_type,
        excludes,
        strip_prefix,
        exec_deps: HttpArchiveExecDeps,
        prefer_local: bool,
        sub_targets: list[str] | dict[str, list[str]]):
    exec_is_windows = exec_deps.exec_os_type[OsLookup].os == Os("windows")

    # Unpack archive to output directory.
    exclude_flags = []
    exclude_hidden = []
    if excludes:
        tar_flags = _TAR_FLAGS.get(ext_type)
        expect(tar_flags != None, "excludes not supported for non-tar archives")

        # Tar excludes files using globs, but we take regexes, so we need to
        # apply our regexes onto the file listing and produce an exclusion list
        # that just has strings.
        exclusions = ctx.actions.declare_output("exclusions")
        create_exclusion_list = [
            exec_deps.create_exclusion_list[RunInfo],
            "--tar-archive",
            archive,
            cmd_args(tar_flags, format = "--tar-flag={}"),
            "--out",
            exclusions.as_output(),
        ]
        for exclusion in excludes:
            create_exclusion_list.append(cmd_args(exclusion, format = "--exclude={}"))

        ctx.actions.run(create_exclusion_list, category = "process_exclusions", prefer_local = prefer_local)
        exclude_flags.append(cmd_args(exclusions, format = "--exclude-from={}"))
        exclude_hidden.append(exclusions)

    if exec_is_windows:
        ext = "bat"
        mkdir = "md {}"
        interpreter = []
    else:
        ext = "sh"
        mkdir = "mkdir -p {}"
        interpreter = ["/bin/sh"]

    unarchive_cmd, needs_strip_prefix = _unarchive_cmd(ext_type, exec_is_windows, archive, strip_prefix)

    output = ctx.actions.declare_output(output_name, dir = True)
    script_output = ctx.actions.declare_output(output_name + "_tmp", dir = True) if needs_strip_prefix else output

    script, _ = ctx.actions.write(
        "unpack.{}".format(ext),
        [
            cmd_args(script_output, format = mkdir),
            cmd_args(script_output, format = "cd {}"),
            cmd_args([unarchive_cmd] + exclude_flags, delimiter = " ", relative_to = script_output),
        ],
        is_executable = True,
        allow_args = True,
    )

    ctx.actions.run(
        cmd_args(
            interpreter + [script],
            hidden = exclude_hidden + [archive, script_output.as_output()],
        ),
        category = "http_archive",
        prefer_local = prefer_local,
    )

    if needs_strip_prefix:
        ctx.actions.copy_dir(output.as_output(), script_output.project(strip_prefix))

    if type(sub_targets) == type([]):
        sub_targets = {
            path: [DefaultInfo(default_output = output.project(path))]
            for path in sub_targets
        }
    elif type(sub_targets) == type({}):
        sub_targets = {
            name: [DefaultInfo(default_outputs = [output.project(path) for path in paths])]
            for name, paths in sub_targets.items()
        }
    else:
        fail("sub_targets must be a list or dict")

    return output, sub_targets
