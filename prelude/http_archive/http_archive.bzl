# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//utils:utils.bzl", "expect", "value_or")

# Flags to apply to decompress the various types of archives.
_TAR_FLAGS = {
    "tar": [],
    "tar.gz": ["-z"],
    "tar.xz": ["-J"],
    "tar.zst": ["--use-compress-program=unzstd"],
}

_ARCHIVE_EXTS = _TAR_FLAGS.keys() + [
    "zip",
]

def _url_path(url: str.type) -> str.type:
    if "?" in url:
        return url.split("?")[0]
    else:
        return url

def _type_from_url(url: str.type) -> [str.type, None]:
    url_path = _url_path(url)
    for filename_ext in _ARCHIVE_EXTS:
        if url_path.endswith("." + filename_ext):
            return filename_ext
    return None

def _type(ctx: "context") -> str.type:
    url = ctx.attrs.urls[0]
    typ = ctx.attrs.type
    if typ == None:
        typ = value_or(_type_from_url(url), "tar.gz")
    if typ not in _ARCHIVE_EXTS:
        fail("unsupported archive type: {}".format(typ))
    return typ

def _unarchive_cmd(
        ext_type: str.type,
        exec_is_windows: bool.type,
        archive: "artifact",
        strip_prefix: [str.type, None]) -> "cmd_args":
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
                "tar",
                "-x",
                "-f",
                "-",
                _tar_strip_prefix_flags(strip_prefix),
            )
        elif ext_type == "zip":
            # unzip and zip are not cli commands available on windows. however, the
            # bsdtar that ships with windows has builtin support for zip
            return cmd_args(
                "tar",
                "-x",
                "-f",
                archive,
                _tar_strip_prefix_flags(strip_prefix),
            )

        # Else hope for the best

    if ext_type in _TAR_FLAGS:
        return cmd_args(
            "tar",
            _TAR_FLAGS[ext_type],
            "-x",
            "-f",
            archive,
            _tar_strip_prefix_flags(strip_prefix),
        )
    elif ext_type == "zip":
        if strip_prefix:
            fail("`strip_prefix` for zip is only supported on windows")

        # gnutar does not intrinsically support zip
        return cmd_args(archive, format = "unzip {}")
    else:
        fail()

def _tar_strip_prefix_flags(strip_prefix: [str.type, None]) -> [str.type]:
    if strip_prefix:
        # count nonempty path components in the prefix
        count = len(filter(lambda c: c != "", strip_prefix.split("/")))
        return ["--strip-components=" + str(count), strip_prefix]
    return []

def http_archive_impl(ctx: "context") -> ["provider"]:
    expect(len(ctx.attrs.urls) == 1, "multiple `urls` not support: {}".format(ctx.attrs.urls))

    # The HTTP download is local so it makes little sense to run actions
    # remotely, unless we can defer them.
    local_only = ctx.attrs.sha1 == None

    ext_type = _type(ctx)

    exec_is_windows = ctx.attrs._exec_os_type[OsLookup].platform == "windows"

    # Download archive.
    archive = ctx.actions.declare_output("archive." + ext_type)
    url = ctx.attrs.urls[0]
    ctx.actions.download_file(archive.as_output(), url, sha1 = ctx.attrs.sha1, sha256 = ctx.attrs.sha256, is_deferrable = True)

    # Unpack archive to output directory.
    exclude_flags = []
    exclude_hidden = []
    if ctx.attrs.excludes:
        tar_flags = _TAR_FLAGS.get(ext_type)
        expect(tar_flags != None, "excludes not supported for non-tar archives")

        # Tar excludes files using globs, but we take regexes, so we need to
        # apply our regexes onto the file listing and produce an exclusion list
        # that just has strings.
        exclusions = ctx.actions.declare_output("exclusions")
        create_exclusion_list = [
            ctx.attrs._create_exclusion_list[RunInfo],
            "--tar-archive",
            archive,
            cmd_args(tar_flags, format = "--tar-flag={}"),
            "--out",
            exclusions.as_output(),
        ]
        for exclusion in ctx.attrs.excludes:
            create_exclusion_list.append(cmd_args(exclusion, format = "--exclude={}"))

        ctx.actions.run(create_exclusion_list, category = "process_exclusions", local_only = local_only)
        exclude_flags.append(cmd_args(exclusions, format = "--exclude-from={}"))
        exclude_hidden.append(exclusions)

    output = ctx.actions.declare_output(value_or(ctx.attrs.out, ctx.label.name), dir = True)

    if exec_is_windows:
        ext = "bat"
        mkdir = "md {}"
        interpreter = []
    else:
        ext = "sh"
        mkdir = "mkdir -p {}"
        interpreter = ["/bin/sh"]

    unarchive_cmd = _unarchive_cmd(ext_type, exec_is_windows, archive, ctx.attrs.strip_prefix)
    script, _ = ctx.actions.write(
        "unpack.{}".format(ext),
        [
            cmd_args(output, format = mkdir),
            cmd_args(output, format = "cd {}"),
            cmd_args([unarchive_cmd] + exclude_flags, delimiter = " ").relative_to(output),
        ],
        is_executable = True,
        allow_args = True,
    )

    ctx.actions.run(
        cmd_args(interpreter + [script]).hidden(exclude_hidden + [archive, output.as_output()]),
        category = "http_archive",
        local_only = local_only,
    )

    return [DefaultInfo(default_output = output)]
