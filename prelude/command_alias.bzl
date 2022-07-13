def command_alias_impl(ctx):
    if ctx.attrs.exe == None:
        base = RunInfo()
    else:
        base = _get_run_info_from_exe(ctx.attrs.exe)

    run_info_args = cmd_args()

    if len(ctx.attrs.env) > 0 or len(ctx.attrs.platform_exe.items()) > 0:
        trampoline_args = cmd_args()
        trampoline_args.add("#!/bin/sh")
        trampoline_args.add("set -euo pipefail")
        trampoline_args.add('BUCK_COMMAND_ALIAS_ABSOLUTE=$(cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P)')

        for (k, v) in ctx.attrs.env.items():
            # TODO(akozhevnikov): maybe check environment variable is not conflicting with pre-existing one
            trampoline_args.add(cmd_args(["export ", k, "=", v], delimiter = ""))

        if len(ctx.attrs.platform_exe.items()) > 0:
            trampoline_args.add('case "$(uname)" in')
            for platform, exe in ctx.attrs.platform_exe.items():
                # Only linux and macos are supported.
                if platform == "linux":
                    _add_platform_case_to_trampoline_args(trampoline_args, "Linux", _get_run_info_from_exe(exe), ctx.attrs.args)
                elif platform == "macos":
                    _add_platform_case_to_trampoline_args(trampoline_args, "Darwin", _get_run_info_from_exe(exe), ctx.attrs.args)

            # Default case
            _add_platform_case_to_trampoline_args(trampoline_args, "*", base, ctx.attrs.args)
            trampoline_args.add("esac")
        else:
            _add_args_declaration_to_trampoline_args(trampoline_args, base, ctx.attrs.args)

        trampoline_args.add('"${ARGS[@]}"')

        # FIXME(ndmitchell): more straightforward relativization with better API
        non_materialized_reference = ctx.actions.write("dummy", "")
        trampoline_args.relative_to(non_materialized_reference, parent = 1).absolute_prefix("__BUCK_COMMAND_ALIAS_ABSOLUTE__/")

        trampoline_tmp, deps = ctx.actions.write("__command_alias_trampoline.sh.pre", trampoline_args, allow_args = True)

        # FIXME (T111687922): Avert your eyes... We want to add
        # $BUCK_COMMAND_ALIAS_ABSOLUTE a prefix on all the args we include, but
        # those args will be shell-quoted (so that they might include e.g.
        # spaces). However, our shell-quoting will apply to the absolute_prefix
        # as well, which will render it inoperable. To fix this, we emit
        # __BUCK_COMMAND_ALIAS_ABSOLUTE__ instead, and then we use sed to work
        # around our own quoting to produce the thing we want.
        # NOTE: We do this with local_only = True since the only input is a
        # file we just wrote locally.
        trampoline = ctx.actions.declare_output("__command_alias_trampoline.sh")
        ctx.actions.run([
            "sh",
            "-c",
            "sed \"s|__BUCK_COMMAND_ALIAS_ABSOLUTE__|\\$BUCK_COMMAND_ALIAS_ABSOLUTE|g\" < \"$1\" > \"$2\" && chmod +x $2",
            "--",
            trampoline_tmp,
            trampoline.as_output(),
        ], category = "sed", local_only = True)

        run_info_args.add(trampoline)
        run_info_args.hidden([trampoline_args, deps])
    else:
        run_info_args.add(base.args)
        run_info_args.add(ctx.attrs.args)

    run_info_args.hidden(ctx.attrs.resources)

    # TODO(cjhopman): Consider what this should have for default outputs. Using
    # the base's default outputs may not really be correct (it makes more sense to
    # be the outputs required by the args).
    return [
        DefaultInfo(),
        RunInfo(args = run_info_args),
    ]

def _add_platform_case_to_trampoline_args(trampoline_args: "cmd_args", platform_name: str.type, base: RunInfo.type, args: ["_arglike"]):
    trampoline_args.add("    {})".format(platform_name))
    _add_args_declaration_to_trampoline_args(trampoline_args, base, args)
    trampoline_args.add("        ;;")

def _add_args_declaration_to_trampoline_args(trampoline_args: "cmd_args", base: RunInfo.type, args: ["_arglike"]):
    trampoline_args.add("ARGS=(")

    # FIXME (T111687922): We cannot preserve BUCK_COMMAND_ALIAS_ABSOLUTE *and*
    # quote here...  So we don't quote the exe's RunInfo (which usually has a
    # path and hopefully no args that need quoting), but we quote the args
    # themselves (which usually are literals that might need quoting and
    # hopefully doesn't contain relative paths).
    # FIXME (T111687922): Note that we have no shot at quoting base.args anyway
    # at this time, since we need to quote the individual words, but using
    # `quote = "shell"` would just quote the whole thing into one big word.
    trampoline_args.add(base.args)
    for arg in args:
        trampoline_args.add(cmd_args(arg, quote = "shell"))

    # Add the args passed to the command_alias itself.
    trampoline_args.add('"$@"')

    trampoline_args.add(")")

def _get_run_info_from_exe(exe: "dependency") -> RunInfo.type:
    run_info = exe[RunInfo]
    if run_info == None:
        run_info = RunInfo(
            args = exe[DefaultInfo].default_outputs,
        )

    return run_info
