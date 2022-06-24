GoToolchainInfo = provider(fields = [
    "assembler",
    "compile_wrapper",
    "compiler",
    "external_linker_flags",
    "filter_srcs",
    "go",
    "env_go_arch",
    "env_go_os",
    "env_go_root",
    "linker",
    "packer",
])

def get_toolchain_cmd_args(toolchain: "GoToolchainInfo", go_root = True) -> "cmd_args":
    cmd = cmd_args("env")
    if toolchain.env_go_arch != None:
        cmd.add("GOARCH={}".format(toolchain.env_go_arch))
    if toolchain.env_go_os != None:
        cmd.add("GOOS={}".format(toolchain.env_go_os))
    if go_root and toolchain.env_go_root != None:
        cmd.add(cmd_args(toolchain.env_go_root, format = "GOROOT={}"))
    return cmd
