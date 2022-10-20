prebuilt_cxx_library(
    name = "openssl",
    exported_headers = glob(["include/**/*.h"]),
    header_dirs = ["include"],
    deps = [":ssl"],
    visibility = ["PUBLIC"],
)

prebuilt_cxx_library(
    name = "ssl",
    static_lib = "lib/x86_64-linux-gnu/libssl.a",
    deps = [":crypto"],
)

prebuilt_cxx_library(
    name = "crypto",
    exported_post_linker_flags = ["-lc"],
    static_lib = "lib/x86_64-linux-gnu/libcrypto.a",
)
