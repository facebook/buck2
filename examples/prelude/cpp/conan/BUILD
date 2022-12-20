load(
    "@prelude//toolchains/conan:defs.bzl",
    "conan_generate",
    "conan_lock",
    "conan_update",
    "lock_generate",
)
load("//test_utils.bzl", "assert_output")

conan_lock(
    name = "lock",
    conanfile = "conanfile.txt",
    visibility = ["//cpp/conan/import:"],
)

lock_generate(
    name = "lock-generate",
    lockfile = ":lock",
)

conan_generate(
    name = "conan-generate",
    conanfile = "conanfile.txt",
    lockfile = ":lock",
)

conan_update(
    name = "update",
    lockfile = ":lock",
    lock_generate = ":lock-generate",
    conan_generate = ":conan-generate",
    conanfile = "conanfile.txt",
    lockfile_name = "conan.lock",
    targets_name = "import/BUILD",
)

cxx_binary(
    name = "main",
    link_style = "static",
    srcs = ["main.cpp"],
) if not host_info().os.is_windows else None

assert_output(
    name = "check_main",
    command = "$(exe_target :main)",
    output = "Hello Conan",
) if not host_info().os.is_windows else None

