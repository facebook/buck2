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

# TODO[AH] Prevent double build of packages.
#   This rule builds or fetches all transitive dependencies defined by the
#   conanfile in order to generate the import targets. These packages will
#   later be built or fetched again one-by-one again as their corresponding
#   conan_package targets. Avoid this double building. Mark this target manual,
#   or don't tie the generation to it's default output.
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
    deps = ["//cpp/conan/import:zlib"],
)

assert_output(
    name = "check_main",
    command = "$(exe_target :main)",
    output = "395248644",
)
