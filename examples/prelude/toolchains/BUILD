load("@prelude//toolchains/conan:defs.bzl", "conan_init", "conan_profile", "system_conan_toolchain")
load("@prelude//toolchains:cxx.bzl", "system_cxx_toolchain")
load("@prelude//toolchains:ocaml.bzl", "system_ocaml_toolchain")
load("@prelude//toolchains:python.bzl", "system_python_bootstrap_toolchain", "system_python_toolchain")
load("@prelude//toolchains:rust.bzl", "system_rust_toolchain")

system_conan_toolchain(
    name = "conan",
    conan_path = "conan",
    visibility = ["PUBLIC"],
)

conan_profile(
    name = "conan-profile-linux",
    arch = "x86_64",
    os = "Linux" ,
    build_type = "Release",
    compiler = "gcc",
    compiler_version = "11.3",
    compiler_libcxx = "libstdc++",
)

conan_profile(
    name = "conan-profile-macos",
    arch = "x86_64",
    os = "Macos" ,
    build_type = "Release",
    compiler = "clang",
    compiler_version = "15",
    compiler_libcxx = "libc++",
)

conan_profile(
    name = "conan-profile-windows",
    arch = "x86_64",
    os = "Windows" ,
    build_type = "Release",
    compiler = "gcc",
    compiler_version = "11.3",
    compiler_libcxx = "libstdc++",
)

alias(
    name = "conan-profile",
    actual = select({
        "prelude//os:linux": ":conan-profile-linux",
        "prelude//os:macos": ":conan-profile-macos",
        "prelude//os:windows": ":conan-profile-windows",
    }),
)

conan_init(
    name = "conan-init",
    profile = ":conan-profile",
    visibility = ["PUBLIC"],
)

system_cxx_toolchain(
    name = "cxx",
    visibility = ["PUBLIC"],
)

system_ocaml_toolchain(
    name = "ocaml",
    visibility = ["PUBLIC"],
)

system_python_toolchain(
    name = "python",
    visibility = ["PUBLIC"],
)

system_python_bootstrap_toolchain(
    name = "python_bootstrap",
    visibility = ["PUBLIC"],
)

system_rust_toolchain(
    name = "rust",
    default_edition = "2021",
    visibility = ["PUBLIC"],
)
