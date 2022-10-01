# Styles of LTO.
LtoMode = enum(
    # No LTO
    "none",
    # Object files contain both LTO IR and native code to allow binaries to link
    # either via standard or LTO.
    "fat",
    # Traditional, monolithic LTO.
    "monolithic",
    # https://clang.llvm.org/docs/ThinLTO.html
    "thin",
)
