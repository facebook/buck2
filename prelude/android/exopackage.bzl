SECONDARY_DEX = 1
NATIVE_LIBRARY = 2
RESOURCES = 4
MODULES = 8
ARCH64 = 16

def get_exopackage_flags(exopackage_modes: [str.type]) -> int.type:
    flags = 0

    for (name, flag) in [
        ("secondary_dex", SECONDARY_DEX),
        ("native_library", NATIVE_LIBRARY),
        ("resources", RESOURCES),
        ("modules", MODULES),
        ("arch64", ARCH64),
    ]:
        if name in exopackage_modes:
            flags += flag

    return flags
