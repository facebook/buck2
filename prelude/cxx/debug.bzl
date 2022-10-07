# Model the various "split" debug scenarios (e.g. `-gsplit-dwarf`).
SplitDebugMode = enum(
    # Debug info, if present, is inline in the object file, and will be linked
    # into executables and shared libraries (e.g. traditional behavior when
    # using `-g`).
    "none",
    # Debug info. if present is included in the object file, but will *not* be
    # linked into executables and shared libraries.  This style usually requires
    # an additional step, separate from the link, to combine and package debug
    # info (e.g. `dSYM`, `dwp`).
    "single",
    # FIXME(agallagher): Add support for "split", which probably just requires
    # modifying `compile_cxx` to add a `.dwo` file as a hidden output in this
    # case.
    #"split",
)
