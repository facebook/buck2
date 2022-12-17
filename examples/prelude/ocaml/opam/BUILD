# opam_switch_prefix = read_config("ocaml", "opam_switch_prefix", None)

prebuilt_cxx_library(
    name = "ocaml-dev",
    header_only = True,
    header_dirs = ["ocaml"],
    visibility = ["PUBLIC"],
) if not host_info().os.is_windows else None
