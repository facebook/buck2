# buildifier: disable=no-effect
ocaml_binary(
    name = "calc",
    srcs = [
        "calc.ml",
        "lexer.mll",
        "parser.mly",
    ],
) if not host_info().os.is_windows else None
