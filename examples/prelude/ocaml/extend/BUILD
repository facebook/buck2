ocaml_binary(
    name = "hello",
    srcs = [
        "hello.ml",
        "hello_stubs.c",
    ],
) if not host_info().os.is_windows else None
