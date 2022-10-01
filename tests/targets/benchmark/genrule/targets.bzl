prelude = native

def targets(n):
    for i in range(n):
        prelude.genrule(
            name = "{}".format(i),
            srcs = ["src.txt"],
            out = "out{}".format(i),
            bash = "echo HELLO > $OUT",
        )
