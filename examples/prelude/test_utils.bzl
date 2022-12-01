def assert_output(name, command, output):
    return native.genrule(
        name = name,
        bash = command + " | grep \"" + output + "\" && touch \"$OUT\"",
        out = "out.txt",
    )
