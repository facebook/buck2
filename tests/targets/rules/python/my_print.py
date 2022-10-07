import buck2.tests.targets.rules.python.my_strings as my_strings


def my_print(s):
    print(s)


if __name__ == "__main__":
    my_print(my_strings.HELLO_WORLD)
