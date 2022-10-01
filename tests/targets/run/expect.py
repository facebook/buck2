import sys


def main():
    want = "test"
    got = sys.stdin.read()
    assert want == got, "Wanted {}, got {}".format(repr(want), repr(got))


if __name__ == "__main__":
    main()
