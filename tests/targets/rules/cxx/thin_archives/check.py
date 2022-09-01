import subprocess
import sys

# First, make sure we have a thin archive.
with open(sys.argv[1], mode="rb") as f:
    header = f.read(7)
    if header != b"!<thin>":
        raise Exception("missing thin archive header: {!r}".format(header))


# Then just list it -- this will fail if the referenced objects aren't also
# available.
subprocess.check_call(["ar", "t", sys.argv[1]])
