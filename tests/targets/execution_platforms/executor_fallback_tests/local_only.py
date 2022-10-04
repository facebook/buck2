import pathlib
import sys

re_worker_path = "/run/re_worker/beacon"
if pathlib.Path(re_worker_path).exists():
    print("This only runs on local", file=sys.stderr)
    sys.exit(1)

out = sys.argv[1]
pathlib.Path(out).touch()
