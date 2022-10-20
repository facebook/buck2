import pathlib
import sys

re_worker_path = "/run/re_worker/beacon"
if pathlib.Path(re_worker_path).exists():
    print("Failed on RE")
else:
    print("Failed on local")

sys.exit(1)
