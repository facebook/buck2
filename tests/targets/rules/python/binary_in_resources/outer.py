import subprocess

from libfb.py import parutil

path = parutil.get_file_path("inner.par")
subprocess.check_call([path])
