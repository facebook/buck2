from libfb.py import parutil

path = parutil.get_file_path("foo.txt")
with open(path) as f:
    print(f.read())
