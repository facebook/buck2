# We use a fixed module cache location. This works around issues with
# multi-user setups with MobileOnDemand and allows us to share the
# module cache with Xcode, LLDB and arc focus.
#
# It's also necessary for Swift on RE because, by default, it will
# try to write to $HOME which might be read-only.
MODULE_CACHE_PATH = "/tmp/buck-module-cache"
