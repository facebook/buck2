# A list of available rules and their signatures can be found here: https://buck2.build/docs/api/rules/

load("@one//:rules.bzl", "binary", "library")

library(name = "a")
library(name = "b", deps = [":a"])
binary(name = "works", deps = [":a", ":b"])

# these fail though
binary(name = "fails", deps = ["one//:lib"])
binary(name = "fails2", deps = ["two//:lib"])
