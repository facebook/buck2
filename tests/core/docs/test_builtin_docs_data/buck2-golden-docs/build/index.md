# This file is @generated, regenerate by re-running test with `-- --env BUCK2_UPDATE_GOLDEN=1` appended to the test command

# Build APIs
## CommandExecutorConfig
## ConfigurationInfo
## ConstraintSettingInfo
## ConstraintValueInfo
## DefaultInfo
# //foo_binary.bzl
# //subdir/BUCK
# ":gen_stuff" pulls the default_outputs for //subdir:gen_stuff
# Builds just 'foo' binary. The strip command is never invoked.
# builds the 'foo' binary, because it is needed by the 'strip' command. Ensures that
# both the stripped binary and the debug symbols are built.
## ExecutionPlatformInfo
## ExecutionPlatformRegistrationInfo
## ExternalRunnerTestInfo
## InstallInfo
## LocalResourceInfo
## PlatformInfo
## Provider
## RequiredTestLocalResource
## RunInfo
## TemplatePlaceholderInfo
## ValidationInfo
## ValidationSpec
## WorkerInfo
## WorkerRunInfo
## anon\_rule
## attrs
## cmd\_args
## `ignore_artifacts`
## `hidden`
## `absolute_prefix` and `absolute_suffix`
# `parent
# `relative_to=dir` or `relative_to=(dir, parent)`
# `replace_regex`
## dedupe
## dynamic\_actions
## get\_base\_path
## get\_cell\_name
## glob
## host\_info
## implicit\_package\_symbol
## load\_symbols
## oncall
## package
## package\_name
## plugins
## provider
## provider\_field
## read\_config
## read\_oncall
## read\_package\_value
## read\_parent\_package\_value
## read\_root\_config
## regex
## regex\_match
## repository\_name
## rule
## rule\_exists
## select
## select\_equal\_internal
## select\_map
## select\_test
## set\_cfg\_constructor
## set\_starlark\_peak\_allocated\_byte\_limit
## sha256
## soft\_error
## transition
## transitive\_set
## warning
## write\_package\_value
