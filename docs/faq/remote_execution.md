# Remote Execution

buck2 chose to leverage the Bazel spec of [Remote Build Execution](https://bazel.build/remote/rbe) as the primary means of parallelization and caching. This also increases the importance of idempotency and hermeticity. Internally, we refer to this as just "Remote Execution" or RE.
By default, RE workers aim to be network isolated and only have the files/inputs made aware to the RE client. In buck2 this means a requirement of properly declaring dependencies.

## My build fails on remote execution, how do I diagnose the problem?

The build will have failed on a build step or "action" which is composed of a command line, arguments, list of input files/directories by hash, and specified output files and directories.
The build should give you a [`frecli`](https://www.internalfb.com/intern/wiki/CLI_man_pages/frecli/) command to download the action that failed.

1) Download the action, as this will populate a local directory with the inputs and specify the command.
2) Test if the command runs locally

If the command can finish successfully locally but not on RE, then the command may be utilizing the network.
Either fix this network access to be a dependency into the step, or add a label to shift the rule to run locally.

If the command cannot finish successfully, it may be that a dependency to run or an input dependency was not declared.

Common issues:
* Requesting a configuration during the step such as configerator or JustKnobs, and a error may be seen to this effect from the build errors. Add a "reads_configerator" or "justknobs" label to make these steps "local".
