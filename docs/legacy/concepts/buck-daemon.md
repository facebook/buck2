# Buck Daemon (buckd)

The first time that you run a Buck command, Buck starts a daemon process for the current project in the current working directory. For subsequent commands, Buck checks for the running daemon process and if found, uses the daemon to execute the command. Using the Buck daemon can save significant time as it avoids the overhead of starting a Java virtual machine (JVM) and loading the Buck class files. It also enables Buck to take advantage of caches for build-file parsing, and for Buck's target graph and action graph.
The Buck daemon writes its port, process id, and log output to files in a `.buckd` directory that the daemon creates in the project root directory. Subsequent Buck commands use these files to find the daemon process, and a new Buck daemon process will use them to kill any already-existing daemon process.
It is safe to run multiple Buck daemons started from different project directories as they do not interfere with each other, making `buckd` suitable for use in shared-server environments or where several projects are being worked on concurrently.
While it runs, the Buck daemon process monitors the project's file system and invalidates cached build rules if any build input files change. The Buck daemon excludes from monitoring any subtrees of the project file system that are specified in the [`[project].ignore`](https://buck.build/files-and-dirs/buckconfig.html#project.ignore) setting of `.buckconfig`. By adding project-specific output directories and source-control directories, such as`.git`, to this setting, you can significantly improve performance; this might be necessary to avoid file-change overflows when using Buck daemons to build large projects.
By default, Buck daemon processes ignore changes to temporary files created by text editors.

#### Killing or disabling the Buck daemon

The Buck daemon process is killed if

* the [`buck clean`](https://buck.build/command/clean.html) command is run.
* the `.buckd` directory in the project root directory is deleted.
* the daemon has been idle for 24 hours.

You can also kill the Buck daemon explicitly by running [`buck kill`](https://buck.build/command/kill.html) in the directory tree for your project. Note that if—for some reason—multiple instances of the daemon are running, the `buck kill` command kills only one of them.
*If the daemon is killed, you might experience a significant delay the next time that you invoke a Buck command as the daemon restarts.*
To disable the daemon and prevent it from starting, set the environment variable `NO_BUCKD` to `1`. For example:

```
NO_BUCKD=1
buck build project_name
```

#### Buck configuration changes invalidate the daemon's state

If you change Buck's configuration, it invalidates any cached state stored by the Buck daemon—unless that change is to a *small subset* of configuration settings that are supported in [`AbstractConfigIgnoredByDaemon.java`](https://github.com/facebook/buck/blob/master/src/com/facebook/buck/command/config/AbstractConfigIgnoredByDaemon.java).
If the daemon's state is invalidated, you'll see a message similar to the following:

```
Invalidating internal cached state:
Buck configuration options changed between invocations.
This may cause slower builds.
```

Note that a Buck configuration change that invalidates the daemon's state can be caused not only by explicitly changing a setting in one of Buck's configuration files, such as `.buckconfig` or `.buckconfig.local`, but also by using the `--config`, `--flagfile`, or `--config-file` [command-line parameters](https://buck.build/command/common_parameters.html).
