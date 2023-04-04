# buckd

Buck runs a persistent daemon process (buckd) to reuse work between commands.
Most work is done by the daemon process. When executing a buck command, the
process running the command is a client to the buckd server. The buckd server
exposes a simple grpc service that the client uses to implement the various
buck commands.

There's a small set of commands/arguments that don't require the daemon (`buck
help`, cli arg parse failures, `buck version`, ...), but most commands will
require it.

For almost all commands, buck requires that the client and server are the same
version of buck and may restart buckd to ensure that's the case.

# daemon process flow

The daemon process is started with the (hidden) `buck daemon` command.

The daemon process has a simple startup. It will first daemonize itself and
write its pid to a locked file "buckd.pid" in the "daemon directory" (a
directory in `$HOME/.buck` specific to that repository+output directory). The
file is locked exclusively by the daemon process until it exits. This means that
only a single daemon is allowed at a time. It redirects its stdout and stderr
to files in the daemon directory.

The daemon then starts up the grpc DaemonApi server. Once that is running, it will
write the port it is running on (along with some other information) to the
"buckd.info" file in the daemon dir. Once that is done, the server is ready to
be used.

There are 3 ways that the buckd process will shutdown:

1. The grpc api includes a `kill()` call that will shutdown buckd.
2. buckd will periodically (every 100s or so) check the "buckd.pid" and
   "buckd.info" files to ensure that they still match that buckd process.
3. If buckd hits a rust `panic()` the buckd process will exit

# client connection and buckd startup

When the client is processing a command that requires communicating with the
buckd server it will follow this approach:

1. read the "buckd.info" file to get the port the grpc api is being served on
2. connect to the api on that port
3. send a `status()` request to get the version

If there is an error during 1-3, or if there is a version mismatch the client
needs to (re)start the buck daemon. Otherwise, the client can continue as it
now has made a connection with a correctly versioned buckd.

When the client is killing or starting the buckd process, it will grab an
exclusive lock on the "lifecycle.lock" file in the daemon directory to ensure
that multiple clients aren't racing with each other.

To start/restart the buckd process, the client does:

1. lock the "lifecycle.lock" file
2. send a kill command to the existing buckd
3. ensure the buckd process has exited (based on pid)
4. run a `buck daemon` command to start buckd
5. wait for the daemon to start up and the grpc server to be ready
6. release the "lifecycle.lock" file

After that, it will repeat the connection steps (including verifying the
version after connecting).

# buck kill and other daemon restarts

If there are other invocations currently using the buck daemon when it is killed or
restarted by a client, those invocations will fail due to the early disconnection.

Generally, we support concurrent buck invocations using the same buck version, but
if there are concurrent invocations with different versions, they may unexpectedly
fail or otherwise work incorrectly. This is sufficient for the normal buck workflow
where the buckversion is checked into the repo, in that case, it's not expected that
buck commands will work across a rebase or other operation that changes the buckversion.

# correctness

We have a couple of guarantees here.

1. Only a single buckd is running at a time
2. Only a single client is killing/starting a buckd at a time
3. A client only uses a buckd connection after making sure it has a compatible version

The main way that we could run into issues would be if there are multiple clients
that are racing and they want different versions of buck. In that case, one
might cause the other two fail to connect to a buckd with the correct version
or one of the client's connections may be prematurely disconnected. A client **will not**
use a server with a mismatched version. While this is a failure, no expected workflow
would hit this case, all concurrent commands should be using the same buck version.
