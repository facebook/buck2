# Observability for Buck2 OSS

## Context

In large organizations building software is a fundamental part of a developer's journey and can significantly impact productivity. As such, being able to monitor and debug builds is a fundamental ability every user and power-user must have. 
This ability is further limited by the remote nature of CI builds where instances are usually remote and most of the times inaccessible due to security requirements (see SLSA L2 and above).

## Prior Art

### Bazel - BEP

Bazel provides multiple tools to monitor and debug various aspects of a build, but most importantly it supports a protocol called BEP (Build Event Protocol) to send events to a remote server. The API is tool agnostic and only provides a method to send events of some kind. The type of the events though can be defined on a per tool basis. Bazel comes with its own set of events that should be almost completely applicable to Buck2 as well, given the similarities between the two.

File uploads are performed using the same ByteStream API the RBE protocol uses.

#### Tools using BEP

The biggest advantage in adopting this protocol is the number of already existing tools already able to process it.

## Open problems

### Reliability

### Sync Vs Async event stream

Sending events to a remote end may cause a build to incur delays. The most clear of which is when a user wants to ensure that by the end of a command's invocation all events have been successfully uploaded or the command exits unsuccessfully. Doing this within the command itself will always add additional time to the build. At a minimum that delay will be the time it takes to send the last event, but in the worst case scenario this could cause the build to wait for multiple large blobs to be uploaded. This could take several minute and have a profound impact on performances.

Alternatively, the events could be send asynchronously. This works really well when the instance the command is run from can be guaranteed to stay alive for as long as it takes to upload all the events. This is typically not the case with ephemeral CI instances which are becoming more and more common. Furthermore, consecutive invocations that do not wait for the previous events to be sent may incur higher network latency. Finally, mutating the file system during an asynchronous stream of events may cause inconsistent uploads and thus incorrect results.

### Using bazel's events or creating custom new ones?

## Proposal


