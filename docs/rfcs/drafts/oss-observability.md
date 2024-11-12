# Observability for Buck2 OSS

## Context

In large organizations building software is a fundamental part of a developer's journey and can significantly impact productivity. As such, being able to monitor and debug builds is a fundamental ability every user and power-user must have. 
This ability is further limited by the remote nature of CI builds where instances are usually remote and most of the times inaccessible due to security requirements (see SLSA L2 and above).

## Prior Art

### Bazel - BEP

Bazel provides multiple tools to monitor and debug various aspects of a build, but most importantly it supports a protocol called BEP (Build Event Protocol) to send events to a remote server. The API is tool agnostic and only provides a method to send events of some kind. The type of the events though can be defined on a per tool basis. Bazel comes with its own set of events that should be almost completely applicable to Buck2 as well, given the similarities between the two.

File uploads are performed using the same ByteStream API the RBE protocol uses.

#### Tools using BEP

The biggest advantage in adopting this protocol is the number of already existing tools already able to process it. Anectodally there are various internal users, especially large corporations, using BEP to create integrations and tools of all kinds (eg: [Salesforce's talk during 'Build Meetup 2021'](https://youtu.be/qboJOW1vZLA?si=w7uC-ZxhGtHHM_m6)). This, combined with the already existing similarities with Bazel, could be a driver for adoption. Furtheremore, there are some commercial and free-OSS options available:
- [EngFlow (commercial)](https://www.engflow.com/)
- BuildBuddy: [limited free OSS offering](https://github.com/buildbuddy-io/buildbuddy) and [fully featured commercial](https://www.buildbuddy.io/)
- [Bzl and stackb's build UI](https://bzl.io/)

## Open problems

### Sync Vs Async event stream

Sending events to a remote end may cause a build to incur delays. The most clear of which is when a user wants to ensure that by the end of a command's invocation all events have been successfully uploaded or the command exits unsuccessfully. Doing this within the command itself will always add additional time to the build. At a minimum that delay will be the time it takes to send the last event, but in the worst case scenario this could cause the build to wait for multiple large blobs to be uploaded. This could take several minute and have a profound impact on performances.

Alternatively, the events could be send asynchronously. This works really well when the instance the command is run from can be guaranteed to stay alive for as long as it takes to upload all the events. This is typically not the case with ephemeral CI instances which are becoming more and more common. Furthermore, consecutive invocations that do not wait for the previous events to be sent may incur higher network latency. Finally, mutating the file system during an asynchronous stream of events may cause inconsistent uploads and thus incorrect results.

Bazel implements both options. A third option could be to use async but provide a `buck2 flush events` subcommand that waits until all the existing events have been sent, retried if necessary and returns success or failure depending on the outcome of the upload of events. 

All these options are **NOT** mutually exclusive and do in fact combine quite well. 

### Using bazel's events or creating custom new ones?

As previously mentioned, were we to adopt BEP, then there'd still be an open question: should buck2 reuse Bazel's events or should we create new ones? There are [some generalization efforts](https://github.com/bazelbuild/remote-apis/issues/318) happening at the moment, however Bazel and Buck2 share lots of similarities. Enough that all events are almost perfectly applicable to Buck2, though less useful at times, like with command line options. On the other hand, a new set of events would perfectly describe a buck2 build, but would be prone to more issues initially and would require a bigger investment upfront in terms of design and development. Alternatively, we could start with a smaller subset of Bazel events that are equivalent in Buck2 and then proceed to add the additional events needed to properly express the unique behaviours of a Buck2 build.

## Proposal

## Links

- [BEP explaination](https://bazel.build/remote/bep)
- [Bazel's BEP events and glossary](https://bazel.build/remote/bep-glossary)
- 
