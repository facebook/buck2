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

### Impact on performances

### Using bazel's events or creating custom new ones?

## Conclusions
