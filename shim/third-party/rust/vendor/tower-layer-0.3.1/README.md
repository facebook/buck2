# Tower Layer

Decorates a `Service`, transforming either the request or the response.

## Overview

Often, many of the pieces needed for writing network applications can be
reused across multiple services. The `Layer` trait can be used to write
reusable components that can be applied to very different kinds of services;
for example, it can be applied to services operating on different protocols,
and to both the client and server side of a network transaction.

## License

This project is licensed under the [MIT license](LICENSE).

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in Tower by you, shall be licensed as MIT, without any additional
terms or conditions.
