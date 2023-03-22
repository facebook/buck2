## Remote execution integration with EngFlow

This project provides a small example of what a project that utilizies [EngFlow](https://www.engflow.com/)'s RE offering might look like.

In this document, we will go over the key configs used in this setup.

### Relevant configs in .buckconfig

First, the EngFlow endpoint and certificate should be configured as the following:

```ini
[buck2_re_client]
engine_address       = $ENGFLOW_ENDPOINT
action_cache_address = $ENGFLOW_ENDPOINT
cas_address          = $ENGFLOW_ENDPOINT
tls_client_cert      = $ENGFLOW_CERTIFICATE
```

Additionally, set the `digest_algorithm` config to `SHA256`.
```ini
[buck2]
digest_algorithms = SHA256
```

### Relevant configs in `ExecutionPlatformInfo`

EngFlow takes in a Docker image as its execution platform.
The execution platform used in this project `root//platforms:platforms` uses the `container-image` key to set this up.
