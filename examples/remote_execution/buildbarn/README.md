## Remote execution integration with Build Barn

This project provides a small example of what a project that utilizes [Build Barn](https://github.com/buildbarn) for RE might look like.

In this document, we will go over the key configs used in this setup.

### Relevant configs in .buckconfig

First, the Build Barn endpoint and certificate should be configured as the following:

```ini
[buck2_re_client]
engine_address       = $BB_ENDPOINT
action_cache_address = $BB_ENDPOINT
cas_address          = $BB_ENDPOINT
```

If you're using the Docker-compose setup, the endpoint would be `http://localhost:8980` for all 3 of those.

Additionally, set the `digest_algorithm` config to `SHA256`.
```ini
[buck2]
digest_algorithms = SHA256
```

### Relevant configs in `ExecutionPlatformInfo`

Build Barn takes in a Docker image and OSFamily  in its RE properties to select a worker.

The execution platform used in this project `root//platforms:platforms` do so.
