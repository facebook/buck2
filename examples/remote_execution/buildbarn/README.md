## Remote execution integration with Buildbarn

This project provides a small example of what a project that utilizes [Buildbarn](https://github.com/buildbarn).

In this document, we will go over the key configs used in this setup.
Using a local docker-compose deployment from the [example deployment repo](https://github.com/buildbarn/bb-deployments).
If you already have a Buildbarn deployment you can skip that.

### Deploy a local Buildbarn

```
... $ git clone https://github.com/buildbarn/bb-deployments
... $ cd bb-deployments
.../bb-deployments $ cd docker-compose
.../bb-deployments/docker-compose $ ./run.sh
```

This uses `docker-compose` to spin up the required Buildbarn services.
Using FUSE based workers, those are generally the fastest as they can load action files on demand
and avoids the overhead of setting up the full input root up front.
In practice many actions do not read all the files in the input root.

If you do not want FUSE workers you can instead switch to hardlinking workers
The example deployments have two worker kinds "fuse", and "hardlinking",
you can see the queues in the Buildbarn scheduler, http://localhost:7982.

```
Buildbarn Scheduler

...

Instance name                                    Platform properties                                    Size  Timeo
   prefix                                                                                               class

"fuse"        OSFamily="Linux" container-image="docker://ghcr.io/catthehacker/                          0     ∞
              ubuntu:act-22.04@sha256:5f9c35c25db1d51a8ddaae5c0ba8d3c163c5e9a4a6cc97acd409ac7eae239448"
"hardlinking" OSFamily="Linux" container-image="docker://ghcr.io/catthehacker/                          0     ∞
              ubuntu:act-22.04@sha256:5f9c35c25db1d51a8ddaae5c0ba8d3c163c5e9a4a6cc97acd409ac7eae239448"
```

More information is available in the [repo](https://github.com/buildbarn/bb-deployments).

### Relevant configs in .buckconfig

First, the Buildbarn endpoint should be configured as the following:

```ini
[buck2_re_client]
engine_address       = grpc://localhost:8980
action_cache_address = grpc://localhost:8980
cas_address          = grpc://localhost:8980
tls                  = false

# Select the instance name, "fuse" or "hardlinking", without quotes
instance_name        = fuse
```

TLS is not used in this example.

### Relevant configs in `ExecutionPlatformInfo`

Buildbarn takes in a Docker image and `OSFamily` in its RE properties to select a worker.
This is configured in `root//platforms:platforms`.
