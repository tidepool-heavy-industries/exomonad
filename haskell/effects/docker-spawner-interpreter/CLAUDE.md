# DockerSpawner Interpreter

HTTP-based interpreter for the `DockerSpawner` effect.

## Overview

This package implements the `DockerSpawner` effect by making HTTP requests to the `docker-spawner` service (a Rust service typically running on `localhost:7435`).

## Build

```bash
cabal build tidepool-docker-spawner-interpreter
```

## Dependencies

- `tidepool-core`: Defines the `DockerSpawner` effect.
- `http-client`: For making HTTP requests.
- `aeson`: For JSON serialization.

## Key Modules

- `Tidepool.DockerSpawner.Interpreter`: The main interpreter logic (`runDockerSpawner`).
