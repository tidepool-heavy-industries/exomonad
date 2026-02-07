# `proto3-wire`

[![Build Status](https://github.com/awakesecurity/proto3-wire/actions/workflows/ci.yml/badge.svg)](https://github.com/awakesecurity/proto3-wire/actions/workflows/ci.yml)

This library provides a low-level implementation of the [Protocol Buffers version 3 wire format](https://developers.google.com/protocol-buffers/docs/encoding).

This library takes a minimalist approach, supporting only the basic wire format.
In particular, correctness is left up to the user in many places (for example,
ensuring that encoders use increasing field numbers).

There are approaches which can give more guarantees, such as Generics and Template
Haskell, but those approaches are not implemented here and are left to
higher-level libraries.

## Building

Install [Stack](http://docs.haskellstack.org/en/stable/README/), clone this repository, and run:

```text
stack build
```

To run tests or generate documentation, use

```text
stack build [--test] [--haddock]
```

### GHC Versions

#### GHC 9.10

Supported on Linux and Darwin.

#### GHC 9.8

Supported on Linux and Darwin.

#### GHC 9.6

Supported on Linux and Darwin.

#### GHC 9.4

Supported on Linux and Darwin.

#### GHC 9.2

Supported on Linux and Darwin.

#### GHC 9.0

Supported only on Linux because "crypton" fails a test on Darwin,
probably due to [this issue](https://github.com/kazu-yamamoto/crypton/issues/35).

#### GHC 8.10.7

Supported on Linux and Darwin.
