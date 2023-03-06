Message Digest Chisel Generator
=======================
This is a series of [Chisel](chisel-lang.org/) modules capable of generating hardware for several common [message digest](https://en.wikipedia.org/wiki/Cryptographic_hash_function) algorithms.

### Authors
- Jessica Dagostini <jimlauda@ucsc.edu>
- TJ Banghart <tbanghar@ucsc.edu>

### This is a WIP
Currently the following are supported:
- [MD5](https://en.wikipedia.org/wiki/MD5)
- [SHA-0](https://en.wikipedia.org/wiki/SHA-1#Development)
- [SHA-1](https://en.wikipedia.org/wiki/SHA-1)

We hope to support SHA-2 and its several variants.
### How to get started

#### Clone this repository

```sh
git clone git@github.com:tjbanghart/sha-hash-generator.git
cd sha-hash-generator
```

### Run some tests

```sh
./scripts/test.sh
```

You should see a whole bunch of output that ends with something like the following lines
```
[info] Tests: succeeded 1, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[success] Total time: 5 s, completed Dec 16, 2020 12:18:44 PM
```

**NOTE:** We are actively debugging several issues with the hash algorithm implementations.
You may still run `sbt test` but they will fail.
