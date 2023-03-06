#!/usr/bin/env bash
sbt "test:testOnly *FSMTester"
#NOTE: We are actively debugging several issues with the hash algorithm implementations.
# You may still run `sbt test` but they will fail.
#sbt test