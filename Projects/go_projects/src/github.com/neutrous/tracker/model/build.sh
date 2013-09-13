#! /usr/bin/env bash

# Directly build everything of this directory.
protoc --go_out=. *.proto
