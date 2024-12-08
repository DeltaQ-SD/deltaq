#!/usr/bin/env bash
#
# This script copies the generate HTML test coverage reports
# to the directory $1.
# We expect that test coverage has been created with
# cabal test all --enable-coverage

DIR="$1"
mkdir -p "${DIR}"

for path in lib/*
do
    libname=$(basename "${path}")

    mkdir -p "${DIR}/${libname}"
    cp -R \
        dist-newstyle/build/*/*/${libname}-*/t/test/hpc/vanilla/html/* \
        "${DIR}/${libname}"
done
