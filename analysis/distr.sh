#!/usr/bin/env sh

for i in {1..50000}
do
    ../ExactSamplers/_build/default/bin/main.exe >> log.csv
done
