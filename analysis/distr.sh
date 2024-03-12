#!/usr/bin/env sh

for i in {1..100000}
do
    ../_build/default/bin/main.exe >> log.csv
done
