#! /usr/bin/bash

$ERL_LIBS/erlperf/erlperf \
    'run(Arg) -> [csv_scan:file("samples/SampleCSVFile_556kb.csv")].' \
        --init_runner 'lists:seq(1, 100).' \
    'run(Arg) -> [csv_scan:file("samples/SampleCSVFile_1109kb.csv")].' \
        --init_runner 'lists:seq(1, 100).' \
