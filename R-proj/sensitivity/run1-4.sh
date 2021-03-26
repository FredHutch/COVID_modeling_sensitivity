#!/bin/sh
nohup ./run_morris_sets.pl morris1.csv 2>&1 > morris1.out &
nohup ./run_morris_sets.pl morris2.csv 2>&1 > morris2.out &
nohup ./run_morris_sets.pl morris3.csv 2>&1 > morris3.out &
nohup ./run_morris_sets.pl morris4.csv 2>&1 > morris4.out &
