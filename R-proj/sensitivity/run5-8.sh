#!/bin/sh
nohup ./run_morris_sets.pl morris5.csv 2>&1 > morris5.out &
nohup ./run_morris_sets.pl morris6.csv 2>&1 > morris6.out &
nohup ./run_morris_sets.pl morris7.csv 2>&1 > morris7.out &
nohup ./run_morris_sets.pl morris8.csv 2>&1 > morris8.out &
