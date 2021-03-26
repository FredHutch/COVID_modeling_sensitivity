#!/bin/sh
nohup ./run_sensitivity.sh 20 200 1.55 0.8 1 2>&1 > sens_20_200.out &
nohup ./run_sensitivity.sh 20 350 1.55 0.8 1 2>&1 > sens_20_350.out &
nohup ./run_sensitivity.sh 20 500 1.55 0.8 1 2>&1 > sens_20_500.out &
nohup ./run_sensitivity.sh 20 650 1.55 0.8 1 2>&1 > sens_20_650.out &
