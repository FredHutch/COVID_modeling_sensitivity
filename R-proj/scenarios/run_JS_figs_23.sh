#!/bin/sh
Rscript JS_figs_23_data.R seniors 0.1  0.2 0.6 5000 1100000 8 100 300 0
Rscript JS_figs_23_table.R seniors 0.1  0.2 0.6 5000 1100000 100 300
head -n 1 out/results_table_seniors_vep_0.1_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv > seniors_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv
grep -v scenario out/results_table_seniors_vep_0.1_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv >> seniors_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv

Rscript JS_figs_23_data.R seniors 0.3  0.2 0.6 5000 1100000 8 100 300 0
Rscript JS_figs_23_table.R seniors 0.3  0.2 0.6 5000 1100000 100 300
grep -v scenario out/results_table_seniors_vep_0.3_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv >> seniors_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv

Rscript JS_figs_23_data.R seniors 0.5  0.2 0.6 5000 1100000 8 100 300 0
Rscript JS_figs_23_table.R seniors 0.5  0.2 0.6 5000 1100000 100 300
grep -v scenario out/results_table_seniors_vep_0.5_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv >> seniors_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv

Rscript JS_figs_23_data.R seniors 0.7  0.2 0.6 5000 1100000 8 100 300 0
Rscript JS_figs_23_table.R seniors 0.7  0.2 0.6 5000 1100000 100 300
grep -v scenario out/results_table_seniors_vep_0.7_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv >> seniors_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv

Rscript JS_figs_23_data.R seniors 0.9  0.2 0.6 5000 1100000 8 100 300 0
Rscript JS_figs_23_table.R seniors 0.9  0.2 0.6 5000 1100000 100 300
grep -v scenario out/results_table_seniors_vep_0.9_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv >> seniors_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv
