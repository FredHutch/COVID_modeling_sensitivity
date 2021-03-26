#!/bin/sh
Rscript JS_paper_figs_345.R seniors 0.1  0.2 0.6 2000 1e6 8 100 300 0
Rscript vac_table_2yr.R seniors 0.1  0.2 0.6 2000 1e6 100 300
head -n 1 out/results_table_seniors_vep_0.1_SD_0.2_to_0.6_rate_2000_tot_1100000_trigs_100_300.csv > seniors_2yr_100_300.csv
grep -v scenario out/results_table_seniors_vep_0.1_SD_0.2_to_0.6_rate_2000_tot_1100000_trigs_100_300.csv | grep -v epidemic >> seniors_2yr_100_300.csv
Rscript JS_paper_figs_345.R seniors 0.1  0.2 0.6 5000 1e6 8 100 300 1
Rscript vac_table_2yr.R seniors 0.1  0.2 0.6 5000 1e6 100 300
grep -v scenario out/results_table_seniors_vep_0.1_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv | grep -v epidemic >> seniors_2yr_100_300.csv
Rscript JS_paper_figs_345.R seniors 0.1  0.2 0.6 10000 1e6 8 100 300 1
Rscript vac_table_2yr.R seniors 0.1  0.2 0.6 10000 1e6 100 300
grep -v scenario out/results_table_seniors_vep_0.1_SD_0.2_to_0.6_rate_10000_tot_1100000_trigs_100_300.csv | grep -v epidemic >> seniors_2yr_100_300.csv
exit
Rscript JS_paper_figs_345.R adults 0.1  0.2 0.6 2000 1e6 6 100 300 0
Rscript vac_table_2yr.R adults 0.1  0.2 0.6 2000 1e6 100 300
head -n 1 out/results_table_adults_vep_0.1_SD_0.2_to_0.6_rate_2000_tot_1100000_trigs_100_300.csv > adults_2yr_100_300.csv
grep -v scenario out/results_table_adults_vep_0.1_SD_0.2_to_0.6_rate_2000_tot_1100000_trigs_100_300.csv | grep -v epidemic >> adults_2yr_100_300.csv
Rscript JS_paper_figs_345.R adults 0.1  0.2 0.6 5000 1e6 6 100 300 1
Rscript vac_table_2yr.R adults 0.1  0.2 0.6 5000 1e6 100 300
grep -v scenario out/results_table_adults_vep_0.1_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv | grep -v epidemic >> adults_2yr_100_300.csv
Rscript JS_paper_figs_345.R adults 0.1  0.2 0.6 10000 1e6 6 100 300 1
Rscript vac_table_2yr.R adults 0.1  0.2 0.6 10000 1e6 100 300
grep -v scenario out/results_table_adults_vep_0.1_SD_0.2_to_0.6_rate_10000_tot_1100000_trigs_100_300.csv | grep -v epidemic >> adults_2yr_100_300.csv
