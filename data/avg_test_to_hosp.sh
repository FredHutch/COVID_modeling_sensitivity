#!/bin/sh
#This script is used to populate the monthly variables for hosp_rates in kc_global_params.R
#
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}} END {printf("%g days avg (%g)\n",days/entries,entries/days);}'

cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 < 20 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "02" || hosp_date[2] == "03"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Feb/Mar: for 0-19: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Feb/Mar: No 0-19 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 20 && $4 < 50 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "02" || hosp_date[2] == "03"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Feb/Mar: for 20-50: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Feb/Mar: No 20-49 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 50 && $4 < 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "02" || hosp_date[2] == "03"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Feb/Mar: for 50-70: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Feb/Mar: No 50-69 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "02" || hosp_date[2] == "03"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Feb/Mar: for 70+: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Feb/Mar: No 70+ hosps\n");}}'

cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 < 20 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "04"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Apr: for 0-19: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Apr: No 0-19 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 20 && $4 < 50 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "04"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Apr: for 20-50: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Apr: No 20-49 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 50 && $4 < 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "04"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Apr: for 50-70: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Apr: No 50-69 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "04"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Apr: for 70+: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Apr: No 70+ hosps\n");}}'

cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 < 20 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "05"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("May: for 0-19: %g days avg (%g)\n",days/entries,entries/days);} else {printf("May: No 0-19 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 20 && $4 < 50 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "05"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("May: for 20-50: %g days avg (%g)\n",days/entries,entries/days);} else {printf("May: No 20-49 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 50 && $4 < 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "05"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("May: for 50-70: %g days avg (%g)\n",days/entries,entries/days);} else {printf("May: No 50-69 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "05"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("May: for 70+: %g days avg (%g)\n",days/entries,entries/days);} else {printf("May: No 70+ hosps\n");}}'

cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 < 20 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "06"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Jun: for 0-19: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Jun: No 0-19 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 20 && $4 < 50 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "06"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Jun: for 20-50: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Jun: No 20-49 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 50 && $4 < 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "06"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Jun: for 50-70: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Jun: No 50-69 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "06"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Jun: for 70+: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Jun: No 70+ hosps\n");}}'

cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 < 20 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "07"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Jul: for 0-19: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Jul: No 0-19 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 20 && $4 < 50 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "07"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Jul: for 20-50: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Jul: No 20-49 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 50 && $4 < 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "07"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Jul: for 50-70: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Jul: No 50-69 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "07"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Jul: for 70+: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Jul: No 70+ hosps\n");}}'

cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 < 20 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "08"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Aug: for 0-19: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Aug: No 0-19 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 20 && $4 < 50 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "08"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Aug: for 20-50: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Aug: No 20-49 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 50 && $4 < 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "08"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Aug: for 50-70: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Aug: No 50-69 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "08"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Aug: for 70+: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Aug: No 70+ hosps\n");}}'

cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 < 20 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "09"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Sept: for 0-19: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Sept: No 0-19 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 20 && $4 < 50 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "09"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Sept: for 20-50: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Sept: No 20-49 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 50 && $4 < 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "09"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Sept: for 50-70: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Sept: No 50-69 hosps\n");}}'
cat IDM_king_county.csv | awk -F',' '{if (NR > 1 &&  $4 > 70 && $10 ~ /2020/ && $1 ~ /2020/ && $1 != $10){split($10,hosp_date,"-");if (hosp_date[2] == "09"){split($1,test_date,"-");if (hosp_date[2] == "02"){mon_days=29;}else{ if (hosp_date[2] == "04" || hosp_date[2] == "06" || hosp_date[2] == "09" ){mon_days=30;}else{mon_days=31;}}elapsed=(hosp_date[2]-test_date[2])*mon_days+(hosp_date[3]-test_date[3]);if(elapsed > 0){entries++;days+=elapsed;}}}} END {if (entries > 0) {printf("Sept: for 70+: %g days avg (%g)\n",days/entries,entries/days);} else {printf("Sept: No 70+ hosps\n");}}'