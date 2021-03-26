#!/bin/sh
county=$1
sort -n -k1 -t',' unsorted_hosp.csv > sorted_hosp.csv
cat sorted_hosp.csv | awk -F',' 'BEGIN{pdate="";}{if (NR==1){printf("date,hosps1,hosps2,hosps3,hosps4,Hospitalizations\n");}else {if(NR==2 || $1==pdate){pdate=$1;if($2 < 20){hosp1++;}else{if ($2 < 50){hosp2++;}else{if ($2 < 70){hosp3++;}else{hosp4++;}}}} else {printf("%s,%d,%d,%d,%d,%d\n",pdate,hosp1,hosp2,hosp3,hosp4,hosp1+hosp2+hosp3+hosp4);pdate=$1;hosp1=0;hosp2=0;hosp3=0;hosp4=0;if($2<20){hosp1++;}else{if ($2 < 50){hosp2++;}else{if ($2 < 70){hosp3++;}else{hosp4++;}}}}}}' > $county\_county_hosp_by_age.csv
