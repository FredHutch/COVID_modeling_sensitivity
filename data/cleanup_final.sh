#!/bin/sh
county=$1
cat $county\_county_by_age.csv | awk -F',' '{if (NR > 1){printf("%s,",$1);for(i=2;i<NF;i++){if ($i=="NA"){if (i==22){printf("%d,",p22);};if (i==27){printf("%d,",p27);};if (i!=22&&i!=27){printf("0,");}}else{printf("%d,",$i);if (i==22){p22=$22;}if(i==27){p27=$27;}}}if ($NF=="NA"){printf("%d\n",pNF);}else{printf("%d\n",$NF);pNF=$NF;};}else{print $0;}}'> temp.csv
mv temp.csv $county\_county_by_age.csv
