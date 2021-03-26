#!/bin/sh
county=$1
sort -n -k1 -t',' unsorted_tests.csv > sorted_tests.csv
#event doy,age,result,event date,dead,CovidDeath,SYMPTOM_ONSET_DATE,admitdate
cat sorted_tests.csv | awk -F',' 'BEGIN{pdate="";}{if (NR==1){printf("date,tests1,tests2,tests3,tests4,tests,pos1,pos2,pos3,pos4,daily pos,cases,sym_pos1,sym_pos2,sym_pos3,sym_pos4,sym_cases\n");}else {if(NR==2 || $1==pdate){pdate=$1;if($2 < 20){test1++;if ($3=="Positive"){pos1++; if (($7 != "" && $7 != "FALSE" && $7 <= $4)||($8 !="" && $8!="FALSE")||($5=="Dead" && $6=="Yes")) sym_pos1++;}}else{if ($2 < 50){test2++;if ($3=="Positive"){pos2++;if (($7 != "" && $7 != "FALSE" && $7 <= $4)||($8 !="" && $8!="FALSE")||($5=="Dead" && $6=="Yes")) sym_pos2++;}}else{if ($2 < 70){test3++;if ($3=="Positive"){pos3++;if (($7 != "" && $7 != "FALSE" && $7 <= $4)||($8 !="" && $8!="FALSE")||($5=="Dead" && $6=="Yes")) sym_pos3++;}}else{test4++;if ($3=="Positive"){pos4++;if (($7 != "" && $7 != "FALSE" && $7 <= $4)||($8 !="" && $8!="FALSE")||($5=="Dead" && $6=="Yes")) sym_pos4++;}}}}} else {cases+=pos1+pos2+pos3+pos4;sym_cases+=sym_pos1+sym_pos2+sym_pos3+sym_pos4;printf("%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n",pdate,test1,test2,test3,test4,test1+test2+test3+test4,pos1,pos2,pos3,pos4,pos1+pos2+pos3+pos4,cases,sym_pos1,sym_pos2,sym_pos3,sym_pos4,sym_cases);pdate=$1;test1=0;test2=0;test3=0;test4=0;pos1=0;pos2=0;pos3=0;pos4=0;sym_pos1=0;sym_pos2=0;sym_pos3=0;sym_pos4=0;if($2<20){test1++;if ($3=="Positive"){pos1++;if (($7 != "" && $7 != "FALSE" && $7 <= $4)||($8 !="" && $8!="FALSE")||($5=="Dead" && $6=="Yes")) sym_pos1++;}else{if ($2 < 50){test2++;if ($3=="Positive"){pos2++;if (($7 != "" && $7 != "FALSE" && $7 <= $4)||($8 !="" && $8!="FALSE")||($5=="Dead" && $6=="Yes")) sym_pos2++;}}else{if ($2 < 70){test3++;if ($3=="Positive"){pos3++;if (($7 != "" && $7 != "FALSE" && $7 <= $4)||($8 !="" && $8!="FALSE")||($5=="Dead" && $6=="Yes")) sym_pos3++;}}else{test4++;if ($3=="Positive"){pos4++;if (($7 != "" && $7 != "FALSE" && $7 <= $4)||($8 !="" && $8!="FALSE")||($5=="Dead" && $6=="Yes")) sym_pos4++;}}}}}}}}' > $county\_county_tests_by_age.csv
