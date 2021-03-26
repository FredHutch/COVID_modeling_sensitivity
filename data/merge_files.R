require(lubridate)
library(readxl)
library(dplyr)
library(reshape2)
library(tidyr)

args<-commandArgs(trailingOnly=T)
county=args[1]

#date,tests1,tests2,tests3,tests4,pos1,pos2,pos3,pos4
kc_data1 = read.csv(paste0(county,"_county_tests_by_age.csv"))

#date,hosps1,hosps2,hosps3,hosps4
kc_data2 = read.csv(paste0(county,"_county_hosp_by_age.csv"))

kc_data <- left_join(kc_data1, kc_data2)
names(kc_data)

#date,deaths1,deaths2,deaths3,deaths4
kc_data3 = read.csv(paste0(county,"_county_death_by_age.csv"))
kc_data <- left_join(kc_data, kc_data3)
names(kc_data)

write.csv(kc_data, paste0("../data/",county,"_county_by_age.csv"), row.names = FALSE, quote = FALSE)
