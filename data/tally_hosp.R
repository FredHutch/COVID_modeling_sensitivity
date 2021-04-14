require(lubridate)
library(readxl)
args<-commandArgs(trailingOnly=T)
county=args[1]
#CDC_EVENT_DATE_SARS,CREATE_DATE,DEATH_DATE,AGE_YEARS,Dead,CovidDeath,County,LabTestResult,SYMPTOM_ONSET_DATE,admitdate
kc_data = read.csv(paste0("../data/IDM_",county,"_county.csv"))
print(paste0("Done reading ","../data/IDM_",county,"_county.csv"))

header = "admin date,age"
write(header,file="unsorted_hosp.csv",append=FALSE)

for (i in 1:nrow(kc_data)) {
  doy=0
  if (kc_data$admitdate[i] != "" && substr(kc_data$admitdate[i],1,3)=="202")
    doy = kc_data$admitdate[i]
  if (doy != 0) {
    write(paste(doy,kc_data$AGE_YEARS[i],sep=","),file="unsorted_hosp.csv",append=TRUE)
  }
}

