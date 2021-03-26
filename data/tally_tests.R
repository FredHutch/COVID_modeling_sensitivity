require(lubridate)
library(readxl)
args<-commandArgs(trailingOnly=T)
county=args[1]
#CDC_EVENT_DATE_SARS,CREATE_DATE,DEATH_DATE,AGE_YEARS,Dead,CovidDeath,County,LabTestResult,SYMPTOM_ONSET_DATE,admitdate
print(paste0("Reading ","../data/IDM_",county,"_county.csv"))
kc_data = read.csv(paste0("../data/IDM_",county,"_county.csv"))

print(paste0("Done reading ","../data/IDM_",county,"_county.csv"))

header = "event doy,age,result,event date,dead,CovidDeath,SYMPTOM_ONSET_DATE,admitdate"
write(header,file="unsorted_tests.csv",append=FALSE)

for (i in 1:nrow(kc_data)) {
  doy=0
  if (substr(kc_data$CDC_EVENT_DATE_SARS[i],1,3)=="202")
    #doy = yday(ymd(kc_data$CDC_EVENT_DATE_SARS[i]))
    doy = kc_data$CDC_EVENT_DATE_SARS[i]
  else if (substr(kc_data$CREATE_DATE[i],1,3)=="202")
    #doy = yday(ymd(kc_data$CREATE_DATE[i]))
    doy = kc_data$CREATE_DATE[i]
  if (doy != 0) {
    write(paste(doy,kc_data$AGE_YEARS[i],kc_data$LabTestResult[i],kc_data$CDC_EVENT_DATE_SARS[i],kc_data$Dead[i],kc_data$CovidDeath[i],kc_data$SYMPTOM_ONSET_DATE[i],kc_data$admitdate[i],sep=","),file="unsorted_tests.csv",append=TRUE)
  }
}

