require(lubridate)
library(readxl)
args<-commandArgs(trailingOnly=T)
county=args[1]
#CDC_EVENT_DATE_SARS,CREATE_DATE,DEATH_DATE,AGE_YEARS,Dead,CovidDeath,County,LabTestResult,SYMPTOM_ONSET_DATE,admitdate
kc_data = read.csv(paste0("../data/IDM_",county,"_county.csv"))

header = "death date,age,hosp death"
write(header,file="unsorted_death.csv",append=FALSE)

for (i in 1:nrow(kc_data)) {
  doy=0
  if (substr(kc_data$DEATH_DATE[i],1,3)=="202" && kc_data$CovidDeath[i] == "Yes")
    doy = kc_data$DEATH_DATE[i]
  if (doy != 0) {
    if (substr(kc_data$admitdate[i],1,3)=="202") {
	write(paste(doy,kc_data$AGE_YEARS[i],1,sep=","),file="unsorted_death.csv",append=TRUE)
    } else {
	write(paste(doy,kc_data$AGE_YEARS[i],0,sep=","),file="unsorted_death.csv",append=TRUE)
    }
  }
}

