library(dplyr)
library(ggplot2)


folder <- "data/CDCHosp"
filelist <- list.files(folder)

hospdata <- read.csv("data/COVID-19Surveillance_COVID-NET_Entire Network_Data.csv", skip = 2, na.strings = "null")

for (i in 2:length(filelist)){
  temp <- read.csv(paste0(folder, "/", filelist[i]), skip = 2, na.strings = "null")
  hospdata <- bind_rows(hospdata, temp)
}

rm(temp)

hospdata <- hospdata %>%
  filter(is.na(YEAR) == FALSE)

hospdata <- hospdata %>%
  filter(is.na(CUMULATIVE.RATE) == FALSE)

write.csv(hospdata, "data/CDCHosp/hospdata.csv", row.names = FALSE)




#The Coronavirus Disease 2019 (COVID-19)-Associated Hospitalization Surveillance Network (COVID-NET) conducts population-based surveillance for laboratory-confirmed COVID-19-associated hospitalizations in children (persons younger than 18 years) and adults. The current network covers nearly 100 counties in the 10 Emerging Infections Program (EIP) states (CA, CO, CT, GA, MD, MN, NM, NY, OR, and TN) and four additional states through the Influenza Hospitalization Surveillance Project (IA, MI, OH, and UT). The network represents approximately 10% of US population (~32 million people).

#Cases are identified by reviewing hospital, laboratory, and admission databases and infection control logs for patients hospitalized with a documented positive SARS-CoV-2 test.

#Data gathered are used to estimate age-specific hospitalization rates on a weekly basis and describe characteristics of persons hospitalized with COVID-19. Laboratory confirmation is dependent on clinician-ordered SARS-CoV-2 testing. Therefore, the rates provided are likely to be underestimated as COVID-19-associated hospitalizations can be missed due to test availability and provider or facility testing practices.

#COVID-NET hospitalization data are preliminary and subject to change as more data become available. Please use the following citation when referencing these data: “COVID-NET: COVID-19-Associated Hospitalization Surveillance Network, Centers for Disease Control and Prevention. WEBSITE. Accessed on DATE”.
