# Data for NYC: read in latest

# Rates per 100,000 people
# Annual citywide, borough-specific, and demographic specific intercensal population estimates from 2018 were developed by NYC DOHMH on the basis of the US Census Bureau’s Population Estimates Program, as of November 2019.
# 
# Rates of cases at the borough-level were calculated using direct standardization for age at diagnosis and weighting by the US 2000 standard population. https://www.cdc.gov/nchs/data/statnt/statnt20.pdf

# By age: case rate, hospitalized case rate, death rate
# This contains age-specific rates of confirmed cases, hospitalizations, and deaths.
# need to know denominator
# snapshot
byage <- read.csv("by-age.csv")

# This file includes daily counts of new confirmed cases, hospitalizations, and deaths.
# Cases are by date of diagnosis
# Hospitalizations are by date of admission
# Deaths are by date of death
# Because of delays in reporting, the most recent data may be incomplete. Data shown currently will be updated in the future as new cases, hospitalizations, and deaths are reported.
# time series back to 3/3/20 
hosp <- read.csv("case-hosp-death.csv")

# This file includes deaths, by date of interest, for both confirmed COVID-19 deaths, and for probable COVID-19 deaths. Only deaths that occurred on or after March 11 are included (March 11 was the first date of death for a confirmed death).
# A death is classified as confirmed if the decedent was a New York City resident who had a positive SARS CoV-2 (COVID-19) laboratory test.
# A death is classified as probable if the decedent was a New York City resident (NYC resident or residency pending) who had no known positive laboratory test for SARS-CoV-2 (COVID-19) but the death certificate lists as a cause of death “COVID-19” or an equivalent.
# As new information becomes available, some deaths previously classified as probable may be reclassified as laboratory-confirmed.
deaths <- read.csv("probable-confirmed-dod.csv")

# This file includes the cumulative count of New York City residents by ZIP code of residence who:
#   Were ever tested for COVID-19 (SARS-CoV-2)
#   Tested positive The cumulative counts are as of the date of extraction from the NYC Health Department's disease surveillance database.
zip <- read.csv("tests-by-zcta.csv")


# This contains rates of confirmed cases, hospitalizations, and deaths.
bysex <- read.csv("by-sex.csv")