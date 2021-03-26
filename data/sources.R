# Possible data sources

# NY Times: positive cases by US county ####
# date,county,state,fips,cases,deaths
# https://github.com/nytimes/covid-19-data
# county_file_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
# downloaded file locally for now
county_file_url = "data/us-counties.txt"
countydata <- read.csv(county_file_url, 
                       header = TRUE 
                       #stringsAsFactors = FALSE, 
                       #na.strings = ""
                       )
str(countydata)
summary(countydata)

# COVID Tracking: tests, positives, hospitalizations by state ####
# downloaded via api
# https://ourworldindata.org/coronavirus-testing-source-data
# https://covidtracking.com/
# https://docs.google.com/spreadsheets/u/2/d/e/2PACX-1vRwAqp96T9sYYq2-i7Tj0pvTf6XVHjDSMIKBdZHXiCGGdNC0ypEU9NbngS8mxea55JuCFuua1MUeOj5/pubhtml#
state_file_url <- "https://covidtracking.com/api/states/daily.csv"
statedata <- read.csv(state_file_url, 
                       header = TRUE, 
                       stringsAsFactors = FALSE, 
                      na.strings = ""
)

# other potential sources
# https://github.com/CSSEGISandData/COVID-19  # Data underlying johns hopkins dashboard
# https://www.kaggle.com/covid-19-contributions  # Information for Kaggle competition, includes reference to several datasets
# https://coronavirus.1point3acres.com/en # has some data we want... from where? 
