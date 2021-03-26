# this file reads in the data used to fit the model and processes it into the required format

require(lubridate)
library(readxl)



# Original sources ####

# data file downloaded from the DOH site uses data from King County downloaded on Oct 9th (cut down to 9-30)
# this is the data used to calibrate the model 
#
# tests, cases, deaths and hospitalizations are daily and by age gropu (0-19,20-49,50-69,70+)
# dates must be saved in the format year-mo-day (ex. 2020-08-31)

# To get more recent: refilter IDM raw data from sft.wa.gov (PRIVATE!) into IDM_king_county.csv (checked in) run the
# script select_data.sh in the data directory (change input file if you have a new one) then...
#
# get tests, hosp & deaths separately using the following scripts:
#     tally_tests.R, tally_hosp.R and tally_deaths.R
# sort and tally for each date using the following scripts:
#     sort_test_data.sh, sort_hosp_data.sh and sort_death_data.sh
# then merge into one file using "merge_files.R"
#
# The file is "king_county_by_age.csv".  Its header is shown here:
#date,tests1,tests2,tests3,tests4,pos1,pos2,pos3,pos4,daily pos,cases,hosps1,hosps2,hosps3,hosps4,Hospitalizations,tot hosp,deaths1,deaths2,deaths3,deaths4,daily deaths,deaths
#
# Make sure the 1st date is 2/26 (not 1/14) and run "cleanup_final.sh" in the data directory to replace other NAs.
# The NAs come from joins of the files by date on days when there were no deaths!
#
the_data = read.csv("../data/king_county_by_age.csv")

# new data off for calibration is Sept 30th
# -> the command below no longer works! 
# the_data_calib = filter(the_data, date <= ymd("2020-08-31"))
#
# It is replaced by an explicit search for the d-o-y and a truncation of the data at that date

end_day = yday("2020-10-31")

end_idx = length(the_data$date)
for (i in 1:length(the_data$date)) 
{
    if (the_data$date[i] == "2020-10-31")
    {
	end_idx=i
	print(paste("end_idx=",i))
    }
}
the_data_calib = the_data[1:end_idx,]

# Set global starting parameters ####
source("kc_globalparams.R")
source("globalparams.R")

