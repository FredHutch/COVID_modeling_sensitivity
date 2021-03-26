# Global parameters for King County Washington
# Set starting parameters for King County ####
the_pop = 2190000 #2118119                         # King County population
the_age_prop = c(0.2293,	0.4552,	0.2350,	0.0805)   # King County age distribution
the_first_case_doy = yday(ymd("2020-02-26"))     # First diagnosed case, relative to delta0offset, to align model time and calendar time

# fractions based percent in an age group that are hospitalized (based on date of hospital admission)
# obtained by running "./get_hosp_data.sh king" (uses "IDM_king_county.csv") in the ../data directory
#
source("../data/hosp_data_king.R")

# fractions based percent in an age group that die after hospitalization (based on date of death)
# obtained by running "./get_out_death_data.sh king" (uses "IDM_king_county.csv") in the ../data directory
#
source("../data/out_death_data_king.R")

# fractions based percent in an age group that die after hospitalization (based on date of death)
# obtained by running "./get_hosp_death_data.sh king" (uses "IDM_king_county.csv") in the ../data directory
#
source("../data/hosp_death_data_king.R")
