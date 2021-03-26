# Set starting parameters for Spokane County ####
the_pop = 522800                         # Spokane County population
the_age_prop = c(0.2293,	0.4552,	0.2350,	0.0805)   # Spokane County age distribution
the_first_case_doy = yday(ymd("2020-03-08"))     # First diagnosed case, relative to delta0offset, to align model time and calendar time

# fractions based fraction of tests in each age group 
# obtained by running "./get_tests_by_age.sh Spokane" uses "IDM_Spokane_county.csv"
#
age_diag_frac1 = c(0.0898144,0.509784,0.294782,0.10562)
age_diag_frac2 = c(0.0894982,0.46715,0.302897,0.140455)
age_diag_frac3 = c(0.117219,0.470468,0.267606,0.144707)
age_diag_frac4 = c(0.134106,0.509066,0.242785,0.114044)
age_diag_frac5 = c(0.147779,0.55036,0.218863,0.0829983)
age_diag_frac6 = c(0.149094,0.514169,0.24077,0.0959674)
age_diag_frac7 = c(0.164868,0.475335,0.252372,0.107425)
age_diag_frac8 = c(0.214862,0.47212,0.220622,0.0923963)

# rates based on time between diagnosis and hospitalization
# obtained by running "./avg_test_to_hosp.sh Spokane" uses "IDM_Spokane_county.csv"
#
hosp_rates1 = c(0, 0.210526, 0.219512, 0.191781)
hosp_rates2 = c(0, 0.2, 0.115385, 0.133333)
hosp_rates3 = c(0.2, 0.222222, 0.205128, 0.121212)
hosp_rates4 = c(1, 0.23913, 0.228571, 0.294118)
hosp_rates5 = c(0.4, 0.220183, 0.112583, 0.273585)
hosp_rates6 = c(0, 0.170213, 0.195652, 0.22449)
hosp_rates7 = c(0, 0.382979, 0.314815, 0.277778)
hosp_rates8 = c(0.097561,0.304348, 0.213235, 0.274775)


# fractions based percent in an age group that are hospitalized (based on date of hospital admission)
# obtained by running "./get_hosp_data.sh Spokane" uses "IDM_Spokane_county.csv"
#
hosp_fract1 = c(0,0.0909091,0.172043,0.5)
hosp_fract2 = c(0,0.0555556,0.212121,0.471698)
hosp_fract3 = c(0.0204082,0.04,0.209302,0.277778)
hosp_fract4 = c(0.00609756,0.0149051,0.130435,0.403509)
hosp_fract5 = c(0.00600601,0.0256992,0.102296,0.414545)
hosp_fract6 = c(0.00632911,0.0281481,0.104377,0.375887)
hosp_fract7 = c(0.00833333,0.0226782,0.0929705,0.36612)
hosp_fract8 = c(0.00842105,0.0216802,0.0977011,0.30916)

############## UNUSED FOR NOW ################################################
# rates based on time between hospitalization and death
# obtained by running "./avg_hosp_to_death.sh Spokane" uses "IDM_Spokane_county.csv"
#
hosp_death_rates1 = c(0,0,0.138889,0.11049) # based on hospitalization durations 2/1/20 - 3/31/20
hosp_death_rates2 = c(0,0.0927835,0.0801887,0.0929395) # based on hospitalization durations during April
hosp_death_rates3 = c(0,0.029703,0.0525253,0.0480864) # based on hospitalization durations during May
hosp_death_rates4 = c(0,0.333333,0.0336134,0.0438144) # based on hospitalization durations during June
hosp_death_rates5 = c(0,0.25,0.0211268,0.0413043) # based on hospitalization durations during July
hosp_death_rates6 = c(0,0.0555556,0.0296053,0.0435435) # based on hospitalization durations during Aug
hosp_death_rates7 = c(1,0.142857,0.0271318,0.0223714) # based on hospitalization durations during Sept
hosp_death_rates8 = c(1,0.142857,0.0271318,0.0223714) # based on hospitalization durations during Oct

# rates based on time between diagnosis and death
# obtained by running "rate_calc.R" with data from running scripts against "IDM_Spokane_county.csv"
#
no_hosp_death_rates1 = c(0,0.0035399298769818,0.0103044327701096,0.0579369625643052) # based on hospitalization durations 2/1/20 - 3/31/20
no_hosp_death_rates2 = c(0,0.00129603186718432,0.0108122801333226,0.0408149188052594 ) # based on hospitalization durations during April
no_hosp_death_rates3 = c(0,0.00205220123338788,0.00169868343024546,0.0390887034307249) # based on hospitalization durations during May
no_hosp_death_rates4 = c(0,0,0.00867627713559569,0.032696301006201) # based on hospitalization durations during June
no_hosp_death_rates5 = c(0,0.00294164850814613,0.00575753153406403,0.106083881466783) # based on hospitalization durations during July
no_hosp_death_rates6 = c(0,0,0.0055504479220917,0.0348837332612217) # based on hospitalization durations during Aug
no_hosp_death_rates7 = c(0,0.00597369311197608,0.00839366775483805,0.0270175222611901) # based on hospitalization durations during Sept
no_hosp_death_rates8 = c(0,0.00597369311197608,0.00839366775483805,0.0270175222611901) # based on hospitalization durations during Oct

# rates based on time between diagnosis and recovery outside of hospital (estimated using other rates)
# obtained by running "rate_calc.R" with data from running scripts against "IDM_Spokane_county.csv"
#
no_hosp_rec_rates1 = c(4.5833390000024,1.77612113147931,0.62315939018508,0.101455484323589) # based on hospitalization & deaths 2/1/20 - 3/31/20
no_hosp_rec_rates2 = c(8.833360000027,1.75563721534438,0.562055475896382,0.158862785834789) # based on hospitalization & deaths during April
no_hosp_rec_rates3 = c(11.8749745375535,1.70690135693389,0.492632611400436,0.267241846507807) # based on hospitalization & deaths during May
no_hosp_rec_rates4 = c(68.2497792007066,5.13627152550768,1.6658461731021,0.294266709055809) # based on hospitalization & deaths during June
no_hosp_rec_rates5 = c(50.4999825357203,8.32780081076119,1.18892961499755,0.597604144684499) # based on hospitalization & deaths during July
no_hosp_rec_rates6 = c(194.749958997509,6.92515956526413,0.9088864803164,0.377906933585724) # based on hospitalization & deaths during Aug
no_hosp_rec_rates7 = c(9.7078947,9.14572751571442,2.11940246149246,0.301052032515008) # based on hospitalization & deaths during Sept
no_hosp_rec_rates8 = c(9.7078947,9.14572751571442,2.11940246149246,0.301052032515008) # based on hospitalization & deaths during Oct


# fractions based percent in an age group that die after hospitalization (based on date of death)
# obtained by running "./get_out_death_data.sh Spokane" uses "IDM_Spokane_county.csv"
#
no_hosp_death_fract1 = c(0,0.00181598,0.0124708,0.14) # based on non-hospital deaths during 2/1/20 - 3/31/20
no_hosp_death_fract2 = c(0,0.000668449,0.0144033,0.095723) # based on non-hospital deaths during April
no_hosp_death_fract3 = c(0,0.00112233,0.00275482,0.0720721) # based on non-hospital deaths during May
no_hosp_death_fract4 = c(0,0,0.0045045,0.0641026) # based on non-hospital deaths during June
no_hosp_death_fract5 = c(0,0.000342466,0.00422833,0.10274) # based on non-hospital deaths during July
no_hosp_death_fract6 = c(0,0,0.00532623,0.0526316) # based on non-hospital deaths during Aug
no_hosp_death_fract7 = c(0,0.000639795,0.00359712,0.0538462) # based on non-hospital deaths during Sept
no_hosp_death_fract8 = c(0,0.000639795,0.00359712,0.0538462) # based on non-hospital deaths during Oct

# fractions based percent in an age group that die after hospitalization (based on date of death)
# obtained by running "./get_hosp_death_data.sh Spokane" uses "IDM_Spokane_county.csv"
#
hosp_death_fract1 = c(0,0.00423729,0.0342946,0.207059) # based on hospital deaths during 2/1/20 - 3/31/20
hosp_death_fract2 = c(0,0.00334225,0.0360082,0.181263) # based on hospital deaths during April
hosp_death_fract3 = c(0,0,0.0413223,0.103604) # based on hospital deaths during May
hosp_death_fract4 = c(0,0.000593824,0.0045045,0.121795) # based on hospital deaths during June
hosp_death_fract5 = c(0.00122699,0.000684932,0.012685,0.0787671) # based on hospital deaths during July
hosp_death_fract6 = c(0,0,0.00798935,0.0789474) # based on hospital deaths during Aug
hosp_death_fract7 = c(0,0.000639795,0,0) # based on hospital deaths during Sept
hosp_death_fract8 = c(0,0.000639795,0,0) # based on hospital deaths during Oct

old_age_diag_frac = c(0.098493468,0.553769558,0.245332602,0.102404372) # based on test ratios taken 9/4/20
