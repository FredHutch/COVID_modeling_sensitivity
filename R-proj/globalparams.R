# Global parameters ####5
print_debug=0

# traspose because we want rows to sum to one, the proportions of contact across age classes
c_ij = t(as.matrix(read.csv("../data/contact_matrix.csv", header = FALSE)))       # Contact matrix 

inf_presyp = 0.44            # Proportion of infections due to pre-symptomatic individuals
id = 7                       # Duration of infectiousness in days

mean_tests = function(date){
  start = yday(ymd(the_data$date[1]))
  now = yday(ymd(date))
  if (substr(date,1,4)=="2021")
	now = now + 366
  range = seq(-3,3) + (now - start + 2)
  
  print(paste(start,mean(the_data$tests1[range]),mean(the_data$tests2[range]),mean(the_data$tests3[range]),mean(the_data$tests4[range]),sep=","))
  c(mean(the_data$tests1[range]),mean(the_data$tests2[range]),mean(the_data$tests3[range]),mean(the_data$tests4[range]))
}

mean_pos = function(date){
  start = yday(ymd(the_data$date[1]))
  now = yday(ymd(date))
  if (substr(date,1,4)=="2021")
	now = now + 366
  range = seq(-3,3) + (now - start + 2)
  
  c(mean(the_data$pos1[range]),mean(the_data$pos2[range]),mean(the_data$pos3[range]),mean(the_data$pos4[range]))
}

params_fix <- list(gamma_1 = 0.33,                          # Progression rates from exposed (E) to infectious (A or P) (latent time)-1
                   gamma_2 = 0.5,                           # Progression rates from pre-symptomatic (P) to symptomatic (I) (pre-symptomatic time)-1
                   r_1 = 1/id,                              # Recovery rate of the asymptomatic cases [10-15%]
                   r_2 = 1/id,                              # Recovery rate of the mild symptomatic cases [5-6%]
                   r_3 = 1/14,                              # Recovery rate of the hospitalized cases (2 weeks)
                   p = 0.8,                                 # Proportion of the infections which become symptomatic by age
		   first_case_doy = the_first_case_doy,
		   calib_doy = yday(ymd("2020-12-31")),     # End of model calibration

                   delta1_doy = yday(ymd("2020-03-08")),    # Day to transition diagnotic rate, start social distancing , was march 5
                   delta2_doy = yday(ymd("2020-03-29")),    # Day to attain full social distancing (sd), was march 24
                   delta3_doy = yday(ymd("2020-05-01")),    # First day of monthly sd calibration...
                   delta4_doy = yday(ymd("2020-06-01")),    # 
                   delta5_doy = yday(ymd("2020-07-01")),    # 
                   delta6_doy = yday(ymd("2020-08-01")),    # 
                   delta7_doy = yday(ymd("2020-09-01")),    # 
                   delta8_doy = yday(ymd("2020-10-01")),    #
                   delta9_doy = yday(ymd("2020-11-01")),    #
                   delta10_doy = yday(ymd("2020-12-01")),    # 
                   delta11_doy = 366 + yday(ymd("2021-01-01")),    # Day to switch to dynamic SDs?
                   delta12_doy = 366 + yday(ymd("2021-02-01")),    # Day to switch to dynamic SDs?

                   DiagDate1 = yday(ymd("2020-03-15")),	    # Days by which to achieve monthly SD (starting from the 1st)
                   DiagDate2 = yday(ymd("2020-04-15")),
                   DiagDate3 = yday(ymd("2020-05-15")),
                   DiagDate4 = yday(ymd("2020-06-15")),
                   DiagDate5 = yday(ymd("2020-07-15")),
                   DiagDate6 = yday(ymd("2020-08-15")),
                   DiagDate7 = yday(ymd("2020-09-15")),
                   DiagDate8 = yday(ymd("2020-10-15")),
                   DiagDate9 = yday(ymd("2020-11-15")),
                   DiagDate10 = yday(ymd("2020-12-15")),
                   DiagDate11 = 366+yday(ymd("2021-01-15")),

                   Tests1 = mean_tests("2020-03-15"),
                   Tests2 = mean_tests("2020-04-15"),
                   Tests3 = mean_tests("2020-05-15"),
                   Tests4 = mean_tests("2020-06-15"),
                   Tests5 = mean_tests("2020-07-15"),
                   Tests6 = mean_tests("2020-08-15"),
                   Tests7 = mean_tests("2020-09-15"),
                   Tests8 = mean_tests("2020-10-15"),
                   Tests9 = mean_tests("2020-11-15"),
                   Tests10 = mean_tests("2020-12-15"),
                   Tests11 = mean_tests("2021-1-15"),
                   Pos1 = mean_pos("2020-03-15"),	    # Days at which to capture daily pose for the month
                   Pos2 = mean_pos("2020-04-15"),
                   Pos3 = mean_pos("2020-05-15"),
                   Pos4 = mean_pos("2020-06-15"),
                   Pos5 = mean_pos("2020-07-15"),
                   Pos6 = mean_pos("2020-08-15"),
                   Pos7 = mean_pos("2020-09-15"),
                   Pos8 = mean_pos("2020-10-15"),
                   Pos9 = mean_pos("2020-11-15"),
                   Pos10 = mean_pos("2020-12-15"),
                   Pos11 = mean_pos("2021-01-15"),

		   vac_final_rate = 11000,              # this one can be changed & will be adopted after schedule ends

                   hd = 11.2,                               # time from hospitalization to death (run "avg_hosp_to_death.sh" script)
                   nhd1 = 8,                               # time from diagnosis to death (without hospitalization - fitted monthly)
		   nhd2=8,nhd3=8,nhd4=8,nhd5=8,nhd6=8,
		   nhd7=8,nhd8=8,nhd9=8,nhd10=8,nhd11=8,
                   #cfr = c(0, 0.002, 0.021, 0.159),         # case fatality rate from Italy?
                   #cfr = c(0, 0.03, 0.132, 0.394),         # hospital fatality rate from DOH data
                   f_simple = TRUE,                         # true for simple mortality without sigmoid (just use fmin_i)
                   fs = 100,                                # sigmoid slope
                   icu = 3000,                              # ICU capacity
                   q_i = c(0.05, 0.054, 0.0188, 0.541),     # proportion hospitalized who need critical care
                   beta_a = 1,                              # transmission rate from asymptomatic
                   beta_p = inf_presyp / (1 - inf_presyp) * id / 2,        # transmission rate from pre-symptomatic, 2 comes from gamma_2
                   beta_s = 1,                              # transmission rate from infected
                   beta_h = 0,                              # transmission rate from hospitalized
                   c_ij = c_ij,                             # Contact matrix 
                   sd_init = c(0, 0,0,0),               # starting social distancing from 10/15 (post-calibration)
		   sd_inc=c(0,0,0,0),
		   sd_growth_trigger=0,	# % growth as tighten trigger (0 means use old policy)
		   sd_decline_trigger=0,	# % decline as loosen trigger
                   dynamic_sd = TRUE,                      # Whether to adjust sd dynamically (based on case rate)
		   dynamic_sd_limit = 100 * the_pop / 100000,		    # bi-weekly cases per 100k to stay below (add hysteresis)
		   dynamic_sd_period = 14,		    # how often to check (14 days)
		   dynamic_sd_hyster = 50 * the_pop / 100000, 		    # hysteresis: perhaps 25 * the_pop / 100000?
		   dynamic_sd_delta = 0.1,		    # adjustment to sd (larger for large hysteresis values)
		   dynamic_sd_min = 0.2,		    	    # lower limit on SD for non-seniors (0.3?)
		   dynamic_sd_min_snrs = 0.4,	    	    # lower limit on SD for seniors (0.5?)
		   dynamic_sd_max = 0.7,		    # upper limit on SD for non-seniors (0.7?)
		   dynamic_sd_max_snrs = 0.9,	    	    # upper limit on SD for seniors (0.9?)
    		   sd_trans = 14, 			    # 14-day transition to new values of social distancing (monthly)
		   severity = 1,
		   new_strain_preval=0,     # what is the prevalence of new variant at start (perc of exposures?)
                   dynamic_column = "cum_diag",             # Which column to use for the threshold
                   dynamic_threshold = 10,                  # The thresold to adjust sd
                   beta_d_fact = 1,                         # factor to multiply beta_d by for intervention scenario
                   hstar_fact = 1,                          # factor to multiply hstar by for intervention scenario
                   f_fact = 1,                              # factor to multiply f_i by for intervention scenario
                   r_3_intervention = NA,                    # new value for r_3 for intervention scenario
		   cfr1_1=0,
		   # these all get values during fitting but must be defined up front
		   rho_S1_1=0, rho_S1_2=0, rho_S1_3=0, rho_S1_4=0,      # rho_S during lockdown
		   rho_S2_1=0, rho_S2_2=0, rho_S2_3=0, rho_S2_4=0,      # rho_S during May
		   rho_S3_1=0, rho_S3_2=0, rho_S3_3=0, rho_S3_4=0,      # rho_S during June
		   rho_S4_1=0, rho_S4_2=0, rho_S4_3=0, rho_S4_4=0,      # rho_S during July
		   rho_S5_1=0, rho_S5_2=0, rho_S5_3=0, rho_S5_4=0,      # rho_S during Aug
		   rho_S6_1=0, rho_S6_2=0, rho_S6_3=0, rho_S6_4=0,      # rho_S during Sept
		   rho_S7_1=0, rho_S7_2=0, rho_S7_3=0, rho_S7_4=0,      # rho_S during Oct
		   rho_S8_1=0, rho_S8_2=0, rho_S8_3=0, rho_S8_4=0,      # rho_S during Nov
		   rho_S9_1=0, rho_S9_2=0, rho_S9_3=0, rho_S9_4=0,      # rho_S during Dec

		   d_I1_1 = 0.02, d_I1_2 = 0.05, d_I1_3 = 0.02, d_I1_4 = 0.1, 
		   d_I2_1=0, d_I2_2=0, d_I2_3=0, d_I2_4=0,      # d_I during May
		   d_I3_1=0, d_I3_2=0, d_I3_3=0, d_I3_4=0,      # d_I during June
		   d_I4_1=0, d_I4_2=0, d_I4_3=0, d_I4_4=0,      # d_I during July
		   d_I5_1=0, d_I5_2=0, d_I5_3=0, d_I5_4=0,      # d_I during Aug
		   d_I6_1=0, d_I6_2=0, d_I6_3=0, d_I6_4=0,      # d_I during Sept
		   d_I7_1=0, d_I7_2=0, d_I7_3=0, d_I7_4=0,      # d_I during Oct
		   d_I8_1=0, d_I8_2=0, d_I8_3=0, d_I8_4=0,      # d_I during Nov
		   d_I9_1=0, d_I9_2=0, d_I9_3=0, d_I9_4=0,      # d_I during Dec
                   cfr1_2 = 0.001, cfr1_3 = 0.01, cfr1_4 = 0.5,
		   cfr2_1=0, cfr2_2=0, cfr2_3=0, cfr2_4=0,      # CFR during May
		   cfr3_1=0, cfr3_2=0, cfr3_3=0, cfr3_4=0,      # CFR during June
		   cfr4_1=0, cfr4_2=0, cfr4_3=0, cfr4_4=0,      # CFR during July
		   cfr5_1=0, cfr5_2=0, cfr5_3=0, cfr5_4=0,      # CFR during Aug
		   cfr6_1=0, cfr6_2=0, cfr6_3=0, cfr6_4=0,      # CFR during Sept
		   cfr7_1=0, cfr7_2=0, cfr7_3=0, cfr7_4=0,      # CFR during Oct
		   cfr8_1=0, cfr8_2=0, cfr8_3=0, cfr8_4=0,      # CFR during Nov
		   cfr9_1=0, cfr9_2=0, cfr9_3=0, cfr9_4=0,      # CFR during Dec
		   h1_1 = 0.007, h1_2 = 0.06, h1_3 = 0.07, h1_4 = 0.1,
		   h2_1=0, h2_2=0, h2_3=0, h2_4=0,      # hospitalization rates during May
		   h3_1=0, h3_2=0, h3_3=0, h3_4=0,      # hospitalization rates during June
		   h4_1=0, h4_2=0, h4_3=0, h4_4=0,      # hospitalization rates during July
		   h5_1=0, h5_2=0, h5_3=0, h5_4=0,      # hospitalization rates during Aug
		   h6_1=0, h6_2=0, h6_3=0, h6_4=0,      # hospitalization rates during Sept
		   h7_1=0, h7_2=0, h7_3=0, h7_4=0,      # hospitalization rates during Oct
		   h8_1=0, h8_2=0, h8_3=0, h8_4=0,      # hospitalization rates during Nov
		   h9_1=0, h9_2=0, h9_3=0, h9_4=0,      # hospitalization rates during DEc
		   sd1_1=0, sd1_2=0, sd1_3=0, sd1_4=0, # Lockdown SD values
		   sd2_1=0, sd2_2=0, sd2_3=0, sd2_4=0, # May SD values
		   sd3_1=0, sd3_2=0, sd3_3=0, sd3_4=0, # June SD values
		   sd4_1=0, sd4_2=0, sd4_3=0, sd4_4=0, # July SD values
		   sd5_1=0, sd5_2=0, sd5_3=0, sd5_4=0, # Aug SD values
		   sd6_1=0, sd6_2=0, sd6_3=0, sd6_4=0, # Sept SD values
		   sd7_1=0, sd7_2=0, sd7_3=0, sd7_4=0, # Oct SD values
		   sd8_1=0, sd8_2=0, sd8_3=0, sd8_4=0, # Nov SD values
		   sd9_1=0, sd9_2=0, sd9_3=0, sd9_4=0 # Dec SD values
)
# parameters to calibrate, these are values used for inital tests with KC data
# WARNING: changes parameters here means they also need to be updated in fit.R!!!!
params_kc <- list(d_I1_1 = 0.03,                  # Relative testing rate of susceptibles
		  d_I1_2 = 0.01, 
		  d_I1_3 = 0.01, 
		  d_I1_4 = 0.01, 
		  h1_1 = 0.005, 
		  h1_2 = 0.02, 
		  h1_3 = 0.05,
		  h1_4 = 0.15,
		  bstar=0.2,
		  sd1_1 = 0.5,                  # Social distancing (relaxed fit by age) May 15 - July 15th
                  sd1_2 = 0.7,
                  sd1_3 = 0.7,
                  sd1_4 = 0.9,
                  beta_d = 0.75,              # transmission rate from diagnosed
                  delta0offset = 49.80,         # number of days to add before detected first case
                  cfr1_2 = 0.01,
                  cfr1_3 = 0.02,
                  cfr1_4 = 0.5
)

params_kc_lower <- c(d_I1_1 = 0.001, 
		  d_I1_2 = 0.001, 
		  d_I1_3 = 0.001, 
		  d_I1_4 = 0.001, 
		  h1_1 = 0.005, 
		  h1_2 = 0.01, 
		  h1_3 = 0.01, 
		  h1_4 = 0.05, 
                     bstar = 2.2 / (params_fix$beta_p / params_fix$gamma_2 + id), # take R0 = 3
		     sd1_1 = 0.1,                  # Social distancing (relaxed fit by age) May 15 - July 15th
		     sd1_2 = 0.2,
		     sd1_3 = 0.2,
		     sd1_4 = 0.4,
                     beta_d = 0.5, 
                     delta0offset = 40,
                     cfr1_2 = 0.01,
                     cfr1_3 = 0.01,
                     cfr1_4 = 0.05
)
params_kc_upper <- c(d_I1_1 = 0.1,
		  d_I1_2 = 0.1, 
		  d_I1_3 = 0.1, 
		  d_I1_4 = 0.1, 
		  h1_1 = 0.1, 
		  h1_2 = 0.2, 
		  h1_3 = 0.3, 
		  h1_4 = 0.5, 
                     bstar = 2.5 / (params_fix$beta_p / params_fix$gamma_2 + id), # take R0 = 5
		     sd1_1 = 0.5,                  # Social distancing (relaxed fit by age) May 15 - July 15th
		     sd1_2 = 0.7,
		     sd1_3 = 0.7,
		     sd1_4 = 0.9,
                     beta_d = 0.75, 
                     delta0offset = 50,
                     cfr1_2 = 0.1,
                     cfr1_3 = 0.1,
                     cfr1_4 = 0.6
)


# Set starting infected and susceptible counts ####

# note that matching to order here and in the model definition is critical and thus we define indices
# unfortunately can't pass as list of vectors like parameters or ode will only allow returning 
# results the same length as the list

# indicies into state vector

# shared for multiple strain implementation
S_idx = 1:4
VS_idx = S_idx+4; 

# update these for both strains
tot_tests_idx=VS_idx+4
Vtot_idx=tot_tests_idx+4
tot_diag_idx=Vtot_idx+4
tot_deaths_idx=tot_diag_idx+4
tot_inf_idx=tot_deaths_idx+4
tot_hosp_idx=tot_inf_idx+4
VEX_idx=tot_hosp_idx+4
age_pop_idx=VEX_idx+4
shared_entries=age_pop_idx[length(age_pop_idx)]

sd_adj_idx=age_pop_idx+4
adj_once_entries=sd_adj_idx[length(sd_adj_idx)]-shared_entries

#These will be filled in for each strain (indices calculated in the model_eq_set routine)
E_idx = sd_adj_idx+4; A_idx = E_idx+4; P_idx = A_idx+4; 
I_idx = P_idx+4; D_idx = I_idx+4; DA_idx = D_idx+4; 
H_idx = DA_idx+4; R_idx = H_idx+4; F_idx = R_idx+4;
cum_exp_idx = F_idx+4; cum_asym_idx = cum_exp_idx+4; cum_sym_idx = cum_asym_idx+4; 
cum_diag_idx = cum_sym_idx+4; cum_hosp_idx = cum_diag_idx+4; 

# indicies into state vector for non-vaccinated
VE_idx = cum_hosp_idx+4; VA_idx = VE_idx+4; VP_idx = VA_idx+4; 
VI_idx = VP_idx+4; VD_idx = VI_idx+4; VDA_idx = VD_idx+4; 
VH_idx = VDA_idx+4; VR_idx = VH_idx+4; VF_idx = VR_idx+4;
Vcum_exp_idx = VF_idx+4; Vcum_asym_idx = Vcum_exp_idx+4; Vcum_sym_idx = Vcum_asym_idx+4; 
Vcum_diag_idx = Vcum_sym_idx+4; Vcum_hosp_idx = Vcum_diag_idx+4; 
strainwise_entries=Vcum_hosp_idx[length(Vcum_hosp_idx)]-(shared_entries+adj_once_entries)

#These will be filled in for strain 2 (indices calculated in the model_eq_set routine)
E2_idx = Vcum_hosp_idx+4; A2_idx = E2_idx+4; P2_idx = A2_idx+4; 
I2_idx = P2_idx+4; D2_idx = I2_idx+4; DA2_idx = D2_idx+4; 
H2_idx = DA2_idx+4; R2_idx = H2_idx+4; F2_idx = R2_idx+4;
cum_exp2_idx = F2_idx+4; cum_asym2_idx = cum_exp2_idx+4; cum_sym2_idx = cum_asym2_idx+4; 
cum_diag2_idx = cum_sym2_idx+4; cum_hosp2_idx = cum_diag2_idx+4; 


vac_stop_doy = 365 + yday(ymd("2021-1-01"))     # Vaccinate for 1 year

vac_on = 0# no vaccine by default
vac_rate = 0
vac_dist_i = the_age_prop
vac_first = 0
vac_exp_rate=0
vac_coverage=0.8
expon_imports=0

vac_eff_susc=0.7	# reduction in susceptibility
vac_eff_susc1=vac_eff_susc

vac_eff_pi=0.5	# reduction in sympt rate
vac_eff_pi1=vac_eff_pi

vac_eff_hi=0	# reduction in hospitalization rate
vac_eff_hi1=vac_eff_hi

vac_eff_inf=0.5	# reduction in infection rate
vac_eff_inf1=vac_eff_inf

vac_eff_pi2=0
vac_eff_susc2=0
vac_eff_inf2=0
vac_eff_hi2=0

new_strain_fact=1	# multiplier for 2nd strain betas
new_strain_severity=c(1,1,1,1)   # the increase in disease severity due to the new variant (none for now)
new_strain_intros=5	# daily arrival rate for new strain infections from out-of-county (mult w/ age-dist)
inf_intros=c(0,0,0,0)	# daily arrival rate for new infections from out-of-county (any strain)
vac_mutate=0
vac_mutate_time=0

new_check_date=366+yday(ymd("2021-01-15")) # date for switch from case triggers to percent change in cases/hospitalizations
ode_step_time=1

vac_irresp=0	# do vaccinees not socially distance

vac_schedule = matrix(c(366+yday(ymd("2021-1-15")),2000,     # Start of vaccination protection (1st point)
     366+yday(ymd("2021-2-5")),8600,     # vaccination protection (2nd point)
     366+yday(ymd("2021-3-7")),3700,     # vaccination protection (3rd point)
     366+yday(ymd("2021-3-17")),10000,     # vaccination protection (4th point)
     366+yday(ymd("2021-3-29")),12000,     # vaccination protection (5th point)
     366+yday(ymd("2021-4-02")),11000),     # vaccination protection (6th point)
                       byrow = TRUE, nrow = 6)

state      <- c(S_i = the_pop * the_age_prop - c(0, 2, 2, 0), # Starting susceptible population by age bucket
		VS_i = c(0, 0, 0, 0), 			     # Starting vaccinated population by age bucket
		tot_tests_i = c(0,0,0,0),		     # total tests including pos & negs
                Vtot_i = c(0, 0, 0, 0),
		tot_diag_i = c(0,0,0,0),		     # total cases including vaccinated ones
		tot_deaths_i = c(0,0,0,0),		     # total deaths including vaccinated ones
		tot_Inf_i = c(0,0,0,0),			     # total infections including vaccinated ones
		tot_hosp_i = c(0,0,0,0),		     # total hospitalization including vaccinated ones
                VEX_i = c(0, 0, 0, 0),                        # Starting vaccine expirees
		N_i = the_pop * the_age_prop,	    # starting pop by age group

                sd_adj_i = params_fix$sd_init,               # starting social distancing from 9/1 (back to school)

                E_i = c(0, 2, 2, 0),                        # Starting Exposed
                A_i = c(0, 0, 0, 0),                        # Starting Asymptomatic
                P_i = c(0, 0, 0, 0),                        # Starting Pre-symptomatic
                I_i = c(0, 0, 0, 0),                        # Starting number of Infected by age bucket
                D_i = c(0, 0, 0, 0),                        # Starting Diagnosed
                DA_i = c(0, 0, 0, 0),                       # Starting Diagnosed Asymptomatic
                H_i = c(0, 0, 0, 0),                        # Starting Hospitalized
                R_i = c(0, 0, 0, 0),                        # Starting Recovered
                F_i = c(0, 0, 0, 0),                        # Starting Deaths
                cum_exp_i = c(0, 0, 0, 0),
                cum_asym_i = c(0, 0, 0, 0),
                cum_sym_i = c(0, 0, 0, 0),
                cum_diag_i = c(0, 0, 0, 0),
                cum_hosp_i = c(0, 0, 0, 0),
                VE_i = c(0, 0, 0, 0),                        # Starting vaccinated Exposed
                VA_i = c(0, 0, 0, 0),                        # Starting vaccinated Asymptomatic
                VP_i = c(0, 0, 0, 0),                        # Starting vaccinated Pre-symptomatic
                VI_i = c(0, 0, 0, 0),                        # Starting vaccinated number of Infected by age bucket
                VD_i = c(0, 0, 0, 0),                        # Starting vaccinated Diagnosed
                VDA_i = c(0, 0, 0, 0),                       # Starting vaccinated Diagnosed Asymptomatic
                VH_i = c(0, 0, 0, 0),                        # Starting vaccinated Hospitalized
                VR_i = c(0, 0, 0, 0),                        # Starting vaccinated Recovered
                VF_i = c(0, 0, 0, 0),                        # Starting vaccinated Deaths
                Vcum_exp_i = c(0, 0, 0, 0),
                Vcum_asym_i = c(0, 0, 0, 0),
                Vcum_sym_i = c(0, 0, 0, 0),
                Vcum_diag_i = c(0, 0, 0, 0),
                Vcum_hosp_i = c(0, 0, 0, 0),
#
# This state section is for a second strain (set vac_mutate=1 and vac_mutate_time)
#
                E2_i = c(0, 1, 0, 0),                        # Starting Exposed for 2nd strain by age bucket
                A2_i = c(0, 0, 0, 0),                        # Starting Asymptomatic
                P2_i = c(0, 0, 0, 0),                        # Starting Pre-symptomatic
                I2_i = c(0, 0, 0, 0),                        # Starting number of Infected by age bucket
                D2_i = c(0, 0, 0, 0),                        # Starting Diagnosed
                DA2_i = c(0, 0, 0, 0),                       # Starting Diagnosed Asymptomatic
                H2_i = c(0, 0, 0, 0),                        # Starting Hospitalized
                R2_i = c(0, 0, 0, 0),                        # Starting Recovered
                F2_i = c(0, 0, 0, 0),                        # Starting Deaths
                cum_exp2_i = c(0, 0, 0, 0),
                cum_asym2_i = c(0, 0, 0, 0),
                cum_sym2_i = c(0, 0, 0, 0),
                cum_diag2_i = c(0, 0, 0, 0),
                cum_hosp2_i = c(0, 0, 0, 0),
                VE2_i = c(0, 0, 0, 0),                        # Starting vaccinated Exposed
                VA2_i = c(0, 0, 0, 0),                        # Starting vaccinated Asymptomatic
                VP2_i = c(0, 0, 0, 0),                        # Starting vaccinated Pre-symptomatic
                VI2_i = c(0, 0, 0, 0),                        # Starting vaccinated number of Infected by age bucket
                VD2_i = c(0, 0, 0, 0),                        # Starting vaccinated Diagnosed
                VDA2_i = c(0, 0, 0, 0),                       # Starting vaccinated Diagnosed Asymptomatic
                VH2_i = c(0, 0, 0, 0),                        # Starting vaccinated Hospitalized
                VR2_i = c(0, 0, 0, 0),                        # Starting vaccinated Recovered
                VF2_i = c(0, 0, 0, 0),                        # Starting vaccinated Deaths
                Vcum_exp2_i = c(0, 0, 0, 0),
                Vcum_asym2_i = c(0, 0, 0, 0),
                Vcum_sym2_i = c(0, 0, 0, 0),
                Vcum_diag2_i = c(0, 0, 0, 0),
                Vcum_hosp2_i = c(0, 0, 0, 0)
)


# Set length of simulation ####
times      <- seq(0, 720, by = 1)
