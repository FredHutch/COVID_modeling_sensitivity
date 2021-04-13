# Model fitting
# Author: Dave Swan
# Initial model authors: Chloe Bracis, Mia Moore and Dobromir Dimitrov
#
# This file controls the fitting of the model parameters to King County data.
# Fitting is done first for the lockdonw period (start of pandemic through April 30th)
# The 1st fitting includes general parameters to set main strain infectivity, infectivity reduction for those diagnosed
# and alignment of 1st case to calendar date of 1st case in King County.
#
# Subsequent fits are done monthly with each parameter tuned to the age-specific cases, deaths and hospital admissions
# for that month.  The version takes a painstaking approach to the monthly fittings with new parameter names for each
# month's settings.  The next version cleans this up and does the fittying in a more generic, scalable fashion.
#
#
library(lubridate)
library(mco)

setwd("..")	# run in the R-proj directory

source("covid-model.R")

# Read in King County actual data for calibration ####
source("kc_read-data.R")

#we model just one "intervention" starting May 15th (isolation of diagnosted, symptomatic individuals)

intervention_day = yday(ymd("2020-5-15"))     # Start of intervention protocol
int_rampup = 14				      # Time to achieve full intervention effect

intervention_abbr = "TI_S+C"
params_fix$beta_d_fact= 0.5

# skip dynamic social distancing during calibration (done in projection mode)
params_fix$dynamic_sd = F

# truncate calibration data to just include lockdown period initially...
# subsequent calibrations done monthly w/ new variable names (cleaned up in version 2)
#
end_day = yday("2020-4-30")

# data off for calibration is May 1st (lockdown)
end_idx = length(the_data$date)
for (i in 1:length(the_data$date)) 
{
    if (the_data$date[i] == "2020-04-30")
    {
	end_idx=i
	print(paste("end_idx=",i))
    }
}
the_data_calib = the_data[1:end_idx,]

# set suffix for output data and plot files
suffix="lockdown_fit"

# do not include vaccinations
vac_on = 0
vac_rate = 0
vac_first = 0

# parameters to calibrate, these are values used for inital tests with KC data
# WARNING: changes parameters here means they also need to be updated in fit.R!!!!
#
# parameters to calibrate, these are values used for inital tests with KC data
# WARNING: changes parameters here means they also need to be updated in fit.R!!!!
#
params_kc <- list(rho_S1_1 = 0.002,                  # Relative testing rate of susceptibles
		  rho_S1_2 = 0.005, 
		  rho_S1_3 = 0.002, 
		  rho_S1_4 = 0.001, 
		  h1_1 = 0.005, 
		  h1_2 = 0.03, 
		  h1_3 = 0.05,
		  h1_4 = 0.08,
		  bstar=0.2,
		  sd1_1 = 0.5,                  # Social distancing (relaxed fit by age) 
                  sd1_2 = 0.7,
                  sd1_3 = 0.7,
                  sd1_4 = 0.9,
                  beta_d = 0.6,              # transmission rate from diagnosed
                  delta0offset = 50,         # number of days to add before detected first case
                  cfr1_2 = 0.001,
                  cfr1_3 = 0.02,
                  cfr1_4 = 0.25
)

#lower bounds on all lockdown settings
params_kc_lower <- c(rho_S1_1 = 0.001, 
		  rho_S1_2 = 0.001, 
		  rho_S1_3 = 0.001, 
		  rho_S1_4 = 0.001, 
		  h1_1 = 0.005, 
		  h1_2 = 0.001, 
		  h1_3 = 0.001, 
		  h1_4 = 0.001, 
		  bstar = 2.2 / (params_fix$beta_p / params_fix$gamma_2 + id), # take R0 = 3
		  sd1_1 = 0.1,                  # Social distancing (relaxed fit by age) 
		  sd1_2 = 0.1,
		  sd1_3 = 0.1,
		  sd1_4 = 0.4,
		  beta_d = 0.5, 
		  delta0offset = 45,
		  cfr1_2 = 0,
		  cfr1_3 = 0,
		  cfr1_4 = 0.05
)
#upper bounds on all lockdown settings
params_kc_upper <- c(rho_S1_1 = 0.2,
		  rho_S1_2 = 0.2, 
		  rho_S1_3 = 0.2, 
		  rho_S1_4 = 0.2, 
		  h1_1 = 0.1, 
		  h1_2 = 0.2, 
		  h1_3 = 0.3, 
		  h1_4 = 0.5, 
		  bstar = 2.5 / (params_fix$beta_p / params_fix$gamma_2 + id), # take R0 = 5
		  sd1_1 = 0.5,                  # Social distancing (relaxed fit by age) 
                  sd1_2 = 0.7,
                  sd1_3 = 0.7,
                  sd1_4 = 0.9,
		  beta_d = 0.75, 
		  delta0offset = 55,
		  cfr1_2 = 0.1,
		  cfr1_3 = 0.1,
		  cfr1_4 = 0.5
)
print(params_fix)

# Model calibration by sum of squared errors (not used) ####
# Calculate error at starting values and bounds
# This call & subsequent plots are for informational purposes only
#
sse=calc_sse_multi(unlist(params_kc), names(params_kc), params_fix, the_data_calib, kc_age_t)
print("For initial values...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))

pdf(paste0("calibration/initial_fit_",suffix,".pdf"), width = 10, height = 5)
plot_calibration_params(unlist(params_kc), names(params_kc), params_fix, the_data_calib, kc_age_t,end_day, state)
plot_age_case_fit(unlist(params_kc), names(params_kc), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_death_fit(unlist(params_kc), names(params_kc), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_hosp_fit(unlist(params_kc), names(params_kc), params_fix, the_data_calib, kc_age_t, end_day, state)
dev.off()

sse=calc_sse_multi(params_kc_lower, names(params_kc_lower), params_fix, the_data_calib, kc_age_t)
print("For lower bounds...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))

sse=calc_sse_multi(params_kc_upper, names(params_kc_upper), params_fix, the_data_calib, kc_age_t)
print("For upper bounds...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))

Sys.time()
# Optimization done using BFGS algorithm
res = optim(par = unlist(params_kc),
            fn = calc_sse_simple,
            gr = NULL,
            names(params_kc), params_fix, the_data_calib, kc_age_t, 
            method = "L-BFGS-B",
	    control = c(maxit=1000,trace=2),
           lower = params_kc_lower,
            upper = params_kc_upper)
# Calibrated parameters
Sys.time()
res$par

# Calculate sum of squared errors with calibrated parameters
sse=calc_sse_multi(res$par, names(res$par), params_fix, the_data_calib, kc_age_t)
print("For best L-BFGS-B fit...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))
score_bfgs = sse[1]+sse[2]+sse[3]

# plot the parameter outcome that has the lowest mean squared error against actuals (cases and deaths)
pdf(paste0("calibration/",suffix,".pdf"), width = 10, height = 5)
plot_calibration_params(res$par, names(res$par), params_fix, the_data_calib, kc_age_t,end_day, state)
plot_age_case_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_death_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_hosp_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
dev.off()

params_fix = get_params(res$par, names(res$par), params_fix)
save(params_fix,res, file = paste0("calibration/res_test_",suffix,".Rdata"))
#load(file = paste0("calibration/res_test_",suffix,".Rdata"))

# When moving to the next fitting period, our starting settings for rhoS, SD, h_i & cfr are those of the previous month

params_kc <- list(rho_S2_1 = params_fix$rho_S1_1,                  # Relative testing rate of susceptibles
		  rho_S2_2 = params_fix$rho_S1_2, 
		  rho_S2_3 = params_fix$rho_S1_3, 
		  rho_S2_4 = params_fix$rho_S1_4, 
		  sd2_1 = params_fix$sd1_1,                  # Social distancing (relaxed fit by age) 
                  sd2_2 = params_fix$sd1_2,
                  sd2_3 = params_fix$sd1_3,
                  sd2_4 = params_fix$sd1_4,
		  h2_1 = params_fix$h1_1, 		# hospitalization rates among severly infected
		  h2_2 = params_fix$h1_2, 
		  h2_3 = params_fix$h1_3,
		  h2_4 = params_fix$h1_4,
                  cfr2_2 = params_fix$cfr1_2,		# case fatality - applied to both in/out of hospital, severly infected
                  cfr2_3 = params_fix$cfr1_3,		# children have not died so their age group CFR is always 0
                  cfr2_4 = params_fix$cfr1_4
)

params_kc_lower <- list(rho_S2_1 = 0.001,
		  rho_S2_2 = 0.001, 
		  rho_S2_3 = 0.001, 
		  rho_S2_4 = 0.001, 
		  sd2_1 = 0,                  # Social distancing (relaxed fit by age) 
                  sd2_2 = 0.2,
                  sd2_3 = 0.2,
                  sd2_4 = 0.4,
		  h2_1 = 0.005, 
		  h2_2 = 0.001, 
		  h2_3 = 0.001, 
		  h2_4 = 0.001, 
		  cfr2_2 = 0,
		  cfr2_3 = 0,
		  cfr2_4 = 0.05
)
params_kc_upper <- c(rho_S2_1 = 0.2,
		  rho_S2_2 = 0.2, 
		  rho_S2_3 = 0.2, 
		  rho_S2_4 = 0.2, 
		  sd2_1 = 0.6,                  # Social distancing (relaxed fit by age) 
                  sd2_2 = 0.6,
                  sd2_3 = 0.6,
                  sd2_4 = 0.8,
		  h2_1 = 0.1, 
		  h2_2 = 0.2, 
		  h2_3 = 0.3, 
		  h2_4 = 0.5, 
		  cfr2_2 = 0.1,
		  cfr2_3 = 0.1,
		  cfr2_4 = 0.5
)
end_day = yday("2020-5-31")

# data off for calibration is May 31 ( end of lockdown)
end_idx = length(the_data$date)
for (i in 1:length(the_data$date)) 
{
    if (the_data$date[i] == "2020-05-31")
    {
	end_idx=i
	print(paste("end_idx=",i))
    }
}
the_data_calib = the_data[1:end_idx,]
suffix="May_fit"

Sys.time()
# Optimization
res = optim(par = unlist(params_kc),
            fn = calc_sse_simple,
            gr = NULL,
            names(params_kc), params_fix, the_data_calib, kc_age_t, 
            method = "L-BFGS-B",
	    control = c(maxit=1000,trace=2),
            lower = params_kc_lower,
            upper = params_kc_upper)
# Calibrated parameters
Sys.time()
res$par

# Calculate sum of squared errors with calibrated parameters
sse=calc_sse_multi(res$par, names(res$par), params_fix, the_data_calib, kc_age_t)
print("For best L-BFGS-B fit...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))
score_bfgs = sse[1]+sse[2]+sse[3]

# plot the parameter outcome that has the lowest mean squared error against actuals (cases and deaths)
pdf(paste0("calibration/",suffix,".pdf"), width = 10, height = 5)
plot_calibration_params(res$par, names(res$par), params_fix, the_data_calib, kc_age_t,end_day, state)
plot_age_case_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_death_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_hosp_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
dev.off()

params_fix = get_params(res$par, names(res$par), params_fix)
save(params_fix,res, file = paste0("calibration/res_test_",suffix,".Rdata"))

params_kc <- list(rho_S3_1 = params_fix$rho_S2_1,                  # Relative testing rate of susceptibles
		  rho_S3_2 = params_fix$rho_S2_2, 
		  rho_S3_3 = params_fix$rho_S2_3, 
		  rho_S3_4 = params_fix$rho_S2_4, 
		  sd3_1 = params_fix$sd2_1,                  # Social distancing (relaxed fit by age) 
                  sd3_2 = params_fix$sd2_2,
                  sd3_3 = params_fix$sd2_3,
                  sd3_4 = params_fix$sd2_4,
		  h3_1 = params_fix$h2_1, 
		  h3_2 = params_fix$h2_2, 
		  h3_3 = params_fix$h2_3,
		  h3_4 = params_fix$h2_4,
                  cfr3_2 = params_fix$cfr2_2,
                  cfr3_3 = params_fix$cfr2_3,
                  cfr3_4 = params_fix$cfr2_4
)

params_kc_lower <- c(rho_S3_1 = 0.001, 
		  rho_S3_2 = 0.001, 
		  rho_S3_3 = 0.001, 
		  rho_S3_4 = 0.001, 
		  sd3_1 = 0,                  # Social distancing (relaxed fit by age) 
                  sd3_2 = 0.2,
                  sd3_3 = 0.2,
                  sd3_4 = 0.4,
		  h3_1 = 0.005, 
		  h3_2 = 0.001, 
		  h3_3 = 0.001, 
		  h3_4 = 0.001, 
		  cfr3_2 = 0,
		  cfr3_3 = 0,
		  cfr3_4 = 0.05
)
params_kc_upper <- c(rho_S3_1 = 0.2,
		  rho_S3_2 = 0.2, 
		  rho_S3_3 = 0.2, 
		  rho_S3_4 = 0.2, 
		  sd3_1 = 0.6,                  # Social distancing (relaxed fit by age) 
                  sd3_2 = 0.6,
                  sd3_3 = 0.6,
                  sd3_4 = 0.8,
		  h3_1 = 0.1, 
		  h3_2 = 0.2, 
		  h3_3 = 0.3, 
		  h3_4 = 0.5, 
		  cfr3_2 = 0.1,
		  cfr3_3 = 0.1,
		  cfr3_4 = 0.5
)
end_day = yday("2020-6-30")

end_idx = length(the_data$date)

for (i in 1:length(the_data$date)) 
{
    if (the_data$date[i] == "2020-06-30")
    {
	end_idx=i
	print(paste("end_idx=",i))
    }
}
the_data_calib = the_data[1:end_idx,]
suffix="June_fit"

Sys.time()
# Optimization
res = optim(par = unlist(params_kc),
            fn = calc_sse_simple,
            gr = NULL,
            names(params_kc), params_fix, the_data_calib, kc_age_t, 
            method = "L-BFGS-B",
	    control = c(maxit=1000,trace=2),
            lower = params_kc_lower,
            upper = params_kc_upper)
# Calibrated parameters
Sys.time()
res$par

# Calculate sum of squared errors with calibrated parameters
sse=calc_sse_multi(res$par, names(res$par), params_fix, the_data_calib, kc_age_t)
print("For best L-BFGS-B fit...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))
score_bfgs = sse[1]+sse[2]+sse[3]

# plot the parameter outcome that has the lowest mean squared error against actuals (cases and deaths)
pdf(paste0("calibration/",suffix,".pdf"), width = 10, height = 5)
plot_calibration_params(res$par, names(res$par), params_fix, the_data_calib, kc_age_t,end_day, state)
plot_age_case_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_death_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_hosp_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
dev.off()

params_fix = get_params(res$par, names(res$par), params_fix)
save(params_fix,res, file = paste0("calibration/res_test_",suffix,".Rdata"))

params_kc <- list(rho_S4_1 = params_fix$rho_S3_1,                  # Relative testing rate of susceptibles
		  rho_S4_2 = params_fix$rho_S3_2, 
		  rho_S4_3 = params_fix$rho_S3_3, 
		  rho_S4_4 = params_fix$rho_S3_4, 
		  sd4_1 = params_fix$sd3_1,                  # Social distancing (relaxed fit by age) 
                  sd4_2 = params_fix$sd3_2,
                  sd4_3 = params_fix$sd3_3,
                  sd4_4 = params_fix$sd3_4,
		  h4_1 = params_fix$h3_1, 
		  h4_2 = params_fix$h3_2, 
		  h4_3 = params_fix$h3_3,
		  h4_4 = params_fix$h3_4,
                  cfr4_2 = params_fix$cfr3_2,
                  cfr4_3 = params_fix$cfr3_3,
                  cfr4_4 = params_fix$cfr3_4
)

params_kc_lower <- c(rho_S4_1 = 0.001, 
		  rho_S4_2 = 0.001, 
		  rho_S4_3 = 0.001, 
		  rho_S4_4 = 0.001, 
		  sd4_1 = 0,                  # Social distancing (relaxed fit by age) 
                  sd4_2 = 0.2,
                  sd4_3 = 0.2,
                  sd4_4 = 0.4,
		  h4_1 = 0.005, 
		  h4_2 = 0.001, 
		  h4_3 = 0.001, 
		  h4_4 = 0.001, 
		  cfr4_2 = 0,
		  cfr4_3 = 0,
		  cfr4_4 = 0.05
)
params_kc_upper <- c(rho_S4_1 = 0.2,
		  rho_S4_2 = 0.2, 
		  rho_S4_3 = 0.2, 
		  rho_S4_4 = 0.2, 
		  sd4_1 = 0.6,                  # Social distancing (relaxed fit by age) 
                  sd4_2 = 0.6,
                  sd4_3 = 0.6,
                  sd4_4 = 0.8,
		  h4_1 = 0.1, 
		  h4_2 = 0.2, 
		  h4_3 = 0.3, 
		  h4_4 = 0.5, 
		  cfr4_2 = 0.1,
		  cfr4_3 = 0.1,
		  cfr4_4 = 0.5
)
end_day = yday("2020-7-31")

end_idx = length(the_data$date)
for (i in 1:length(the_data$date)) 
{
    if (the_data$date[i] == "2020-07-31")
    {
	end_idx=i
	print(paste("end_idx=",i))
    }
}
the_data_calib = the_data[1:end_idx,]
suffix="July_fit"

Sys.time()
# Optimization
res = optim(par = unlist(params_kc),
            fn = calc_sse_simple,
            gr = NULL,
            names(params_kc), params_fix, the_data_calib, kc_age_t, 
            method = "L-BFGS-B",
	    control = c(maxit=1000,trace=2),
            lower = params_kc_lower,
            upper = params_kc_upper)
# Calibrated parameters
Sys.time()
res$par

# Calculate sum of squared errors with calibrated parameters
sse=calc_sse_multi(res$par, names(res$par), params_fix, the_data_calib, kc_age_t)
print("For best L-BFGS-B fit...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))
score_bfgs = sse[1]+sse[2]+sse[3]

# plot the parameter outcome that has the lowest mean squared error against actuals (cases and deaths)
pdf(paste0("calibration/",suffix,".pdf"), width = 10, height = 5)
plot_calibration_params(res$par, names(res$par), params_fix, the_data_calib, kc_age_t,end_day, state)
plot_age_case_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_death_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_hosp_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
dev.off()
params_fix = get_params(res$par, names(res$par), params_fix)
save(params_fix,res, file = paste0("calibration/res_test_",suffix,".Rdata"))

params_kc <- list(rho_S5_1 = params_fix$rho_S4_1,                  # Relative testing rate of susceptibles
		  rho_S5_2 = params_fix$rho_S4_2, 
		  rho_S5_3 = params_fix$rho_S4_3, 
		  rho_S5_4 = params_fix$rho_S4_4, 
		  sd5_1 = params_fix$sd4_1,                  # Social distancing (relaxed fit by age) 
                  sd5_2 = params_fix$sd4_2,
                  sd5_3 = params_fix$sd4_3,
                  sd5_4 = params_fix$sd4_4,
		  h5_1 = params_fix$h4_1, 
		  h5_2 = params_fix$h4_2, 
		  h5_3 = params_fix$h4_3,
		  h5_4 = params_fix$h4_4,
                  cfr5_2 = params_fix$cfr4_2,
                  cfr5_3 = params_fix$cfr4_3,
                  cfr5_4 = params_fix$cfr4_4
)

params_kc_lower <- c(rho_S5_1 = 0.001, 
		  rho_S5_2 = 0.001, 
		  rho_S5_3 = 0.001, 
		  rho_S5_4 = 0.001, 
		  sd5_1 = 0,                  # Social distancing (relaxed fit by age) 
                  sd5_2 = 0.2,
                  sd5_3 = 0.2,
                  sd5_4 = 0.4,
		  h5_1 = 0.005, 
		  h5_2 = 0.001, 
		  h5_3 = 0.001, 
		  h5_4 = 0.001, 
		  cfr5_2 = 0,
		  cfr5_3 = 0,
		  cfr5_4 = 0.05
)
params_kc_upper <- c(rho_S5_1 = 0.2,
		  rho_S5_2 = 0.2, 
		  rho_S5_3 = 0.2, 
		  rho_S5_4 = 0.2, 
		  sd5_1 = 0.6,                  # Social distancing (relaxed fit by age) 
                  sd5_2 = 0.6,
                  sd5_3 = 0.6,
                  sd5_4 = 0.8,
		  h5_1 = 0.1, 
		  h5_2 = 0.2, 
		  h5_3 = 0.3, 
		  h5_4 = 0.5, 
		  cfr5_2 = 0.1,
		  cfr5_3 = 0.1,
		  cfr5_4 = 0.5
)
end_day = yday("2020-8-31")

end_idx = length(the_data$date)
for (i in 1:length(the_data$date)) 
{
    if (the_data$date[i] == "2020-08-31")
    {
	end_idx=i
	print(paste("end_idx=",i))
    }
}
the_data_calib = the_data[1:end_idx,]
suffix="Aug_fit"

Sys.time()
# Optimization
res = optim(par = unlist(params_kc),
            fn = calc_sse_simple,
            gr = NULL,
            names(params_kc), params_fix, the_data_calib, kc_age_t, 
            method = "L-BFGS-B",
	    control = c(maxit=1000,trace=2),
            lower = params_kc_lower,
            upper = params_kc_upper)
# Calibrated parameters
Sys.time()
res$par

# Calculate sum of squared errors with calibrated parameters
sse=calc_sse_multi(res$par, names(res$par), params_fix, the_data_calib, kc_age_t)
print("For best L-BFGS-B fit...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))
score_bfgs = sse[1]+sse[2]+sse[3]

# plot the parameter outcome that has the lowest mean squared error against actuals (cases and deaths)
pdf(paste0("calibration/",suffix,".pdf"), width = 10, height = 5)
plot_calibration_params(res$par, names(res$par), params_fix, the_data_calib, kc_age_t,end_day, state)
plot_age_case_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_death_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_hosp_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
dev.off()

params_fix = get_params(res$par, names(res$par), params_fix)
save(params_fix,res, file = paste0("calibration/res_test_",suffix,".Rdata"))

params_kc <- list(rho_S6_1 = params_fix$rho_S5_1,                  # Relative testing rate of susceptibles
		  rho_S6_2 = params_fix$rho_S5_2, 
		  rho_S6_3 = params_fix$rho_S5_3, 
		  rho_S6_4 = params_fix$rho_S5_4, 
		  sd6_1 = params_fix$sd5_1,                  # Social distancing (relaxed fit by age) 
                  sd6_2 = params_fix$sd5_2,
                  sd6_3 = params_fix$sd5_3,
                  sd6_4 = params_fix$sd5_4,
		  h6_1 = params_fix$h5_1, 
		  h6_2 = params_fix$h5_2, 
		  h6_3 = params_fix$h5_3,
		  h6_4 = params_fix$h5_4,
                  cfr6_2 = params_fix$cfr5_2,
                  cfr6_3 = params_fix$cfr5_3,
                  cfr6_4 = params_fix$cfr5_4
)

params_kc_lower <- c(rho_S6_1 = 0.001, 
		  rho_S6_2 = 0.001, 
		  rho_S6_3 = 0.001, 
		  rho_S6_4 = 0.001, 
		  sd6_1 = 0,                  # Social distancing (relaxed fit by age) 
                  sd6_2 = 0.2,
                  sd6_3 = 0.2,
                  sd6_4 = 0.4,
		  h6_1 = 0.005, 
		  h6_2 = 0.001, 
		  h6_3 = 0.001, 
		  h6_4 = 0.001, 
		  cfr6_2 = 0,
		  cfr6_3 = 0,
		  cfr6_4 = 0.05
)
params_kc_upper <- c(rho_S6_1 = 0.2,
		  rho_S6_2 = 0.2, 
		  rho_S6_3 = 0.2, 
		  rho_S6_4 = 0.2, 
		  sd6_1 = 0.6,                  # Social distancing (relaxed fit by age) 
                  sd6_2 = 0.6,
                  sd6_3 = 0.6,
                  sd6_4 = 0.8,
		  h6_1 = 0.1, 
		  h6_2 = 0.2, 
		  h6_3 = 0.3, 
		  h6_4 = 0.5, 
		  cfr6_2 = 0.1,
		  cfr6_3 = 0.1,
		  cfr6_4 = 0.5
)
end_day = yday("2020-9-30")

end_idx = length(the_data$date)
for (i in 1:length(the_data$date)) 
{
    if (the_data$date[i] == "2020-09-30")
    {
	end_idx=i
	print(paste("end_idx=",i))
    }
}
the_data_calib = the_data[1:end_idx,]
suffix="Sept_fit"

Sys.time()
# Optimization
res = optim(par = unlist(params_kc),
            fn = calc_sse_simple,
            gr = NULL,
            names(params_kc), params_fix, the_data_calib, kc_age_t, 
            method = "L-BFGS-B",
	    control = c(maxit=1000,trace=2),
            lower = params_kc_lower,
            upper = params_kc_upper)
# Calibrated parameters
Sys.time()
res$par

# Calculate sum of squared errors with calibrated parameters
sse=calc_sse_multi(res$par, names(res$par), params_fix, the_data_calib, kc_age_t)
print("For best L-BFGS-B fit...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))
score_bfgs = sse[1]+sse[2]+sse[3]

# plot the parameter outcome that has the lowest mean squared error against actuals (cases and deaths)
pdf(paste0("calibration/",suffix,".pdf"), width = 10, height = 5)
plot_calibration_params(res$par, names(res$par), params_fix, the_data_calib, kc_age_t,end_day, state)
plot_age_case_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_death_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_hosp_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
dev.off()

params_fix = get_params(res$par, names(res$par), params_fix)
save(params_fix,res, file = paste0("calibration/res_test_",suffix,".Rdata"))

params_kc <- list(rho_S7_1 = params_fix$rho_S6_1,                  # Relative testing rate of susceptibles
		  rho_S7_2 = params_fix$rho_S6_2, 
		  rho_S7_3 = params_fix$rho_S6_3, 
		  rho_S7_4 = params_fix$rho_S6_4, 
		  sd7_1 = params_fix$sd6_1,                  # Social distancing (relaxed fit by age) 
                  sd7_2 = params_fix$sd6_2,
                  sd7_3 = params_fix$sd6_3,
                  sd7_4 = params_fix$sd6_4,
		  h7_1 = params_fix$h6_1, 
		  h7_2 = params_fix$h6_2, 
		  h7_3 = params_fix$h6_3,
		  h7_4 = params_fix$h6_4,
                  cfr7_2 = params_fix$cfr6_2,
                  cfr7_3 = params_fix$cfr6_3,
                  cfr7_4 = params_fix$cfr6_4
)

params_kc_lower <- c(rho_S7_1 = 0.001, 
		  rho_S7_2 = 0.001, 
		  rho_S7_3 = 0.001, 
		  rho_S7_4 = 0.001, 
		  sd7_1 = 0,                  # Social distancing (relaxed fit by age) 
                  sd7_2 = 0.2,
                  sd7_3 = 0.2,
                  sd7_4 = 0.4,
		  h7_1 = 0.005, 
		  h7_2 = 0.001, 
		  h7_3 = 0.001, 
		  h7_4 = 0.001, 
		  cfr7_2 = 0,
		  cfr7_3 = 0,
		  cfr7_4 = 0.05
)
params_kc_upper <- c(rho_S7_1 = 0.2,
		  rho_S7_2 = 0.2, 
		  rho_S7_3 = 0.2, 
		  rho_S7_4 = 0.2, 
		  sd7_1 = (0.6),                  # Social distancing (relaxed fit by age) 
                  sd7_2 = (0.6),
                  sd7_3 = (0.6),
                  sd7_4 = (0.8),
		  h7_1 = 0.1, 
		  h7_2 = 0.2, 
		  h7_3 = 0.3, 
		  h7_4 = 0.5, 
		  cfr7_2 = 0.1,
		  cfr7_3 = 0.1,
		  cfr7_4 = 0.5
)
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
suffix="Oct_fit"

Sys.time()
# Optimization
res = optim(par = unlist(params_kc),
            fn = calc_sse_simple,
            gr = NULL,
            names(params_kc), params_fix, the_data_calib, kc_age_t, 
            method = "L-BFGS-B",
	    control = c(maxit=1000,trace=2),
            lower = params_kc_lower,
            upper = params_kc_upper)
# Calibrated parameters
Sys.time()
res$par

# Calculate sum of squared errors with calibrated parameters
sse=calc_sse_multi(res$par, names(res$par), params_fix, the_data_calib, kc_age_t)
print("For best L-BFGS-B fit...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))
score_bfgs = sse[1]+sse[2]+sse[3]

# plot the parameter outcome that has the lowest mean squared error against actuals (cases and deaths)
pdf(paste0("calibration/",suffix,".pdf"), width = 10, height = 5)
plot_calibration_params(res$par, names(res$par), params_fix, the_data_calib, kc_age_t,end_day, state)
plot_age_case_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_death_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_hosp_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
dev.off()

params_fix = get_params(res$par, names(res$par), params_fix)
save(params_fix,res, file = paste0("calibration/res_test_",suffix,".Rdata"))

params_kc <- list(rho_S8_1 = params_fix$rho_S7_1,                  # Relative testing rate of susceptibles
		  rho_S8_2 = params_fix$rho_S7_2, 
		  rho_S8_3 = params_fix$rho_S7_3, 
		  rho_S8_4 = params_fix$rho_S7_4, 
		  sd8_1 = params_fix$sd7_1,                  # Social distancing (relaxed fit by age) 
                  sd8_2 = params_fix$sd7_2,
                  sd8_3 = params_fix$sd7_3,
                  sd8_4 = params_fix$sd7_4,
		  h8_1 = params_fix$h7_1, 
		  h8_2 = params_fix$h7_2, 
		  h8_3 = params_fix$h7_3,
		  h8_4 = params_fix$h7_4,
                  cfr8_2 = params_fix$cfr7_2,
                  cfr8_3 = params_fix$cfr7_3,
                  cfr8_4 = params_fix$cfr7_4
)

params_kc_lower <- c(rho_S8_1 = 0.001, 
		  rho_S8_2 = 0.001, 
		  rho_S8_3 = 0.001, 
		  rho_S8_4 = 0.001, 
		  sd8_1 = 0,                  # Social distancing (relaxed fit by age) 
                  sd8_2 = 0.2,
                  sd8_3 = 0.2,
                  sd8_4 = 0.4,
		  h8_1 = 0.005, 
		  h8_2 = 0.001, 
		  h8_3 = 0.001, 
		  h8_4 = 0.001, 
		  cfr8_2 = 0,
		  cfr8_3 = 0,
		  cfr8_4 = 0.05
)
params_kc_upper <- c(rho_S8_1 = 0.2,
		  rho_S8_2 = 0.2, 
		  rho_S8_3 = 0.2, 
		  rho_S8_4 = 0.2, 
		  sd8_1 = (0.6),                  # Social distancing (relaxed fit by age) 
                  sd8_2 = (0.6),
                  sd8_3 = (0.6),
                  sd8_4 = (0.8),
		  h8_1 = 0.1, 
		  h8_2 = 0.2, 
		  h8_3 = 0.3, 
		  h8_4 = 0.5, 
		  cfr8_2 = 0.1,
		  cfr8_3 = 0.1,
		  cfr8_4 = 0.5
)
end_day = yday("2020-11-30")

end_idx = length(the_data$date)
for (i in 1:length(the_data$date)) 
{
    if (the_data$date[i] == "2020-11-30")
    {
	end_idx=i
	print(paste("end_idx=",i))
    }
}
the_data_calib = the_data[1:end_idx,]
suffix="Nov_fit"

Sys.time()
# Optimization
res = optim(par = unlist(params_kc),
            fn = calc_sse_simple,
            gr = NULL,
            names(params_kc), params_fix, the_data_calib, kc_age_t, 
            method = "L-BFGS-B",
	    control = c(maxit=1000,trace=2),
            lower = params_kc_lower,
            upper = params_kc_upper)
# Calibrated parameters
Sys.time()
res$par

# Calculate sum of squared errors with calibrated parameters
sse=calc_sse_multi(res$par, names(res$par), params_fix, the_data_calib, kc_age_t)
print("For best L-BFGS-B fit...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))
score_bfgs = sse[1]+sse[2]+sse[3]

# plot the parameter outcome that has the lowest mean squared error against actuals (cases and deaths)
pdf(paste0("calibration/",suffix,".pdf"), width = 10, height = 5)
plot_calibration_params(res$par, names(res$par), params_fix, the_data_calib, kc_age_t,end_day, state)
plot_age_case_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_death_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_hosp_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
dev.off()

params_fix = get_params(res$par, names(res$par), params_fix)
save(params_fix,res, file = paste0("calibration/res_test_",suffix,".Rdata"))

params_kc <- list(rho_S9_1 = params_fix$rho_S8_1,                  # Relative testing rate of susceptibles
		  rho_S9_2 = params_fix$rho_S8_2, 
		  rho_S9_3 = params_fix$rho_S8_3, 
		  rho_S9_4 = params_fix$rho_S8_4, 
		  sd9_1 = params_fix$sd8_1,                  # Social distancing (relaxed fit by age) 
                  sd9_2 = params_fix$sd8_2,
                  sd9_3 = params_fix$sd8_3,
                  sd9_4 = params_fix$sd8_4,
		  h9_1 = params_fix$h8_1, 
		  h9_2 = params_fix$h8_2, 
		  h9_3 = params_fix$h8_3,
		  h9_4 = params_fix$h8_4,
                  cfr9_2 = params_fix$cfr8_2,
                  cfr9_3 = params_fix$cfr8_3,
                  cfr9_4 = params_fix$cfr8_4
)

params_kc_lower <- c(rho_S9_1 = 0.001, 
		  rho_S9_2 = 0.001, 
		  rho_S9_3 = 0.001, 
		  rho_S9_4 = 0.001, 
		  sd9_1 = 0,                  # Social distancing (relaxed fit by age) 
                  sd9_2 = 0.2,
                  sd9_3 = 0.2,
                  sd9_4 = 0.4,
		  h9_1 = 0.005, 
		  h9_2 = 0.001, 
		  h9_3 = 0.001, 
		  h9_4 = 0.001, 
		  cfr9_2 = 0,
		  cfr9_3 = 0,
		  cfr9_4 = 0.05
)
params_kc_upper <- c(rho_S9_1 = 0.2,
		  rho_S9_2 = 0.2, 
		  rho_S9_3 = 0.2, 
		  rho_S9_4 = 0.2, 
		  sd9_1 = (0.6),                  # Social distancing (relaxed fit by age) 
                  sd9_2 = (0.6),
                  sd9_3 = (0.6),
                  sd9_4 = (0.8),
		  h9_1 = 0.1, 
		  h9_2 = 0.2, 
		  h9_3 = 0.3, 
		  h9_4 = 0.5, 
		  cfr9_2 = 0.1,
		  cfr9_3 = 0.1,
		  cfr9_4 = 0.5
)
end_day = yday("2020-12-31")

end_idx = length(the_data$date)
for (i in 1:length(the_data$date)) 
{
    if (the_data$date[i] == "2020-12-31")
    {
	end_idx=i
	print(paste("end_idx=",i))
    }
}
the_data_calib = the_data[1:end_idx,]
suffix="Dec_fit"

Sys.time()
# Optimization
res = optim(par = unlist(params_kc),
            fn = calc_sse_simple,
            gr = NULL,
            names(params_kc), params_fix, the_data_calib, kc_age_t, 
            method = "L-BFGS-B",
	    control = c(maxit=1000,trace=2),
            lower = params_kc_lower,
            upper = params_kc_upper)
# Calibrated parameters
Sys.time()
res$par

# Calculate sum of squared errors with calibrated parameters
sse=calc_sse_multi(res$par, names(res$par), params_fix, the_data_calib, kc_age_t)
print("For best L-BFGS-B fit...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3],sep='='))
score_bfgs = sse[1]+sse[2]+sse[3]

# plot the parameter outcome that has the lowest mean squared error against actuals (cases and deaths)
pdf(paste0("calibration/",suffix,".pdf"), width = 10, height = 5)
plot_calibration_params(res$par, names(res$par), params_fix, the_data_calib, kc_age_t,end_day, state)
plot_age_case_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_death_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_hosp_fit(res$par, names(res$par), params_fix, the_data_calib, kc_age_t, end_day, state)
dev.off()

params_fix = get_params(res$par, names(res$par), params_fix)
save(params_fix,res, file = paste0("calibration/res_test_",suffix,".Rdata"))
