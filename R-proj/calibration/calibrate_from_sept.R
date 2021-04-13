# Model fitting extension
#
# One problem we had was that during the 3rd wave, the optimizer drove diagnostic rate down to 
# unrealistic levels causing prevalence to become extremely high.  To combat this, we decided to preserve
# the diagnostic rate (calculated from testing levels and the special "rhoS" parameter, at the September
# level and allow the other variable to continue to be fitted as before.
#
library(lubridate)
library(mco)

setwd("..")	# run in the R-proj directory
source("covid-model.R")

# Read in King County actual data for calibration ####
source("kc_read-data.R")

# Basic settings / stretegies are as outlined in calibration_bfgs.R file
#
intervention_abbr = "TI_S+C"
intervention_day = yday(ymd("2020-5-15"))     # Start of intervention protocol
int_rampup = 14				      # Time to achieve full intervention effect

params_fix$beta_d_fact= 0.5
params_fix$dynamic_sd = F

vac_on = 0
vac_rate = 0
vac_first = 0


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
load(file = paste0("calibration/res_test_",suffix,".Rdata"))

params_fix$rho_S7_1 = params_fix$rho_S6_1                  # Relative testing rate of susceptibles
params_fix$rho_S7_2 = params_fix$rho_S6_2 
params_fix$rho_S7_3 = params_fix$rho_S6_3 
params_fix$rho_S7_4 = params_fix$rho_S6_4 
params_fix$delta12_doy = 366 + yday(ymd("2021-02-01"))    # Day to switch to dynamic SDs?
params_fix$DiagDate12 = 366+yday(ymd("2021-02-15"))
params_fix$Tests12 = 366+mean_tests("2021-2-15")
params_fix$Pos12 = 366+mean_pos("2021-02-15")
params_fix$vac_init_doy = 366+yday(ymd("2021-1-15"))     # Start of vaccination protection
params_fix$vac_end_ramp = 366+yday(ymd("2021-03-15"))  # Vax ramping continues to this date
params_fix$vac_start_rate = 1500			  # starting # w/ full doses (1 week lagged)
params_fix$vac_end_rate = 7500
params_fix$vac_on = 1

params_kc <- list(
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

params_kc_lower <- c(
		  sd7_1 = 0.1,                  # Social distancing (relaxed fit by age) 
                  sd7_2 = 0.1,
                  sd7_3 = 0.1,
                  sd7_4 = 0.1,
		  h7_1 = 0.005, 
		  h7_2 = 0.001, 
		  h7_3 = 0.001, 
		  h7_4 = 0.001, 
		  cfr7_2 = 0,
		  cfr7_3 = 0,
		  cfr7_4 = 0.05
)
params_kc_upper <- c(
		  sd7_1 = 0.6,                  # Social distancing (relaxed fit by age) 
                  sd7_2 = 0.6,
                  sd7_3 = 0.6,
                  sd7_4 = 0.8,
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
suffix="Oct_fit2"

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

params_fix$rho_S8_1 = params_fix$rho_S6_1                  # Relative testing rate of susceptibles
params_fix$rho_S8_2 = params_fix$rho_S6_2 
params_fix$rho_S8_3 = params_fix$rho_S6_3 
params_fix$rho_S8_4 = params_fix$rho_S6_4 

params_kc <- list(
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

params_kc_lower <- c(
		  sd8_1 = 0.1,                  # Social distancing (relaxed fit by age) 
                  sd8_2 = 0.1,
                  sd8_3 = 0.1,
                  sd8_4 = 0.1,
		  h8_1 = 0.005, 
		  h8_2 = 0.001, 
		  h8_3 = 0.001, 
		  h8_4 = 0.001, 
		  cfr8_2 = 0,
		  cfr8_3 = 0,
		  cfr8_4 = 0.05
)
params_kc_upper <- c(
		  sd8_1 = 0.6,                  # Social distancing (relaxed fit by age) 
                  sd8_2 = 0.6,
                  sd8_3 = 0.6,
                  sd8_4 = 0.8,
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
suffix="Nov_fit2"

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

params_fix$rho_S9_1 = params_fix$rho_S6_1                  # Relative testing rate of susceptibles
params_fix$rho_S9_2 = params_fix$rho_S6_2 
params_fix$rho_S9_3 = params_fix$rho_S6_3 
params_fix$rho_S9_4 = params_fix$rho_S6_4 

params_kc <- list(
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

params_kc_lower <- c(
		  sd9_1 = 0.1,                  # Social distancing (relaxed fit by age) 
                  sd9_2 = 0.1,
                  sd9_3 = 0.1,
                  sd9_4 = 0.1,
		  h9_1 = 0.005, 
		  h9_2 = 0.001, 
		  h9_3 = 0.001, 
		  h9_4 = 0.001, 
		  cfr9_2 = 0,
		  cfr9_3 = 0,
		  cfr9_4 = 0.05
)
params_kc_upper <- c(
		  sd9_1 = 0.6,                  # Social distancing (relaxed fit by age) 
                  sd9_2 = 0.6,
                  sd9_3 = 0.6,
                  sd9_4 = 0.8,
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
suffix="Dec_fit2"

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
