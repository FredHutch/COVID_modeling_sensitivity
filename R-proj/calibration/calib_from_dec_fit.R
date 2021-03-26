# Model fitting
library(lubridate)
library(mco)

source("covid-model.R")

# Read in rhoS County actual data for calibration ####
source("kc_read-data.R")

intervention_abbr = "TI_S+C"
intervention_day = yday(ymd("2020-5-15"))     # Start of intervention protocol
int_rampup = 14				      # Time to achieve full intervention effect
trig_min<-100
trig_max<-350
sd_delta <- 0.1
max_sd=0.6
min_sd=0.2

params_fix$beta_d_fact= 0.5
params_fix$dynamic_sd = T

# test 40% asymptomatic
#params_fix$p = 0.6

vac_eff_hi = 0
vac_first = 4
vac_eff_pi1 = 0.95
vac_eff_susc1 = 0.8
vac_eff_inf1 = 0.1
vac_eff_pi2 = 0.86
vac_eff_susc2 = 0.7
vac_eff_inf2 = 0.1
vac_coverage=0.8
new_strain_intros=1
new_strain_fact<-1.5 # relative strength of 2nd strain (no increase)
new_check_date=0 # DO NOT switch from case triggers to percent change in cases/hospitalizations

vac_mutate=1
vac_mutate_time=366+yday(ymd("2021-1-01"))

suffix="dec_fit"
load(file = paste0("calibration/res_test_",suffix,".Rdata"))

params_fix$dynamic_sd = T
params_fix$dynamic_sd_delta = as.numeric(sd_delta)
params_fix$dynamic_sd_min = as.numeric(min_sd)
params_fix$dynamic_sd_min_snrs = as.numeric(min_sd) + 0.2
params_fix$dynamic_sd_max = as.numeric(max_sd)
params_fix$dynamic_sd_max_snrs = as.numeric(max_sd) + 0.2
params_fix$dynamic_sd_period = 14
params_fix$sd_inc=c(0,0,0,0)

params_fix$delta12_doy = 366 + yday(ymd("2021-02-01"))    # Day to switch to dynamic SDs?
params_fix$DiagDate11 = 366+yday(ymd("2021-01-15"))
params_fix$DiagDate12 = 366+yday(ymd("2021-02-15"))

params_fix$Tests11 = mean_tests("2021-1-15")
params_fix$Tests12 = mean_tests("2021-2-01")
print(params_fix$Tests12)

params_fix$Pos11 = mean_pos("2021-01-15")
params_fix$Pos12 = mean_pos("2021-02-01")

params_fix$vac_init_doy = 366+yday(ymd("2021-1-15"))     # Start of vaccination protection
params_fix$vac_end_ramp = 366+yday(ymd("2021-03-15"))  # Vax ramping continues to this date
params_fix$vac_start_rate = 1500			  # starting # w/ full doses (1 week lagged)
params_fix$vac_end_rate = 7500
params_fix$vac_on = 1
params_fix$dynamic_sd_limit = ((as.numeric(trig_min) + as.numeric(trig_max))/2) * the_pop / 100000
params_fix$dynamic_sd_hyster = ((as.numeric(trig_max) - as.numeric(trig_min))/2) * the_pop / 100000

params_fix$rho_S10_1 = params_fix$rho_S6_1                  # Relative testing rate of susceptibles
params_fix$rho_S10_2 = params_fix$rho_S6_2 
params_fix$rho_S10_3 = params_fix$rho_S6_3 
params_fix$rho_S10_4 = params_fix$rho_S6_4 
params_fix$sd10_1 = params_fix$sd9_1                  # Social distancing (relaxed fit by age) 
params_fix$sd10_2 = params_fix$sd9_2
params_fix$sd10_3 = params_fix$sd9_3
params_fix$sd10_4 = params_fix$sd9_4
params_fix$h10_1 = params_fix$h9_1 
params_fix$h10_2 = params_fix$h9_2 
params_fix$h10_3 = params_fix$h9_3
params_fix$h10_4 = params_fix$h9_4
params_fix$cfr10_1 = 0
params_fix$cfr10_2 = params_fix$cfr9_2
params_fix$cfr10_3 = params_fix$cfr9_3
params_fix$cfr10_4 = params_fix$cfr9_4

params_kc <- list(
		  sd10_1 = params_fix$sd9_1,                  # Social distancing (relaxed fit by age) 
                  sd10_2 = params_fix$sd9_2,
                  sd10_3 = params_fix$sd9_3,
                  sd10_4 = params_fix$sd9_4,
		  h10_1 = params_fix$h9_1, 
		  h10_2 = params_fix$h9_2, 
		  h10_3 = params_fix$h9_3,
		  h10_4 = params_fix$h9_4,
                  cfr10_2 = params_fix$cfr9_2,
                  cfr10_3 = params_fix$cfr9_3,
                  cfr10_4 = params_fix$cfr9_4
)

params_kc_lower <- c(
		  sd10_1 = 0.1,                  # Social distancing (relaxed fit by age) 
                  sd10_2 = 0.1,
                  sd10_3 = 0.1,
                  sd10_4 = 0.1,
		  h10_1 = 0.005, 
		  h10_2 = 0.001, 
		  h10_3 = 0.001, 
		  h10_4 = 0.001, 
		  cfr10_2 = 0,
		  cfr10_3 = 0,
		  cfr10_4 = 0.05
)
params_kc_upper <- c(
		  sd10_1 = 0.6,                  # Social distancing (relaxed fit by age) 
                  sd10_2 = 0.6,
                  sd10_3 = 0.6,
                  sd10_4 = 0.8,
		  h10_1 = 0.1, 
		  h10_2 = 0.2, 
		  h10_3 = 0.3, 
		  h10_4 = 0.5, 
		  cfr10_2 = 0.1,
		  cfr10_3 = 0.1,
		  cfr10_4 = 0.5
)
end_day = 366+yday("2021-1-31")

end_idx = length(the_data$date)
for (i in 1:length(the_data$date)) 
{
    if (the_data$date[i] == "2021-1-31")
    {
	end_idx=i
    }
}
the_data_calib = the_data[1:end_idx,]
print(end_idx)
params_fix$calib_doy=end_day
suffix="jan_fit"

sse=calc_sse_multi(unlist(params_kc), names(params_kc), params_fix, the_data_calib, kc_age_t)
print("For initial values...")
print(paste("case error",sse[1],sep='='))
print(paste("death error",sse[2],sep='='))
print(paste("daily hosp error",sse[3],sep='='))
print(paste("daily test error",sse[4],sep='='))
print(paste("score",sse[1]+sse[2]+sse[3]+sse[4],sep='='))

pdf(paste0("calibration/initial_fit_",suffix,".pdf"), width = 10, height = 5)
plot_calibration_params(unlist(params_kc), names(params_kc), params_fix, the_data_calib, kc_age_t,end_day, state)
plot_age_case_fit(unlist(params_kc), names(params_kc), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_death_fit(unlist(params_kc), names(params_kc), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_hosp_fit(unlist(params_kc), names(params_kc), params_fix, the_data_calib, kc_age_t, end_day, state)
plot_age_test_fit(unlist(params_kc), names(params_kc), params_fix, the_data_calib, kc_age_t, end_day, state)
dev.off()

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
