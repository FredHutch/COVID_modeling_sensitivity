#Scenario for P1 variant as specified for shiny app 
#
# 243 scenarios representing 3 values each of SDmin, Cmin, Cmax, Coverage and final Vax rate/day

set.seed(20)
print_legend = 0
setwd("..")	# run in the R-proj directory

source("covid-model.R")
source("kc_read-data.R")

args<-commandArgs(trailingOnly=T)
scen<-"P1"
max_sd<-0.6
prior_group<-4
trig_min<-200
sd_delta = 0.1
new_strain_intros=0.25
new_strain_fact<-1.5 # relative strength of 2nd strain (50% increase)
new_strain_severity<-1 # impact on hosp & death vs main variant

new_check_date=0 # DO NOT switch from case triggers to percent change in cases/hospitalizations

# read in calibration fit parameters (representing all calib months)
result_file="calibration/res_test_dec_fit.Rdata"

intervention_day = yday(ymd("2020-5-15"))     # Start of intervention protocol
int_rampup = 14				      # Time to achieve full intervention effect

vac_eff_pi1 = 0.95
vac_eff_susc1 = 0.9
vac_eff_inf1 = 0.1
vac_eff_pi2 = 0.5
vac_eff_susc2 = 0.5
vac_eff_inf2 = 0.1

load(file = result_file)
calib_vals = res$par

calib_params = get_params(calib_vals, names(res$par), params_fix)

# set interventions
calib_params$beta_d_fact= 0.5

calib_params$dynamic_sd = T
calib_params$dynamic_sd_delta = as.numeric(sd_delta)
calib_params$dynamic_sd_max = as.numeric(max_sd)
calib_params$dynamic_sd_max_snrs = as.numeric(max_sd) + 0.2
calib_params$dynamic_sd_period = 14
calib_params$sd_inc=c(0,0,0,0)

#Turn off all childhood vaccinations (0-19)
calib_params$VacChild16plus = 0
calib_params$VacChild12plus = 0
calib_params$VacChild = 0
calib_params$severity = 1

# this loads the default parameters and values deither side of the defaults
source("scenarios/create_all_worlds.R")

int_rampup = 14				      # Time to achieve full intervention effect

vac_eff_hi = 0
vac_first = as.numeric(prior_group)

vac_on = 1
vac_exp_rate = 0

vac_stop_doy = 366 + yday(ymd("2021-12-31"))     # End of vaccination protocol

end_day = 366 + yday(ymd("2021-12-31")) # End of the run

calib_doy = calib_params$calib_doy
vax_calib_doy = 366 + yday(ymd("2021-4-02"))

vac_mutate=1
vac_mutate_time=366+yday(ymd("2021-1-01"))

suffix=scen
print(suffix)

scenarios_out = get_model_data_param_sets(interventions, int_param_names, calib_params, end_day, state)
saveRDS(scenarios_out, file = paste0("../shiny_data/",suffix,".rds"))
#scenarios_out=readRDS(file = paste0("../shiny_data/",suffix,".rds"))

source("scenarios/print_all_worlds.R")
#print(row.names(interventions))
