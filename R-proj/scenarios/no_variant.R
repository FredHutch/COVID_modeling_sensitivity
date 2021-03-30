#Line diagrams for Dan's Vaccine Senesitivity paper

#Roll out = 2000, 3400, 5000, 8000: lockdown threshold=350, VEs = 90, VEi=10, VEsymp=10 -> y-axis = daily cases
#Roll out = 2000, 3400, 5000, 8000: lockdown threshold=350, VEs = 90, VEi=10, VEsymp=10 -> y-axis = daily hospitalizations
#Roll out = 2000, 3400, 5000, 8000: lockdown threshold=350, VEs = 90, VEi=10, VEsymp=10 -> y-axis = daily deaths

set.seed(20)
setwd("..")	# run in the R-proj directory

source("covid-model.R")
source("kc_read-data.R")

args<-commandArgs(trailingOnly=T)
scen<-"NO_VARIANT_VAX"
max_sd<-0.6
prior_group<-4
trig_min<-100
sd_delta = 0.1
vac_coverage=0.94
new_strain_intros=0
new_strain_fact<-1 # relative strength of 2nd strain (no increase)

new_check_date=0 # No switch from case triggers to percent change in cases/hospitalizations

vac_eff_hi = 0
vac_first = as.numeric(prior_group)
vac_eff_pi1 = 0.95
vac_eff_susc1 = 0.8
vac_eff_inf1 = 0.1
vac_eff_pi2 = 0.95
vac_eff_susc2 = 0.8
vac_eff_inf2 = 0.1


# read in calibration fit parameters (representing all calib months)
result_file="calibration/res_test_dec_fit.Rdata"

intervention_day = yday(ymd("2020-5-15"))     # Start of intervention protocol
int_rampup = 14				      # Time to achieve full intervention effect

load(file = result_file)
calib_vals = res$par

calib_params = get_params(calib_vals, names(res$par), params_fix)

# set interventions
calib_params$beta_d_fact= 0.5

calib_params$dynamic_sd = T
calib_params$dynamic_sd_delta = as.numeric(sd_delta)
calib_params$dynamic_sd_max = as.numeric(max_sd)
calib_params$dynamic_sd_max_snrs = as.numeric(max_sd) + 0.2
calib_params$sd_inc=c(0,0,0,0)

vax_calib_doy = 366 + yday(ymd("2021-4-02"))
calib_params$severity = 1

# this loads the vaccination parameters
#source("create_world_scenarios.R")
source("scenarios/create_world_scenarios_new_rate.R")

int_rampup = 14				      # Time to achieve full intervention effect

vac_on = 1
vac_exp_rate = 0

vac_init_doy = 366 + yday(ymd("2021-1-15"))     # Start of vaccination protocol
vac_stop_doy = 366 + yday(ymd("2021-12-31"))     # End of vaccination protocol

end_day = 366 + yday(ymd("2021-12-31")) # End of the run

calib_doy = yday(ymd("2020-12-31"))     # End of model calibration
calib_params$calib_doy = calib_doy

vac_mutate=1
vac_mutate_time=366+yday(ymd("2021-1-01"))

suffix=scen
print(suffix)

scenarios_out = get_model_data_param_sets(interventions, int_param_names, calib_params, end_day, state)
saveRDS(scenarios_out, file = paste0("../shiny_data/",suffix,".rds"))
#scenarios_out=readRDS(file = paste0("../shiny_data/",suffix,".rds"))

#source("scenarios/print_world_scenarios.R")
source("scenarios/print_world_scenarios_new_rate.R")
