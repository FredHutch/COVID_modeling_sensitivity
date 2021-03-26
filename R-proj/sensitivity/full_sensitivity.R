set.seed(20)
print_legend = 0

source("covid-model.R")
source("kc_read-data.R")

#Settings used in all scenarios (for now)
#max_sd<-0.6	# how far to tighten SD
sd_delta = 0.1 # how slowly to relax SD

args<-commandArgs(trailingOnly=T)
dist<-args[1] # example: prop, adults, seniors
prior_group<-args[2] # example: 0 (prop), 1, (0-19), etc. (set as a mask to allow multiple groups)
vei<-args[3] # example: 0, 0.25, etc.
rate<-args[4] # example: 5000
new_strain_fact<-as.numeric(args[5]) # increase in inf for new strain, example: 1.35 (35%), 1.5(50%), etc.
min_sd<-args[6] # how far to relax SD
max_sd<-args[7] # how far to relax SD
trig_min<-args[8] # bi-weekly case rate per 100k pop for loosening SD
trig_max<-args[9] # bi-weekly case rate per 100k pop for tightening SD
trig_perc<-args[10] # bi-weekly case & hosp rate percent rise/drop to tighten/loosen SD (after "new_check_date")
cover<- args[11] # age-group vax coverage (fraction)
imports<- args[12] # daily new mutation imports

new_check_date=730+yday(ymd("2021-01-15")) # date for switch from case triggers to percent change in cases/hospitalizations

vac_exp_rate=0
vac_coverage=as.numeric(cover)
new_strain_intros=as.numeric(imports)

# read in calibration fit parameters (representing all calib months)
result_file="calib/res_test_Dec_diag_rho.Rdata"

intervention_day = yday(ymd("2020-5-15"))     # Start of intervention protocol
int_rampup = 14				      # Time to achieve full intervention effect

load(file = result_file)
calib_vals = res$par

calib_params = get_params(calib_vals, names(res$par), params_fix)

# set interventions
calib_params$beta_d_fact= 0.5

calib_params$dynamic_sd = T
calib_params$dynamic_sd_delta = as.numeric(sd_delta)
calib_params$dynamic_sd_min = as.numeric(min_sd)
calib_params$dynamic_sd_min_snrs = as.numeric(min_sd) + 0.2
calib_params$dynamic_sd_max = as.numeric(max_sd)
calib_params$dynamic_sd_max_snrs = as.numeric(max_sd) + 0.2
calib_params$sd_inc=c(0,0,0,0)
calib_params$dynamic_sd_limit = ((as.numeric(trig_min) + as.numeric(trig_max))/2) * the_pop / 100000
calib_params$dynamic_sd_hyster = ((as.numeric(trig_max) - as.numeric(trig_min))/2) * the_pop / 100000
calib_params$sd_growth_trigger=as.numeric(trig_perc)/100	# % growth as tighten trigger
calib_params$sd_decline_trigger=as.numeric(trig_perc)/100 	# % decline as loosen trigger


# this loads the vaccination parameters

int_param_names = c("vac_eff_susc1", "vac_eff_pi1","vac_eff_susc2", "vac_eff_pi2","vac_on")
interventions = matrix(c(0, 0, 0, 0, 0,
                         0.1, 0.1, 0.1, 0.1, 1,
                         0.5, 0.1, 0.5, 0.1, 1,
                         0.9, 0.1, 0.9, 0.1, 1,
                         0.1, 0.5, 0.1, 0.5, 1,
                         0.5, 0.5, 0.5, 0.5, 1,
                         0.9, 0.5, 0.9, 0.5, 1,
                         0.1, 0.9, 0.1, 0.9, 1,
                         0.5, 0.9, 0.5, 0.9, 1,
                         0.9, 0.9, 0.9, 0.9, 1),
                       byrow = TRUE, nrow = 10)

row.names(interventions) = c("No Vaccine", "10% VE_S,10% VE_I", "50% VE_S,10% VE_I","90% VE_S,10% VE_I", 
				"10% VE_S,50% VE_I", "50% VE_S,50% VE_I","90% VE_S,50% VE_I",
				"10% VE_S,90% VE_I", "50% VE_S,90% VE_I","90% VE_S,90% VE_I")

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)

int_rampup = 14				      # Time to achieve full intervention effect

vac_eff_hi = 0
vac_rate = as.numeric(rate)
vac_first = as.numeric(prior_group)
vac_eff_inf = as.numeric(vei)

vac_eff_inf1 = vac_eff_inf
vac_eff_inf2 = vac_eff_inf

vac_init_doy = 366 + yday(ymd("2021-1-15"))     # Start of vaccination protocol
vac_stop_doy = 366 + yday(ymd("2021-12-31"))     # End of vaccination protocol

vac_mutate=1
vac_mutate_time=366+yday(ymd("2021-1-01"))

end_day = 370 + vac_init_doy

calib_doy = yday(ymd("2020-10-31"))     # End of model calibration

suffix=paste0(dist,"_vei_",vei,"_sdmin_",min_sd,"_sdmax_",max_sd,"_rate_",rate,"_mut_",new_strain_fact,"_trigmin_",trig_min,"_trigmax_",trig_max,"_trigperc_",trig_perc,"_cover_",cover,"_import_",imports)
print(suffix)

state[sd_adj_idx] = calib_params$sd9_1                  # Social distancing (relaxed fit by age) 
state[sd_adj_idx+1] = calib_params$sd9_2
state[sd_adj_idx+2] = calib_params$sd9_3
state[sd_adj_idx+3] = calib_params$sd9_4

scenarios_out = get_model_data_param_sets(interventions, int_param_names, calib_params, end_day, state)
saveRDS(scenarios_out, file = paste0("../sens_data/",suffix,".rds"))

