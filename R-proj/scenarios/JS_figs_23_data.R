set.seed(20)
print_legend = 0

source("covid-model.R")
source("kc_read-data.R")

args<-commandArgs(trailingOnly=T)
scen<-args[1] # example: even
p_eff<-args[2] # example: 0, 0.25, etc.
min_sd<-args[3] # example: 0.2
max_sd<-args[4] # example: 0.2
rate<-args[5] # example: 5000
tot_vac<-args[6] # example: 1e6
prior_group<-args[7] # example: 0 (prop), 1, (0-19), etc. (set as a mask to allow multiple groups)
trig_min<-args[8] # bi-weekly case rate per 100k pop for loosening SD
trig_max<-args[9] # bi-weekly case rate per 100k pop for tightening SD
plot_em<-args[10] # flag whether to do the plots too (vs just saving rds file)
tot_vac= as.integer((0.5 * the_pop + 5000)/10000) * 10000
sd_delta = 0.1
print(tot_vac)

# read in calibration fit parameters (representing all calib months)
result_file="out/res_test_Sept_nhd.Rdata"

intervention_day = yday(ymd("2020-5-15"))     # Start of intervention protocol
int_rampup = 14				      # Time to achieve full intervention effect

load(file = result_file)
calib_vals = res$par

calib_params = get_params(calib_vals, names(res$par), params_fix)

# set interventions
calib_params$beta_d_fact= 0.5
calib_params$hstar_fact= 1.0
calib_params$f_fact= 1.0
calib_params$r_3_intervention = NA

# Temporary params until Rdata file is updated..
#
calib_params$delta8_doy = yday(ymd("2020-10-01"))
calib_params$delta9_doy = yday(ymd("2020-11-01"))
calib_params$DiagDate8 = yday(ymd("2020-10-15"))
calib_params$DiagDate9 = yday(ymd("2020-11-15"))
calib_params$Tests7 = mean_tests("2020-9-15")
calib_params$Tests8 = mean_tests("2020-10-15")
calib_params$Pos1 = mean_pos("2020-03-15")	    # Days at which to capture daily positive test avg for the month
calib_params$Pos2 = mean_pos("2020-04-15")
calib_params$Pos3 = mean_pos("2020-05-15")
calib_params$Pos4 = mean_pos("2020-06-15")
calib_params$Pos5 = mean_pos("2020-07-15")
calib_params$Pos6 = mean_pos("2020-08-15")
calib_params$Pos7 = mean_pos("2020-09-15")
calib_params$Pos8 = mean_pos("2020-10-15")
calib_params$sd_trans = 14
calib_params$dynamic_sd_delta = as.numeric(sd_delta)
calib_params$dynamic_sd_min = as.numeric(min_sd)
calib_params$dynamic_sd_min_snrs = as.numeric(min_sd) + 0.2
calib_params$dynamic_sd_max = as.numeric(max_sd)
calib_params$dynamic_sd_max_snrs = as.numeric(max_sd) + 0.2
calib_params$sd_inc=c(0,0,0,0)
calib_params$dynamic_sd_limit = ((as.numeric(trig_min) + as.numeric(trig_max))/2) * the_pop / 100000
calib_params$dynamic_sd_hyster = ((as.numeric(trig_max) - as.numeric(trig_min))/2) * the_pop / 100000


#calib_params$sd2 = c(calib_params$sd2_1,calib_params$sd2_2,calib_params$sd2_3,calib_params$sd2_4)

# this loads the vaccination parameters
#source("kc_create_vac_scenario_params.R")
int_param_names = c("vac_eff_susc", "vac_eff_inf","vac_on")

#expanded way (for heatmaps)
interventions = matrix(c(0, 0, 0,
                         0.1, 0.1, 1,
                         0.3, 0.1, 1,
                         0.5, 0.1, 1,
                         0.7, 0.1, 1,
                         0.9, 0.1, 1,
                         0.1, 0.3, 1,
                         0.3, 0.3, 1,
                         0.5, 0.3, 1,
                         0.7, 0.3, 1,
                         0.9, 0.3, 1,
                         0.1, 0.5, 1,
                         0.3, 0.5, 1,
                         0.5, 0.5, 1,
                         0.7, 0.5, 1,
                         0.9, 0.5, 1,
                         0.1, 0.7, 1,
                         0.3, 0.7, 1,
                         0.5, 0.7, 1,
                         0.7, 0.7, 1,
                         0.9, 0.7, 1,
                         0.1, 0.9, 1,
                         0.3, 0.9, 1,
                         0.5, 0.9, 1,
                         0.7, 0.9, 1,
                         0.9, 0.9, 1),
                       byrow = TRUE, nrow = 26)

row.names(interventions) = c("No Vaccine", "10% VE_S,10% VE_I", "30% VE_S,10% VE_I","50% VE_S,10% VE_I", "70% VE_S,10% VE_I","90% VE_S,10% VE_I", 
				"10% VE_S,30% VE_I", "30% VE_S,30% VE_I","50% VE_S,30% VE_I","70% VE_S,30% VE_I","90% VE_S,30% VE_I",
				"10% VE_S,50% VE_I", "30% VE_S,50% VE_I","50% VE_S,50% VE_I","70% VE_S,50% VE_I","90% VE_S,50% VE_I",
				"10% VE_S,70% VE_I", "30% VE_S,70% VE_I","50% VE_S,70% VE_I","70% VE_S,70% VE_I","90% VE_S,70% VE_I",
				"10% VE_S,90% VE_I", "30% VE_S,90% VE_I","50% VE_S,90% VE_I","70% VE_S,90% VE_I","90% VE_S,90% VE_I")

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)

int_rampup = 14				      # Time to achieve full intervention effect

vac_eff_hi = 0
vac_rate = as.numeric(rate)
vac_total = as.numeric(tot_vac)
vac_first = as.numeric(prior_group)
vac_eff_pi = as.numeric(p_eff)

vac_init_doy = 366 + yday(ymd("2021-1-01"))     # Start of vaccination protocol
end_day = 366 + yday(ymd("2021-12-31"))

calib_doy = yday(ymd("2020-10-31"))     # End of model calibration

suffix=paste0(scen,"_vep_",vac_eff_pi,"_SD_",min_sd,"_to_",max_sd,"_rate_",rate,"_tot_",tot_vac,"_trigs_",trig_min,"_",trig_max)
print(suffix)

calib_params$rho_S7_1=0.003957275
calib_params$rho_S7_2=0.022899826
calib_params$rho_S7_3= 0.011881329
calib_params$rho_S7_4=0.090674800 
calib_params$sd7_1=0.218055807
calib_params$sd7_2 = 0.400384141
calib_params$sd7_3=0.7
calib_params$sd7_4=0.9
calib_params$h7_1=0.020029291
calib_params$h7_2=0.045895869
calib_params$h7_3=0.028173077
calib_params$h7_4=0.05
calib_params$cfr7_1=0
calib_params$cfr7_2=0
calib_params$cfr7_3=0.005232754
calib_params$cfr7_4=0.112719502 

state[sd_adj_idx] = calib_params$sd7_1                  # Social distancing (relaxed fit by age) 
state[sd_adj_idx+1] = calib_params$sd7_2
state[sd_adj_idx+2] = calib_params$sd7_3
state[sd_adj_idx+3] = calib_params$sd7_4

scenarios_out = get_model_data_param_sets(interventions, int_param_names, calib_params, end_day, state)
saveRDS(scenarios_out, file = paste0("../data/scenarios_vacc_",suffix,".rds"))
