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
pop_fract<-args[6] # example: 0.7 (70%)
prior_group<-args[7] # example: 0 (prop), 1, (0-19), etc. (set as a mask to allow multiple groups)
trig_min<-args[8] # bi-weekly case rate per 100k pop for loosening SD
trig_max<-args[9] # bi-weekly case rate per 100k pop for tightening SD
plot_em<-args[10] # flag whether to do the plots too (vs just saving rds file)

tot_vac= as.integer((as.numeric(pop_fract) * the_pop + 5000)/10000) * 10000
sd_delta = 0 # SD release rate same as tightening rate (over 2 week period)

new_strain_intros=0.25
new_strain_fact<-1.55 # relative strength of 2nd strain (50% increase)
new_strain_severity<-1.55 # impact on hosp & death vs main variant

# read in calibration fit parameters (representing all calib months)
result_file="calibration/res_test_dec_fit.Rdata"

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

calib_params$severity = 1

#Turn off all childhood vaccinations (0-19)
calib_params$VacChild16plus = 0
calib_params$VacChild12plus = 0
calib_params$VacChild = 0

vac_final_rate = as.numeric(rate)
calib_params$vac_final_rate = as.numeric(rate)

# no vaccines until the "final rate"
vac_schedule = matrix(c(366+yday(ymd("2021-1-1")),0,     # Start of vaccination protection (1st point)
     366+yday(ymd("2021-1-15")),0),     # vaccination protection (2nd point)
                       byrow = TRUE, nrow = 2)

vac_init_doy = 366 + yday(ymd("2021-1-01"))     # Start of vaccination protocol
calib_params$dynamic_sd = T
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
int_param_names = c("vac_eff_susc1", "vac_eff_susc2", "vac_eff_inf1","vac_eff_inf2","vac_on")

#expanded way (for heatmaps)
interventions = matrix(c(0, 0, 0, 0, 0,
                         0.1, 0.1, 0.1, 0.1, 1,
                         0.3, 0.3, 0.1, 0.1, 1,
                         0.5, 0.5, 0.1, 0.1, 1,
                         0.7, 0.7, 0.1, 0.1, 1,
                         0.9, 0.9, 0.1, 0.1, 1,
                         0.1, 0.1, 0.3, 0.3, 1,
                         0.3, 0.3, 0.3, 0.3, 1,
                         0.5, 0.5, 0.3, 0.3, 1,
                         0.7, 0.7, 0.3, 0.3, 1,
                         0.9, 0.9, 0.3, 0.3, 1,
                         0.1, 0.1, 0.5, 0.5, 1,
                         0.3, 0.3, 0.5, 0.5, 1,
                         0.5, 0.5, 0.5, 0.5, 1,
                         0.7, 0.7, 0.5, 0.5, 1,
                         0.9, 0.9, 0.5, 0.5, 1,
                         0.1, 0.1, 0.7, 0.7, 1,
                         0.3, 0.3, 0.7, 0.7, 1,
                         0.5, 0.5, 0.7, 0.7, 1,
                         0.7, 0.7, 0.7, 0.7, 1,
                         0.9, 0.9, 0.7, 0.7, 1,
                         0.1, 0.1, 0.9, 0.9, 1,
                         0.3, 0.3, 0.9, 0.9, 1,
                         0.5, 0.5, 0.9, 0.9, 1,
                         0.7, 0.7, 0.9, 0.9, 1,
                         0.9, 0.9, 0.9, 0.9, 1),
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
vac_total = as.numeric(tot_vac)
vac_first = as.numeric(prior_group)
vac_eff_pi1 = as.numeric(p_eff)
vac_eff_pi2 = as.numeric(p_eff)

end_day = 366 + yday(ymd("2021-12-31")) # End of the run

calib_params$calib_doy = yday(ymd("2020-12-31"))
calib_doy = calib_params$calib_doy
vax_calib_doy = 366 + yday(ymd("2021-4-02"))
vac_stop_doy = 366 + yday(ymd("2021-12-31"))     # End of vaccination protocol
vac_init_doy = 366 + yday(ymd("2021-1-01"))     # Start of vaccination protocol

vac_mutate=1
vac_mutate_time=366+yday(ymd("2021-1-01"))

suffix=paste0(scen,"_vep_",p_eff,"_SD_",min_sd,"_to_",max_sd,"_rate_",rate,"_tot_",tot_vac,"_trigs_",trig_min,"_",trig_max)
print(suffix)

scenarios_out = get_model_data_param_sets(interventions, int_param_names, calib_params, end_day, state)
saveRDS(scenarios_out, file = paste0("../data/fig4_",suffix,".rds"))
#scenarios_out=readRDS(file = paste0("../data/fig4_",suffix,".rds"))

x_lim=NULL
#cols = c("black", "pink","lightgreen","lightblue","red","green","blue")
cols = rainbow(26)

pdf("papers_out/fig4_vacs.pdf", width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$vac_1, yday(the_data$date), NA, 
                y_lab = "Vaccinated (0-19)", x_lim = x_lim, col_pal = cols, col_idx = 1:26,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$vac_2, yday(the_data$date), NA, 
                y_lab = "Vaccinated (20-49)", x_lim = x_lim, col_pal = cols, col_idx = 1:26,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$vac_3, yday(the_data$date), NA, 
                y_lab = "Vaccinated (50-69)", x_lim = x_lim, col_pal = cols, col_idx = 1:26,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$vac_4, yday(the_data$date), NA, 
                y_lab = "Vaccinated (seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:26,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
dev.off()
pdf("papers_out/fig4.pdf", width = 5, height=11)
par(mfrow=c(6,1),mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

doy = scenarios_out$doy
doy = doy[-1]

cases = scenarios_out$cases
cases = apply(cases,2, diff)
plot_scenarios(doy, cases, yday(the_data$date), the_data$cases, 
                #y_lab = "Daily Diagnosed Cases", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:26,y_lim = c(0,1500),#lwd=lwds,
                y_lab = "Daily Diagnosed Cases", x_lim = NULL, col_pal = cols, col_idx = 1:26,y_lim = c(0,3000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

plot_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Cumulative Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:26,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

cum_hosp = scenarios_out$cum_hosp
cum_hosp = apply(cum_hosp,2, diff)
plot_scenarios(doy, cum_hosp, yday(the_data$date), NA, 
                y_lab = "Daily Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:26,y_lim = c(0,150),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

deaths = scenarios_out$deaths
deaths = apply(deaths,2, diff)
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                #y_lab = "Daily Deaths", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:26,y_lim = c(0,25),#lwd=lwds,
                y_lab = "Daily Deaths", x_lim = NULL, col_pal = cols, col_idx = 1:26,y_lim = c(0,30),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

plot_scenarios(scenarios_out$doy, scenarios_out$sd_2, yday(the_data$date), NA, 
                y_lab = "Social Distancing (Non-seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:26,y_lim = c(0,1),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

inf = scenarios_out$inf
inf = apply(inf,2, diff)
inf2 = scenarios_out$inf2
inf2 = apply(inf2,2, diff)
perc_inf = 100 * inf2 / inf
startx= 366 + yday(ymd("2021-1-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-11-01"))     # End of x-axis
plot_scenarios(doy, perc_inf, yday(the_data$date), NA, 
               y_lab = "% Daily Infections New Variant", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:26,y_lim = c(0,100),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,all_months=1,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
