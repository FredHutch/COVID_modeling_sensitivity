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
vei<-args[3] # example: 0.1
ves<-args[4] # example: 0.9
vep<-args[5] # example: 0.1
rate<-args[6] # example: 3500
new_strain_fact<-as.numeric(args[7]) # increase in inf for new strain, example: 1.35 (35%), 1.5(50%), etc.
min_sd<-args[8] # how far to relax SD
max_sd<-args[9] # how far to relax SD
trig_min<-args[10] # bi-weekly case rate per 100k pop for loosening SD
trig_max<-args[11] # bi-weekly case rate per 100k pop for tightening SD
trig_perc<- args[12] # bi-weekly case/hosp percentage change (percent)
cover<- args[13] # age-group vax coverage (fraction)
imports<- args[14] # daily new mutation imports

plot_em<-0

#if (trig_perc == 0)
new_check_date=730+yday(ymd("2021-01-15")) # date for switch from case triggers to percent change in cases/hospitalizations

vac_coverage=as.numeric(cover)
new_strain_intros=as.numeric(imports)
vac_exp_rate=0
mut_indep = 1

# read in calibration fit parameters (representing all calib months)
result_file="calib/res_test_Oct_King.Rdata"

intervention_day = yday(ymd("2020-5-15"))     # Start of intervention protocol
int_rampup = 14				      # Time to achieve full intervention effect

load(file = result_file)
calib_vals = res$par

calib_params = get_params(calib_vals, names(res$par), params_fix)

# set interventions
calib_params$beta_d_fact= 0.5

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
sd_growth_trigger=as.numeric(trig_perc)/100	# % growth as tighten trigger
sd_decline_trigger=as.numeric(trig_perc)/100 	# % decline as loosen trigger


#calib_params$sd2 = c(calib_params$sd2_1,calib_params$sd2_2,calib_params$sd2_3,calib_params$sd2_4)

# this loads the vaccination parameters

int_param_names = c("vac_on")
interventions = matrix(c(0, 1),
                       byrow = TRUE, nrow = 2)

row.names(interventions) = c("No Vaccine", "vax")

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)

int_rampup = 14				      # Time to achieve full intervention effect

vac_eff_hi = 0
vac_rate = as.numeric(rate)
vac_first = as.numeric(prior_group)
vac_eff_inf = as.numeric(vei)
vac_eff_pi = as.numeric(vep)
vac_eff_susc = as.numeric(ves)

vac_eff_inf1 = vac_eff_inf
vac_eff_inf2 = vac_eff_inf

vac_eff_pi1 = vac_eff_pi
vac_eff_pi2 = vac_eff_pi

vac_eff_susc1 = vac_eff_susc
vac_eff_susc2 = vac_eff_susc

vac_init_doy = 366 + yday(ymd("2021-1-15"))     # Start of vaccination protocol
vac_stop_doy = 366 + yday(ymd("2021-12-31"))     # End of vaccination protocol

vac_mutate=1
vac_mutate_time=366+yday(ymd("2021-1-01"))

end_day = 370+vac_init_doy

calib_doy = yday(ymd("2020-10-31"))     # End of model calibration

suffix=paste0(dist,"_vei_",vei,"_ves_",ves,"_vep_",vep,"_sdmin_",min_sd,"_sdmax_",max_sd,"_rate_",rate,"_mut_",new_strain_fact,"_trigmin_",trig_min,"_trigmax_",trig_max,"_trigperc_",trig_perc,"_cover_",cover,"_import_",imports)
print(suffix)

state[sd_adj_idx] = calib_params$sd7_1                  # Social distancing (relaxed fit by age) 
state[sd_adj_idx+1] = calib_params$sd7_2
state[sd_adj_idx+2] = calib_params$sd7_3
state[sd_adj_idx+3] = calib_params$sd7_4

if (plot_em == 0)
{
    scenarios_out = get_model_data_param_sets(interventions, int_param_names, calib_params, end_day, state)
    saveRDS(scenarios_out, file = paste0("../sens_data/",suffix,".rds"))
    quit()
} else {
    scenarios_out=readRDS(file = paste0("../sens_data/",suffix,".rds"))
}

cols = c("black","red")

x_lim = NULL
#setwd("shiny_out")

pdf(paste0("daily_cases_",suffix,".pdf"), width = 5, height = 3.5)
x_lim = NULL
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

doy = scenarios_out$doy
doy = doy[-1]

cases = scenarios_out$cases
cases = apply(cases,2, diff)
plot_scenarios(doy, cases, yday(the_data$date), the_data$cases, 
                y_lab = "Daily Diagnosed Cases", x_lim = NULL, col_pal = cols, col_idx = 1:2,y_lim = c(0,2000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL)
dev.off()
pdf(paste0("daily_deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
deaths = scenarios_out$deaths
deaths = apply(deaths,2, diff)
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Daily Deaths", x_lim = NULL, col_pal = cols, col_idx = 1:2,y_lim = c(0,40),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL)
dev.off()
pdf(paste0("daily_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:2,y_lim = c(0,25000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("log_daily_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
plot_scenarios(doy, log10(inf), yday(the_data$date), NA, 
               y_lab = "Log Daily Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:2,y_lim = c(0,4),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("daily_hosps_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
cum_hosp = scenarios_out$cum_hosp
cum_hosp = apply(cum_hosp,2, diff)
plot_scenarios(doy, cum_hosp, yday(the_data$date), NA, 
                y_lab = "Daily Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:2,y_lim = c(0,150),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL)
dev.off()
pdf(paste0("deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
deaths = scenarios_out$deaths
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Cumulative Deaths", x_lim = NULL, col_pal = cols, col_idx = 1:2,y_lim = c(0,4000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL)
dev.off()
pdf(paste0("other_SD_",suffix,".pdf"), width = 5, height = 3.5)
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$sd_2, yday(the_data$date), NA, 
                y_lab = "Social Distancing (Non-seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:2,y_lim = c(0,1),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], totalpop = 0)

dev.off()

pdf(paste0("infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Cumulative Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:2,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], totalpop = the_pop)

dev.off()
pdf(paste0("inf1_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf1, yday(the_data$date), NA, 
               y_lab = "Main Variant Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:2,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], totalpop = the_pop)

dev.off()
pdf(paste0("inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf2, yday(the_data$date), NA, 
               y_lab = "New Variant Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:2,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], totalpop = the_pop)

dev.off()
pdf(paste0("daily_inf1_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf1
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections Main Variant", x_lim = x_lim, col_pal = cols, col_idx = 1:2,y_lim = c(0,8000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("daily_inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf2
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections New Variant", x_lim = x_lim, col_pal = cols, col_idx = 1:2,y_lim = c(0,8000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("daily_perc_inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
inf2 = scenarios_out$inf2
inf2 = apply(inf2,2, diff)
perc_inf = 100 * inf2 / inf
startx= 366 + yday(ymd("2021-1-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-12-01"))     # End of x-axis
plot_scenarios(doy, perc_inf, yday(the_data$date), NA, 
               y_lab = "% Daily Infections New Variant", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:2,y_lim = c(0,100),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL,all_months=1)

dev.off()
pdf(paste0("legend_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 1, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,1,0,0))
    plot.new()
    legend("topleft", 
       legend = c("No Vaccine","Vaccine", "Vaccination Start"), 
       col = c(cols, "orange"), lty = c(1,1,2), 
       lwd = c(2,2,2), bty = "n" , cex=1.5)
dev.off()
