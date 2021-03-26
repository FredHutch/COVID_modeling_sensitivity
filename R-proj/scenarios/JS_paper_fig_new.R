set.seed(20)
print_legend = 0

source("covid-model.R")
source("kc_read-data.R")

args<-commandArgs(trailingOnly=T)
scen<-args[1] # example: even
min_sd<-args[2] # example: 0.2
max_sd<-args[3] # example: 0.2
rate<-args[4] # example: 5000
tot_vac<-args[5] # example: 1e6
prior_group<-args[6] # example: 0 (prop), 1, (0-19), etc. (set as a mask to allow multiple groups)
trig_min<-args[7] # bi-weekly case rate per 100k pop for loosening SD
trig_max<-args[8] # bi-weekly case rate per 100k pop for tightening SD
plot_em<-args[9] # flag whether to do the plots too (vs just saving rds file)
exp_rate<-args[10] # monthly expiration rate for vaccine protection
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
calib_params$vac_exp_rate=as.numeric(exp_rate)


#calib_params$sd2 = c(calib_params$sd2_1,calib_params$sd2_2,calib_params$sd2_3,calib_params$sd2_4)

# this loads the vaccination parameters
#source("kc_create_vac_scenario_params.R")

#9 vaccine profiles (all with VEdis=90%), each is a line on each panels
# VEsusc=90%, VEsymp=0%, VEinf=10% (light blue)
#    VEsusc=90%, VEsymp=0%, VEinf=50% (medium blue)
#     VEsusc=90%, VEsymp=0%, VEinf=90% (dark blue)
#     VEsusc=70%, VEsymp=70%, VEinf=10% (light brown)
#    VEsusc=70%, VEsymp=70%, VEinf=50% (medium brown)
#     VEsusc=70%, VEsymp=70%, VEinf=90% (dark brown)
#   VEsusc=0%, VEsymp=90%, VEinf=10% (light grey)
#   VEsusc=0%, VEsymp=90%, VEinf=50% (medium grey)
#   VEsusc=0%, VEsymp=90%, VEinf=90% (dark grey)
#   No vaccine (black)
cols=c("black","lightblue","blue","darkblue","pink","red","darkred","lightgray","gray","darkgray")

int_param_names = c("vac_eff_susc", "vac_eff_pi", "vac_eff_inf", "vac_on")
interventions = matrix(c(0, 0, 0, 0,
                         0.9, 0, 0.1, 1,
                         0.9, 0, 0.5, 1,
                         0.9, 0, 0.9, 1,
                         0.7, 0.7, 0.1, 1,
                         0.7, 0.7, 0.5, 1,
                         0.7, 0.7, 0.9, 1,
                         0.0, 0.9, 0.1, 1,
                         0.0, 0.9, 0.5, 1,
                         0.0, 0.9, 0.9, 1),
                       byrow = TRUE, nrow = 10)

row.names(interventions) = c("No Vaccine", "90% VE_S,0% VE_P,10% VE_I", "90% VE_S,0% VE_P,50% VE_I","90% VE_S,0% VE_P,90% VE_I", 
				"70% VE_S,70% VE_P,10% VE_I", "70% VE_S,70% VE_P,50% VE_I","70% VE_S,70% VE_P,90% VE_I",
				"0% VE_S,90% VE_P,10% VE_I", "0% VE_S,90% VE_P,50% VE_I","0% VE_S,90% VE_P,90% VE_I")

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)

int_rampup = 14				      # Time to achieve full intervention effect

vac_eff_hi = 0
vac_rate = as.numeric(rate)
vac_total = as.numeric(tot_vac)
vac_first = as.numeric(prior_group)

vac_init_doy = 366 + yday(ymd("2021-1-01"))     # Start of vaccination protocol
end_day = 366 + yday(ymd("2021-12-31"))

calib_doy = yday(ymd("2020-10-31"))     # End of model calibration

suffix=paste0(scen,"_vep_",vac_eff_pi,"_SD_",min_sd,"_to_",max_sd,"_rate_",rate,"_tot_",tot_vac,"_trigs_",trig_min,"_",trig_max,"_",exp_rate)
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

if (as.numeric(plot_em) == 0) quit()

#
# If you want to test/improve the plotting more rapidly, you can comment out the lines above and just load
# the saved rds file with the readRDS command below (provided it has been run since the latest model changes)
#
#scenarios_out=readRDS(file = paste0("../data/scenarios_vacc_",suffix,".rds"))

# plot treatment scenarios against social distancing background
#pdf(paste0("out/vaccine_",suffix,".pdf"), width = 8, height = 8)
pdf(paste0("josh/V2_cases_",suffix,".pdf"), width = 5, height = 3.5)
x_lim = NULL
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

#cols = c("black", "pink","lightgreen","lightblue","red","green","blue","darkred","darkgreen","darkblue")
delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

plot_scenarios(scenarios_out$doy, scenarios_out$cases, yday(the_data$date), the_data$cases, 
                y_lab = "Cumulative Diagnosed Cases", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,125000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)
if(print_legend==1) {
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), lty = c(rep(1, nrow(interventions)), NA,NA), pch = c(rep(NA, nrow(interventions)), 8, 12),
       lwd = c(rep(2, nrow(interventions)), NA,NA), bty = "n" , cex=0.8)
    mtext(paste("Distrib:",scen,"Triggers:",trig_min,"/",trig_max,"VE_I=",vac_eff_inf,"Rate=",rate,"Total Doses=",tot_vac),outer = TRUE, cex = 1.2)
}

dev.off()
pdf(paste0("josh/V2_deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Cumulative Deaths", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,2500),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL)
if(print_legend==1) {
    legend("topleft",
       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
       border = NA, bty = "n", y.intersp = 0.5,
       title = "social distancing")
}

dev.off()
pdf(paste0("josh/V2_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Cumulative Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,600000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)

dev.off()
pdf(paste0("josh/V2_hosps_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$cum_hosp, yday(the_data$date), NA, 
                y_lab = "Cumulative Hospitalized", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,12000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL)

dev.off()

# plot daily measures for treatment scenarios against social distancing background
#pdf(paste0("josh/V2_daily_vaccine_effects_",suffix,".pdf"), width = 8, height = 8)
pdf(paste0("josh/V2_daily_cases_",suffix,".pdf"), width = 5, height = 3.5)
x_lim = NULL
#par(mfrow = c(2,2), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

doy = scenarios_out$doy
doy = doy[-1]

cases = scenarios_out$cases
cases = apply(cases,2, diff)
plot_scenarios(doy, cases, yday(the_data$date), the_data$cases, 
                y_lab = "Daily Diagnosed Cases", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,800),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
mtext(paste("Distrib:",scen,"Triggers:",trig_min,"/",trig_max,"VE_I=",vac_eff_inf,"Rate=",rate,"Total Doses=",tot_vac),outer = TRUE, cex = 1.2)
#legend("topleft", 
#       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
#       col = c(cols, "black","orange"), lty = c(rep(1, nrow(interventions)), NA), pch = c(rep(NA, nrow(interventions)), 8, 12),
#       lwd = c(rep(2, nrow(interventions)), NA,NA), bty = "n, cex=0.8" )

dev.off()
pdf(paste0("josh/V2_daily_deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
deaths = scenarios_out$deaths
deaths = apply(deaths,2, diff)
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Daily Deaths", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,12),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
#legend("topleft",
#       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
#       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
#       border = NA, bty = "n", y.intersp = 0.5,
#       title = "social distancing")

dev.off()
pdf(paste0("josh/V2_daily_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,5000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("josh/V2_daily_hosps_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
cum_hosp = scenarios_out$cum_hosp
cum_hosp = apply(cum_hosp,2, diff)
plot_scenarios(doy, cum_hosp, yday(the_data$date), NA, 
                y_lab = "Daily Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,75),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
dev.off()

pdf(paste0("josh/V2_reff_",suffix,".pdf"), width = 5, height = 3.5)
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$Reff, yday(the_data$date), NA, 
                y_lab = "R Effective", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,2),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
		target=1)
if(print_legend==1) {
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), lty = c(rep(1, nrow(interventions)), NA,NA), pch = c(rep(NA, nrow(interventions)), 8, 12),
       lwd = c(rep(2, nrow(interventions)), NA,NA), bty = "n" , cex=0.8)
    mtext(paste("Distrib:",scen,"Triggers:",trig_min,"/",trig_max,"VE_I=",vac_eff_inf,"Rate=",rate,"Total Doses=",tot_vac),outer = TRUE, cex = 1.2)
}

dev.off()
pdf(paste0("josh/V2_snr_SD_",suffix,".pdf"), width = 5, height = 3.5)
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$sd_4, yday(the_data$date), NA, 
                y_lab = "Social Distancing (Seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,1),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)

if(print_legend==1) {
    legend("topleft",
       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
       border = NA, bty = "n", y.intersp = 0.5,
       title = "Social Distancing")
}

dev.off()
pdf(paste0("josh/V2_other_SD_",suffix,".pdf"), width = 5, height = 3.5)
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$sd_2, yday(the_data$date), NA, 
                y_lab = "Social Distancing (Non-seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,1),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)

if(print_legend==1) {
    legend("topleft",
       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
       border = NA, bty = "n", y.intersp = 0.5,
       title = "Social Distancing")
}

dev.off()
pdf(paste0("josh/V2_case_rates_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenario_rates(scenarios_out$doy, scenarios_out$cases, yday(the_data$date), the_data$cases, 
                y_lab = "2 week Case Rates / 100k Pop", x_lim = x_lim, col_pal = cols, col_idx = 1:10, y_lim = c(0,500),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop,
		limit=100000*calib_params$dynamic_sd_limit/the_pop, hyster=100000*calib_params$dynamic_sd_hyster/the_pop
	 	)
if(print_legend==1) {
    legend("topright",
       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
       border = NA, bty = "n", y.intersp = 0.5,
       title = "Social Distancing")

    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), lty = c(rep(1, nrow(interventions)), NA,NA), pch = c(rep(NA, nrow(interventions)), 8, 12),
       lwd = c(rep(2, nrow(interventions)), NA,NA), bty = "n" , cex=0.8)
    mtext(paste("Distrib:",scen,"Triggers:",trig_min,"/",trig_max,"VE_I=",vac_eff_inf,"Rate=",rate,"Total Doses=",tot_vac),outer = TRUE, cex = 1.2)
}
dev.off()

# plot treatment reduction scenarios against social distancing background
#pdf(paste0("josh/V2_vaccine_reductions_",suffix,".pdf"), width = 8, height = 8)
pdf(paste0("josh/V2_case_reduct_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
x_lim = NULL
#par(mfrow = c(2,2), mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

plot_delta_scenarios(scenarios_out$doy, scenarios_out$cases, yday(the_data$date), the_data$cases, 
                y_lab = "Reduction in Diagnosed Cases", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,50000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)
if(print_legend==1) {
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), lty = c(rep(1, nrow(interventions)), NA,NA), pch = c(rep(NA, nrow(interventions)), 8, 12),
       lwd = c(rep(2, nrow(interventions)), NA,NA), bty = "n" , cex=0.8)
    mtext(paste("Distrib:",scen,"Triggers:",trig_min,"/",trig_max,"VE_I=",vac_eff_inf,"Rate=",rate,"Total Doses=",tot_vac),outer = TRUE, cex = 1.2)
}
dev.off()

pdf(paste0("josh/V2_death_reduct_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_delta_scenarios(scenarios_out$doy, scenarios_out$deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Reduction in Deaths", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,800),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL)
if(print_legend==1) {
    legend("topleft",
       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
       border = NA, bty = "n", y.intersp = 0.5,
       title = "social distancing")
}

dev.off()
pdf(paste0("josh/V2_inf_reduct_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_delta_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Reduction in Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,250000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL)

dev.off()
pdf(paste0("josh/V2_hosp_reduct_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_delta_scenarios(scenarios_out$doy, scenarios_out$cum_hosp, yday(the_data$date), NA, 
                y_lab = "Reduction in Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,4000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL)
dev.off()
pdf(paste0("josh/V2_susc_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$susc_1, yday(the_data$date), NA, 
                y_lab = "Susceptible (0-19)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$susc_2, yday(the_data$date), NA, 
                y_lab = "Susceptible (20-49)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$susc_3, yday(the_data$date), NA, 
                y_lab = "Susceptible (50-69)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$susc_4, yday(the_data$date), NA, 
                y_lab = "Susceptible (seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
dev.off()
pdf(paste0("josh/V2_vacs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$vac_1, yday(the_data$date), NA, 
                y_lab = "Vaccinated (0-19)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$vac_2, yday(the_data$date), NA, 
                y_lab = "Vaccinated (20-49)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$vac_3, yday(the_data$date), NA, 
                y_lab = "Vaccinated (50-69)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$vac_4, yday(the_data$date), NA, 
                y_lab = "Vaccinated (seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
dev.off()
pdf(paste0("josh/V2_age_deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$deaths_1, yday(the_data$date), NA, 
                y_lab = "Deaths (0-19)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,2000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$deaths_2, yday(the_data$date), NA, 
                y_lab = "Deaths (20-49)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,2000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$deaths_3, yday(the_data$date), NA, 
                y_lab = "Deaths (50-69)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,2000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$deaths_4, yday(the_data$date), NA, 
                y_lab = "Deaths (seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,2000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
dev.off()
pdf(paste0("josh/V2_age_hosps_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$hosp_1, yday(the_data$date), NA, 
                y_lab = "Hosp (0-19)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,400),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$hosp_2, yday(the_data$date), NA, 
                y_lab = "Hosp (20-49)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,400),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$hosp_3, yday(the_data$date), NA, 
                y_lab = "Hosp (50-69)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,400),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
plot_scenarios(scenarios_out$doy, scenarios_out$hosp_4, yday(the_data$date), NA, 
                y_lab = "Hosp (seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,400),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
dev.off()
