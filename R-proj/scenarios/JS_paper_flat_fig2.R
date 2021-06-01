set.seed(20)
print_legend = 0

source("covid-model.R")
source("kc_read-data.R")

fig<-2
scen<-"seniors"
p_eff<-0.9
min_sd<-0.3
max_sd<-0.6

rate<-10000

prior_group<-4
trig_min<-100 # bi-weekly case rate per 100k pop for loosening SD
trig_max<-350 # bi-weekly case rate per 100k pop for tightening SD

tot_vac= as.integer((0.9 * the_pop + 5000)/10000) * 10000
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

vac_stop_doy = 366 + yday(ymd("2021-12-31"))     # End of vaccination protocol

vac_mutate=1
vac_mutate_time=366+yday(ymd("2021-1-01"))

calib_params$calib_doy = yday(ymd("2020-12-31"))
calib_doy = calib_params$calib_doy

#calib_params$sd2 = c(calib_params$sd2_1,calib_params$sd2_2,calib_params$sd2_3,calib_params$sd2_4)

# this loads the vaccination parameters
#source("kc_create_vac_scenario_params.R")
int_param_names = c("vac_eff_susc1", "vac_eff_susc2", "vac_eff_inf1","vac_eff_inf2","vac_on")

interventions = matrix(c(0, 0, 0, 0, 0,
                         0.9, 0.9, 0.9, 0.9, 1,
                         0.9, 0.9, 0.5, 0.5, 1,
                         0.9, 0.9, 0.1, 0.1, 1,
                         0.5, 0.5, 0.9, 0.9, 1,
                         0.5, 0.5, 0.5, 0.5, 1,
                         0.5, 0.5, 0.1, 0.1, 1,
                         0.1, 0.1, 0.9, 0.9, 1,
                         0.1, 0.1, 0.5, 0.5, 1,
                         0.1, 0.1, 0.1, 0.1, 1),
                       byrow = TRUE, nrow = 10)

row.names(interventions) = c("No Vaccine", 
		"90% VEsusc,90% VEinf",
		"90% VEsusc,50% VEinf",
		"90% VEsusc,10% VEinf", 
		"50% VEsusc,90% VEinf",
		"50% VEsusc,50% VEinf",
		"50% VEsusc,10% VEinf",
		"10% VEsusc,90% VEinf", 
		"10% VEsusc,50% VEinf", 
		"10% VEsusc,10% VEinf") 

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)

int_rampup = 14				      # Time to achieve full intervention effect

vac_eff_hi = 0
vac_total = as.numeric(tot_vac)
vac_first = as.numeric(prior_group)
vac_eff_pi1 = as.numeric(p_eff)
vac_eff_pi2 = as.numeric(p_eff)

vac_init_doy = 366 + yday(ymd("2021-1-01"))     # Start of vaccination protocol
end_day = 366 + yday(ymd("2021-12-31"))

suffix=paste0(scen,"_vep_",p_eff,"_SD_",min_sd,"_to_",max_sd,"_rate_",rate,"_tot_",tot_vac,"_trigs_",trig_min,"_",trig_max)
print(suffix)

scenarios_out = get_model_data_param_sets(interventions, int_param_names, calib_params, end_day, state)
saveRDS(scenarios_out, file = paste0("papers_data/scenarios_vacc_",suffix,".rds"))

#
# If you want to test/improve the plotting more rapidly, you can comment out the lines above and just load
# the saved rds file with the readRDS command below (provided it has been run since the latest model changes)
#
#scenarios_out=readRDS(file = paste0("papers_data/scenarios_vacc_",suffix,".rds"))

# plot treatment scenarios against social distancing background
pdf(paste0("papers_out/JS",fig,"_flat_cases_",suffix,".pdf"), width = 5, height = 3.5)
x_lim = NULL
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

#cols = c("black", "pink","lightgreen","lightblue","red","green","blue","darkred","darkgreen","darkblue")
#cols = c("black", "pink","lightgreen","lightblue","red","green","blue")
cols = c("black", "darkblue","blue","lightblue","darkgreen","green","lightgreen","darkred","red","pink")

delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

plot_scenarios(scenarios_out$doy, scenarios_out$cases, yday(the_data$date), the_data$cases, 
                y_lab = "Cumulative Diagnosed Cases", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,500000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
if(print_legend==1) {
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), lty = c(rep(1, nrow(interventions)), NA,NA), pch = c(rep(NA, nrow(interventions)), 8, 12),
       lwd = c(rep(2, nrow(interventions)), NA,NA), bty = "n" , cex=0.8)
}

dev.off()
pdf(paste0("papers_out/JS",fig,"_flat_deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Cumulative Deaths", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,4000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL)
if(print_legend==1) {
    legend("topleft",
       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
       border = NA, bty = "n", y.intersp = 0.5,
       title = "social distancing")
}

dev.off()
pdf(paste0("papers_out/JS",fig,"_flat_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Cumulative Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)

dev.off()
pdf(paste0("papers_out/JS",fig,"_flat_hosps_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$cum_hosp, yday(the_data$date), NA, 
                y_lab = "Cumulative Hospitalized", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,20000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL)

dev.off()

# plot daily measures for treatment scenarios against social distancing background
#pdf(paste0("papers_out/JS",fig,"_flat_daily_vaccine_effects_",suffix,".pdf"), width = 8, height = 8)
#startx= 366 + yday(ymd("2021-4-01"))     # Start of x-axis
#endx= 366 + yday(ymd("2021-12-01"))     # End of x-axis
pdf(paste0("papers_out/JS",fig,"_flat_daily_cases_",suffix,".pdf"), width = 5, height = 3.5)
x_lim = NULL
#par(mfrow = c(2,2), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

doy = scenarios_out$doy
doy = doy[-1]

cases = scenarios_out$cases
cases = apply(cases,2, diff)
plot_scenarios(doy, cases, yday(the_data$date), the_data$cases, 
                y_lab = "Daily Diagnosed Cases", x_lim = NULL, col_pal = cols, col_idx = 1:10,y_lim = c(0,4000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
mtext(paste("Distrib:",scen,"Triggers:",trig_min,"/",trig_max,"VE_I=",vac_eff_inf,"Rate=",rate,"Total Doses=",tot_vac),outer = TRUE, cex = 1.2)
#legend("topleft", 
#       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
#       col = c(cols, "black","orange"), lty = c(rep(1, nrow(interventions)), NA), pch = c(rep(NA, nrow(interventions)), 8, 12),
#       lwd = c(rep(2, nrow(interventions)), NA,NA), bty = "n, cex=0.8" )

dev.off()
pdf(paste0("papers_out/JS",fig,"_flat_daily_deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
deaths = scenarios_out$deaths
deaths = apply(deaths,2, diff)
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Daily Deaths", x_lim = NULL, col_pal = cols, col_idx = 1:10,y_lim = c(0,30),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
#legend("topleft",
#       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
#       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
#       border = NA, bty = "n", y.intersp = 0.5,
#       title = "social distancing")

dev.off()
pdf(paste0("papers_out/JS",fig,"_flat_daily_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,8000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("papers_out/JS",fig,"_flat_daily_hosps_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
cum_hosp = scenarios_out$cum_hosp
cum_hosp = apply(cum_hosp,2, diff)
plot_scenarios(doy, cum_hosp, yday(the_data$date), NA, 
                y_lab = "Daily Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,400),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
dev.off()

pdf(paste0("papers_out/JS",fig,"_flat_reff_",suffix,".pdf"), width = 5, height = 3.5)
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
pdf(paste0("papers_out/JS",fig,"_flat_snr_SD_",suffix,".pdf"), width = 5, height = 3.5)
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
pdf(paste0("papers_out/JS",fig,"_flat_other_SD_",suffix,".pdf"), width = 5, height = 3.5)
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
pdf(paste0("papers_out/JS",fig,"_flat_case_rates_",suffix,".pdf"), width = 5, height = 3.5)
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
#pdf(paste0("papers_out/JS",fig,"_flat_vaccine_reductions_",suffix,".pdf"), width = 8, height = 8)
pdf(paste0("papers_out/JS",fig,"_flat_case_reduct_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
x_lim = NULL
#par(mfrow = c(2,2), mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

plot_delta_scenarios(scenarios_out$doy, scenarios_out$cases, yday(the_data$date), the_data$cases, 
                y_lab = "Reduction in Diagnosed Cases", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,100000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)
if(print_legend==1) {
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), lty = c(rep(1, nrow(interventions)), NA,NA), pch = c(rep(NA, nrow(interventions)), 8, 12),
       lwd = c(rep(2, nrow(interventions)), NA,NA), bty = "n" , cex=0.8)
    mtext(paste("Distrib:",scen,"Triggers:",trig_min,"/",trig_max,"VE_I=",vac_eff_inf,"Rate=",rate,"Total Doses=",tot_vac),outer = TRUE, cex = 1.2)
}
dev.off()

pdf(paste0("papers_out/JS",fig,"_flat_death_reduct_",suffix,".pdf"), width = 5, height = 3.5)
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
pdf(paste0("papers_out/JS",fig,"_flat_inf_reduct_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_delta_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Reduction in Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,250000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL)

dev.off()
pdf(paste0("papers_out/JS",fig,"_flat_hosp_reduct_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_delta_scenarios(scenarios_out$doy, scenarios_out$cum_hosp, yday(the_data$date), NA, 
                y_lab = "Reduction in Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,3000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL)
dev.off()
pdf(paste0("papers_out/JS",fig,"_flat_susc_",suffix,".pdf"), width = 5, height = 3.5)
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
pdf(paste0("papers_out/JS",fig,"_flat_vax_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$vax, yday(the_data$date), NA, 
                y_lab = "Total Vaccinated", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)
dev.off()
pdf(paste0("papers_out/JS",fig,"_flat_vacs_",suffix,".pdf"), width = 5, height = 3.5)
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
pdf(paste0("papers_out/JS",fig,"_flat_age_deaths_",suffix,".pdf"), width = 5, height = 3.5)
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
pdf(paste0("papers_out/JS",fig,"_flat_age_hosps_",suffix,".pdf"), width = 5, height = 3.5)
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
pdf(paste0("papers_out/JS",fig,"_flat_legend_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 1, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,1,0,0))
    plot.new()
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), 
       text.col = c(cols, "black","orange"), 
	lty = c(rep(1, nrow(interventions)), 2,2), pch = c(rep(NA, nrow(interventions)), NA, NA),
       lwd = c(rep(2, nrow(interventions)), 2,2), bty = "n" , cex=1)
dev.off()

pdf(paste0("papers_out/JS",fig,"_flat_daily_perc_inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
inf2 = scenarios_out$inf2
inf2 = apply(inf2,2, diff)
perc_inf = 100 * inf2 / inf
startx= 366 + yday(ymd("2021-1-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-11-01"))     # End of x-axis
plot_scenarios(doy, perc_inf, yday(the_data$date), NA, 
               y_lab = "% Daily Infections New Variant", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:10,y_lim = c(0,100),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,all_months=1,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
