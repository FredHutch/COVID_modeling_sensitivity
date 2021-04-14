#Line diagrams for Dan's Vaccine Sensitivity paper
#
#In this case we are varying the limit for triggering lockdown (Cmax in the literature) and holding
#other variables at default values.

set.seed(20)
print_legend = 0
setwd("..")	# run in the R-proj directory

source("covid-model.R")
source("kc_read-data.R")

scen<-"TRIG_MAX"
p_eff<-0
min_sd<-0.2
max_sd<-0.6
rate<-10000
prior_group<-4
trig_min<-100
sd_delta = 0.1

vac_coverage=0.95

#default b117 strain characteristics, but generic 90% Vsusc efficacy of vaccine
new_strain_intros=0.25
new_strain_severity = 1.55
new_strain_fact<-1.55 # relative strength of 2nd strain

new_check_date=0 # No switch from case triggers to percent change in cases/hospitalizations

# read in calibration fit parameters (representing all calib months)
result_file="calibration/res_test_dec_fit.Rdata"

intervention_day = yday(ymd("2020-5-15"))     # Start of intervention protocol
int_rampup = 14				      # Time to achieve full intervention effect

load(file = result_file)
calib_vals = res$par

calib_params = get_params(calib_vals, names(res$par), params_fix)

# set interventions
calib_params$beta_d_fact= 0.5

calib_params$vac_final_rate = as.numeric(rate)		# this one will be adopted after vac_schedule ends
calib_params$severity = 1

calib_params$VacChild16plus = 730+yday(ymd("2021-04-1")) # April 1, 2022 (pushed out to turn off!)
calib_params$VacChild12plus = 730+yday(ymd("2021-06-1")) # June 1, 2022 (pushed out to turn off!)
calib_params$VacChild = 730+yday(ymd("2021-09-1")) # Sept 1, 2022 (pushed out to turn off!)

calib_params$dynamic_sd = T
calib_params$sd_trans = 14
calib_params$dynamic_sd_delta = as.numeric(sd_delta)
calib_params$dynamic_sd_min = as.numeric(min_sd)
calib_params$dynamic_sd_min_snrs = as.numeric(min_sd) + 0.2
calib_params$dynamic_sd_max = as.numeric(max_sd)
calib_params$dynamic_sd_max_snrs = as.numeric(max_sd) + 0.2
calib_params$sd_inc=c(0,0,0,0)

# this loads the vaccination parameters
int_param_names = c("dynamic_sd_limit","dynamic_sd_hyster", "vac_on")

#expanded way (for heatmaps)
interventions = matrix(c(
		((trig_min + 200)/2) * the_pop / 100000,((200 - trig_min)/2) * the_pop / 100000,1,
		((trig_min + 350)/2) * the_pop / 100000,((350 - trig_min)/2) * the_pop / 100000,1,
		((trig_min + 500)/2) * the_pop / 100000,((500 - trig_min)/2) * the_pop / 100000,1,
		((trig_min + 650)/2) * the_pop / 100000,((650 - trig_min)/2) * the_pop / 100000,1),
                       byrow = TRUE, nrow = 4)

#Lockdown threshold 200, 350, 500, 650: VEs = 90, VEi=0, VEsymp=0, roll out 5000 / day -> y-axis = daily cases
row.names(interventions) = c(
			"Lockdown threshold=200",
			"Lockdown threshold=350",
			"Lockdown threshold=500",
			"Lockdown threshold=650")

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)

int_rampup = 14				      # Time to achieve full intervention effect

vac_rate = as.numeric(rate)
vac_first = as.numeric(prior_group)

vac_eff_pi1 = 0.95
vac_eff_susc1 = 0.9
vac_eff_inf1 = 0.1
vac_eff_pi2 = 0.86
vac_eff_susc2 = 0.8
vac_eff_inf2 = 0.1

vac_exp_rate = 0

vac_init_doy = 366 + yday(ymd("2021-1-15"))     # Start of vaccination protocol
vac_stop_doy = 366 + yday(ymd("2021-12-31"))     # End of vaccination protocol

end_day = 366 + yday(ymd("2021-12-31")) # End of the run

calib_doy = calib_params$calib_doy

vac_mutate=1
vac_mutate_time=366+yday(ymd("2021-1-01"))

suffix=scen
print(suffix)

scenarios_out = get_model_data_param_sets(interventions, int_param_names, calib_params, end_day, state)
saveRDS(scenarios_out, file = paste0("papers_data/",suffix,".rds"))
#
# If you want to test/improve the plotting more rapidly, you can comment out the lines above and just load
# the saved rds file with the readRDS command below (provided it has been run since the latest model changes)
#
#scenarios_out=readRDS(file = paste0("papers_data/",suffix,".rds"))

cols = c("blue","green","red")
x_lim = NULL
delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

# plot all panels for column C of fig 2
pdf(paste0("papers_out/",suffix,".pdf"), width = 4, height=12)
par(mfrow=c(6,1),mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

doy = scenarios_out$doy
doy = doy[-1]

cases = scenarios_out$cases
cases = apply(cases,2, diff)
plot_scenarios(doy, cases, yday(the_data$date), the_data$cases, 
                y_lab = "Daily Diagnosed Cases", x_lim = NULL, col_pal = cols, col_idx = 1:3,y_lim = c(0,3000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

plot_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Cumulative Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)

cum_hosp = scenarios_out$cum_hosp
cum_hosp = apply(cum_hosp,2, diff)
plot_scenarios(doy, cum_hosp, yday(the_data$date), NA, 
                y_lab = "Daily Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,150),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

deaths = scenarios_out$deaths
deaths = apply(deaths,2, diff)
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Daily Deaths", x_lim = NULL, col_pal = cols, col_idx = 1:3,y_lim = c(0,20),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

plot_scenarios(scenarios_out$doy, scenarios_out$sd_2, yday(the_data$date), NA, 
                y_lab = "Social Distancing (Non-seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,1),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)

inf = scenarios_out$inf
inf = apply(inf,2, diff)
inf2 = scenarios_out$inf2
inf2 = apply(inf2,2, diff)
perc_inf = 100 * inf2 / inf
startx= 366 + yday(ymd("2021-1-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-11-01"))     # End of x-axis
plot_scenarios(doy, perc_inf, yday(the_data$date), NA, 
               y_lab = "% Daily Infections New Variant", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:3,y_lim = c(0,100),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,all_months=1)

dev.off()
# plot daily measures for treatment scenarios against social distancing background
pdf(paste0("papers_out/daily_cases_",suffix,".pdf"), width = 5, height = 3.5)
x_lim = NULL
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

doy = scenarios_out$doy
doy = doy[-1]

cases = scenarios_out$cases
cases = apply(cases,2, diff)
plot_scenarios(doy, cases, yday(the_data$date), the_data$cases, 
                y_lab = "Daily Diagnosed Cases", x_lim = NULL, col_pal = cols, col_idx = 1:3,y_lim = c(0,3000),
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
dev.off()
pdf(paste0("papers_out/daily_deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
deaths = scenarios_out$deaths
deaths = apply(deaths,2, diff)
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Daily Deaths", x_lim = NULL, col_pal = cols, col_idx = 1:3,y_lim = c(0,20),
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
dev.off()

pdf(paste0("papers_out/daily_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,10000),
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("papers_out/log_daily_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
plot_scenarios(doy, log10(inf), yday(the_data$date), NA, 
               y_lab = "Log Daily Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,4),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("papers_out/daily_hosps_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
cum_hosp = scenarios_out$cum_hosp
cum_hosp = apply(cum_hosp,2, diff)
plot_scenarios(doy, cum_hosp, yday(the_data$date), NA, 
                y_lab = "Daily Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,150),
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
dev.off()

pdf(paste0("papers_out/daily_inf1_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf1
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections Main Strain", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,12000),
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("papers_out/daily_inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf2
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections New Strain", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,12000),
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("papers_out/other_SD_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$sd_2, yday(the_data$date), NA, 
                y_lab = "Social Distancing (Non-seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,1),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)

if(print_legend==1) {
    legend("topleft",
       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
       border = NA, bty = "n", y.intersp = 0.5,
       title = "Social Distancing")
}

dev.off()
pdf(paste0("papers_out/legend_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 1, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,1,0,0))
    plot.new()
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), lty = c(rep(1, nrow(interventions)), 2,2),
       lwd = c(rep(2, nrow(interventions)+2)), bty = "n" , cex=1.5)
dev.off()
pdf(paste0("papers_out/deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$deaths, yday(the_data$date), NA, 
               y_lab = "Cumulative Deaths", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,2500),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], totalpop = 0)

dev.off()
pdf(paste0("papers_out/infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Cumulative Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)

dev.off()
pdf(paste0("papers_out/inf1_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf1, yday(the_data$date), NA, 
               y_lab = "Main Strain Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)

dev.off()
pdf(paste0("papers_out/inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf2, yday(the_data$date), NA, 
               y_lab = "New Strain Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:3,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)

dev.off()
pdf(paste0("papers_out/case_rates_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenario_rates(scenarios_out$doy, scenarios_out$cases, yday(the_data$date), the_data$cases, 
                y_lab = "2 week Case Rates / 100k Pop", x_lim = x_lim, col_pal = cols, col_idx = 1:3, y_lim = c(0,3000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop,
		limit=100000*calib_params$dynamic_sd_limit/the_pop, hyster=100000*calib_params$dynamic_sd_hyster/the_pop
	 	)
dev.off()
