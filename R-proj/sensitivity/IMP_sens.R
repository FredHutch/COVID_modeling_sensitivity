#Line diagrams for Dan's Vaccine Sensitivity paper


set.seed(20)
print_legend = 0

source("covid-model.R")
source("kc_read-data.R")

scen<-"IMP"
p_eff<-0.1
min_sd<-0.2
max_sd<-0.6
rate<-3500
prior_group<-4
trig_min<-25
trig_max<-350
sd_delta = 0.1

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
calib_params$dynamic_sd_limit = (trig_min + trig_max)/2 * the_pop / 100000
calib_params$dynamic_sd_hyster = (trig_max - trig_min)/2 * the_pop / 100000
calib_params$sd_inc=c(0,0,0,0)


#calib_params$sd2 = c(calib_params$sd2_1,calib_params$sd2_2,calib_params$sd2_3,calib_params$sd2_4)

# this loads the vaccination parameters
#source("kc_create_vac_scenario_params.R")
int_param_names = c("new_strain_intros", "vac_on")

#expanded way (for heatmaps)
interventions = matrix(c(
			0, 1,	
			2.5, 1,	
			5, 1,	
			10, 1),	
                       byrow = TRUE, nrow = 4)

row.names(interventions) = c(
			"No Imports/day, Vax",
			"2.5 Imports/day, Vax",
			"5 Imports/day, Vax",
			"10 Imports/day, Vax")

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)

int_rampup = 14				      # Time to achieve full intervention effect

vac_rate = as.numeric(rate)
vac_first = as.numeric(prior_group)

vac_eff_pi1 = 0.1
vac_eff_susc1 = 0.9
vac_eff_inf1 = 0.1
vac_eff_pi2 = 0.1
vac_eff_susc2 = 0.9
vac_eff_inf2 = 0.1

vac_exp_rate = 0

vac_init_doy = 366 + yday(ymd("2021-1-15"))     # Start of vaccination protocol
vac_stop_doy = 366 + yday(ymd("2021-12-31"))     # End of vaccination protocol

end_day = 366 + yday(ymd("2021-12-31")) # End of the run

calib_doy = yday(ymd("2020-10-31"))     # End of model calibration

vac_mutate=1
vac_mutate_time=366+yday(ymd("2021-1-01"))
new_strain_fact<-1.55 # relative strength of 2nd strain

suffix=scen
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
#
# If you want to test/improve the plotting more rapidly, you can comment out the lines above and just load
# the saved rds file with the readRDS command below (provided it has been run since the latest model changes)
#
#scenarios_out=readRDS(file = paste0("../data/scenarios_vacc_",suffix,".rds"))

cols = c("red","green","orange","blue")
x_lim = NULL
#cols = c("pink","peachpuff","lightgreen","lightblue","lavender","red","darkorange","darkgreen","darkblue","purple")
delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

# plot all panels for column C of fig 2
pdf(paste0("to_dan/",suffix,".pdf"), width = 4, height=12)
par(mfrow=c(6,1),mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

doy = scenarios_out$doy
doy = doy[-1]

cases = scenarios_out$cases
cases = apply(cases,2, diff)
plot_scenarios(doy, cases, yday(the_data$date), the_data$cases, 
                #y_lab = "Daily Diagnosed Cases", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:4,y_lim = c(0,1500),#lwd=lwds,
                y_lab = "Daily Diagnosed Cases", x_lim = NULL, col_pal = cols, col_idx = 1:4,y_lim = c(0,1500),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

plot_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Cumulative Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)

cum_hosp = scenarios_out$cum_hosp
cum_hosp = apply(cum_hosp,2, diff)
plot_scenarios(doy, cum_hosp, yday(the_data$date), NA, 
                y_lab = "Daily Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,150),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

deaths = scenarios_out$deaths
deaths = apply(deaths,2, diff)
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                #y_lab = "Daily Deaths", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:4,y_lim = c(0,25),#lwd=lwds,
                y_lab = "Daily Deaths", x_lim = NULL, col_pal = cols, col_idx = 1:4,y_lim = c(0,20),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

plot_scenarios(scenarios_out$doy, scenarios_out$sd_2, yday(the_data$date), NA, 
                y_lab = "Social Distancing (Non-seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,1),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)

inf = scenarios_out$inf
inf = apply(inf,2, diff)
inf2 = scenarios_out$inf2
inf2 = apply(inf2,2, diff)
perc_inf = 100 * inf2 / inf
startx= 366 + yday(ymd("2021-1-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-11-01"))     # End of x-axis
plot_scenarios(doy, perc_inf, yday(the_data$date), NA, 
               y_lab = "% Daily Infections New Variant", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:4,y_lim = c(0,100),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,all_months=1)
dev.off()

# plot daily measures for treatment scenarios against social distancing background
#pdf(paste0("dan/daily_vaccine_effects_",suffix,".pdf"), width = 8, height = 8)
#startx= 366 + yday(ymd("2021-4-01"))     # Start of x-axis
#endx= 366 + yday(ymd("2021-12-01"))     # End of x-axis
pdf(paste0("dan/daily_cases_",suffix,".pdf"), width = 5, height = 3.5)
x_lim = NULL
#par(mfrow = c(2,2), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

doy = scenarios_out$doy
doy = doy[-1]

cases = scenarios_out$cases
cases = apply(cases,2, diff)
plot_scenarios(doy, cases, yday(the_data$date), the_data$cases, 
                y_lab = "Daily Diagnosed Cases", x_lim = NULL, col_pal = cols, col_idx = 1:4,y_lim = c(0,2000),
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
dev.off()
pdf(paste0("dan/daily_deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
deaths = scenarios_out$deaths
deaths = apply(deaths,2, diff)
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Daily Deaths", x_lim = NULL, col_pal = cols, col_idx = 1:4,y_lim = c(0,15),
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL)
#legend("topleft",
#       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
#       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
#       border = NA, bty = "n", y.intersp = 0.5,
#       title = "social distancing")

dev.off()
pdf(paste0("dan/daily_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,12000),
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("dan/log_daily_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
plot_scenarios(doy, log10(inf), yday(the_data$date), NA, 
               y_lab = "Log Daily Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,4),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("dan/daily_hosps_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
cum_hosp = scenarios_out$cum_hosp
cum_hosp = apply(cum_hosp,2, diff)
plot_scenarios(doy, cum_hosp, yday(the_data$date), NA, 
                y_lab = "Daily Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,200),
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
dev.off()

pdf(paste0("dan/daily_inf1_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf1
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections Main Strain", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,15000),
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("dan/daily_inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf2
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections New Strain", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,15000),
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("dan/other_SD_",suffix,".pdf"), width = 5, height = 3.5)
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$sd_2, yday(the_data$date), NA, 
                y_lab = "Social Distancing (Non-seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,1),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0)

if(print_legend==1) {
    legend("topleft",
       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
       border = NA, bty = "n", y.intersp = 0.5,
       title = "Social Distancing")
}

dev.off()
pdf(paste0("dan/daily_perc_inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
inf2 = scenarios_out$inf2
inf2 = apply(inf2,2, diff)
perc_inf = 100 * inf2 / inf
startx= 366 + yday(ymd("2021-2-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-11-01"))     # End of x-axis
plot_scenarios(doy, perc_inf, yday(the_data$date), NA, 
               y_lab = "% Daily Infections New Variant", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:4,y_lim = c(0,100),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], NULL,all_months=1)

dev.off()
pdf(paste0("dan/legend_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 1, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,1,0,0))
    plot.new()
    legend("topleft", 
       legend = c(row.names(interventions)),
       col = cols, lty = c(rep(1, nrow(interventions))), 
       lwd = c(rep(2, nrow(interventions))), bty = "n" , cex=1.5)
dev.off()
pdf(paste0("dan/deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$deaths, yday(the_data$date), NA,
               y_lab = "Cumulative Deaths", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,3000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], totalpop = NULL)

dev.off()
pdf(paste0("dan/infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Cumulative Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)

dev.off()
pdf(paste0("dan/inf1_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf1, yday(the_data$date), NA, 
               y_lab = "Main Strain Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)

dev.off()
pdf(paste0("dan/inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf2, yday(the_data$date), NA, 
               y_lab = "New Strain Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:4,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)

dev.off()
pdf(paste0("dan/case_rates_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenario_rates(scenarios_out$doy, scenarios_out$cases, yday(the_data$date), the_data$cases, 
                y_lab = "2 week Case Rates / 100k Pop", x_lim = x_lim, col_pal = cols, col_idx = 1:4, y_lim = c(0,1500),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop,
		limit=100000*calib_params$dynamic_sd_limit/the_pop, hyster=100000*calib_params$dynamic_sd_hyster/the_pop
	 	)
dev.off()
pdf(paste0("dan/daily_perc_inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
inf2 = scenarios_out$inf2
inf2 = apply(inf2,2, diff)
perc_inf = 100 * inf2 / inf
startx= 366 + yday(ymd("2021-1-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-11-01"))     # End of x-axis
plot_scenarios(doy, perc_inf, yday(the_data$date), NA, 
               y_lab = "% Daily Infections New Variant", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:4,y_lim = c(0,100),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,all_months=1)

dev.off()
pdf(paste0("to_dan/legend_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(2, 1, 1, 4), mgp = c(2, 0.5, 0), oma = c(2,1,0,0))
    plot.new()
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), lty = c(rep(1, nrow(interventions)), 2,2), 
       lwd = c(rep(2, nrow(interventions)), 2,2), bty = "n" , cex=1.5)
dev.off()
