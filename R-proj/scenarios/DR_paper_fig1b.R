#Line diagrams for Dan's Vaccine Senesitivity paper

#Lockdown threshold 200, 350, 500, 800, 1400: VEs = 90, VEi=0, VEsymp=0, roll out 5000 / day -> y-axis = daily cases
#Lockdown threshold 200, 350, 500, 800, 1400: VEs = 90, VEi=0, VEsymp=0, roll out 5000 / day -> y-axis = daily hospitalizations
#Lockdown threshold 200, 350, 500, 800, 1400: VEs = 90, VEi=0, VEsymp=0, roll out 5000 / day -> y-axis = daily deaths

set.seed(20)
print_legend = 0

source("covid-model.R")
source("kc_read-data.R")

args<-commandArgs(trailingOnly=T)
scen<-"no_vaccine"
p_eff<-0
min_sd<-0.2
max_sd<-0.6
rate<-5000
prior_group<-8
trig_min<-100
tot_vac= as.integer((0.5 * the_pop + 5000)/10000) * 10000
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
calib_params$sd_inc=c(0,0,0,0)


#calib_params$sd2 = c(calib_params$sd2_1,calib_params$sd2_2,calib_params$sd2_3,calib_params$sd2_4)

# this loads the vaccination parameters
#source("kc_create_vac_scenario_params.R")
int_param_names = c("dynamic_sd_limit","dynamic_sd_hyster", "vac_on")

#expanded way (for heatmaps)
interventions = matrix(c( 150 * the_pop / 100000, 50 * the_pop / 100000,0,
		       225 * the_pop / 100000, 125 * the_pop / 100000,0,
		       300 * the_pop / 100000, 200 * the_pop / 100000,0,
		       450 * the_pop / 100000, 350 * the_pop / 100000,0,
		       750 * the_pop / 100000, 650 * the_pop / 100000,0),
                       byrow = TRUE, nrow = 5)

#Lockdown threshold 200, 350, 500, 800, 1400: VEs = 90, VEi=0, VEsymp=0, roll out 5000 / day -> y-axis = daily cases
row.names(interventions) = c(
			"Lockdown at 200",
			"Lockdown at 350",
			"Lockdown at 500",
			"Lockdown at 800",
			"Lockdown at 1400")

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)

int_rampup = 14				      # Time to achieve full intervention effect

vac_eff_hi = 0
vac_rate = as.numeric(rate)
vac_total = as.numeric(tot_vac)
vac_first = as.numeric(prior_group)
vac_eff_pi = 0
vac_eff_susc = 0.9
vac_eff_inf = 0
vac_on = 1
vac_exp_rate = 0

vac_init_doy = 366 + yday(ymd("2021-1-01"))     # Start of vaccination protocol
end_day = 366 + yday(ymd("2021-12-31"))

calib_doy = yday(ymd("2020-10-31"))     # End of model calibration

suffix=paste0(scen,"_triggers_SD_",min_sd,"_to_",max_sd)
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

cols = c("black","grey40","grey60","grey80","grey")
delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

# plot daily measures for treatment scenarios against social distancing background
#pdf(paste0("dan/daily_no_vaccine_",suffix,".pdf"), width = 8, height = 8)
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
                #y_lab = "Daily Diagnosed Cases", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:5,y_lim = c(0,1500),#lwd=lwds,
                y_lab = "Daily Diagnosed Cases", x_lim = NULL, col_pal = cols, col_idx = 1:5,y_lim = c(0,1000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
dev.off()
pdf(paste0("dan/daily_deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
deaths = scenarios_out$deaths
deaths = apply(deaths,2, diff)
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                #y_lab = "Daily Deaths", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:5,y_lim = c(0,12),#lwd=lwds,
                y_lab = "Daily Deaths", x_lim = NULL, col_pal = cols, col_idx = 1:5,y_lim = c(0,12),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
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
               y_lab = "Daily Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:5,y_lim = c(0,8000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)

dev.off()
pdf(paste0("dan/daily_hosps_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
cum_hosp = scenarios_out$cum_hosp
cum_hosp = apply(cum_hosp,2, diff)
plot_scenarios(doy, cum_hosp, yday(the_data$date), NA, 
                y_lab = "Daily Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:5,y_lim = c(0,100),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL)
dev.off()

