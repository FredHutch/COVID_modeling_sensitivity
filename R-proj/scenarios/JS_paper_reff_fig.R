set.seed(20)
print_legend = 0

source("covid-model.R")
source("kc_read-data.R")

args<-commandArgs(trailingOnly=T)
scen<-"seniors"
min_sd<-0.3
max_sd<-0.6
rate<-args[1]
pop_fract<-0.9
prior_group<-4
trig_min<-100
trig_max<-350
b117<-args[2]

tot_vac= as.integer((as.numeric(pop_fract) * the_pop + 5000)/10000) * 10000

sd_delta = 0 # SD release rate same as tightening rate (over 2 week period)

if (b117 == 1)
{
    new_strain_intros=0.25
    new_strain_fact<-1.55 # relative strength of 2nd strain (50% increase)
    new_strain_severity<-1.55 # impact on hosp & death vs main variant
    vac_mutate=1
    vac_mutate_time=366+yday(ymd("2021-1-01"))

} else {
    new_strain_intros=0
    new_strain_fact<-1 # relative strength of 2nd strain (50% increase)
    new_strain_severity<-1 # impact on hosp & death vs main variant
    vac_mutate=0
    vac_mutate_time=366+yday(ymd("2021-1-01"))
}

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
calib_params$vac_eff_p1 = 0.9
calib_params$vac_eff_p2 = 0.9

end_day = 366 + yday(ymd("2021-12-31")) # End of the run

calib_params$calib_doy = yday(ymd("2020-12-31"))
calib_doy = calib_params$calib_doy

vax_calib_doy = 366 + yday(ymd("2021-4-02"))
vac_stop_doy = 366 + yday(ymd("2021-12-31"))     # End of vaccination protocol
vac_init_doy = 366 + yday(ymd("2021-1-01"))     # Start of vaccination protocol

if (b117 == 1)
{
   suffix=paste0("B117_fig6_",rate)
} else {
   suffix=paste0("No_Var_fig6_",rate)
}
print(suffix)

scenarios_out = get_model_data_param_sets(interventions, int_param_names, calib_params, end_day, state)
saveRDS(scenarios_out, file = paste0("../data/",suffix,".rds"))

# If you want to test/improve the plotting more rapidly, you can comment out the lines above and just load
# the saved rds file with the readRDS command below (provided it has been run since the latest model changes)
#
#scenarios_out=readRDS(file = paste0("../data/",suffix,".rds"))

cols = c("black", "darkblue","blue","lightblue","darkgreen","green","lightgreen","darkred","red","pink")
x_lim = NULL
startx= 366 + yday(ymd("2021-4-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-12-01"))     # End of x-axis

# plot treatment scenarios against social distancing background
pdf(paste0("papers_out/reff_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$Reff, yday(the_data$date), NA, 
                y_lab = "R Effective", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,2),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, 10)), lwd = c(2, rep(1, 9)),all_months=1,
		target=1)
dev.off()
pdf(paste0("papers_out/vax_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$vax, yday(the_data$date), NA, 
                y_lab = "Total Vaccinated", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:10,y_lim = c(0,the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = vax_calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop,all_months=1,
       		lty = c(rep(1, 10)), lwd = c(2, rep(1, 9)))
dev.off()
pdf(paste0("papers_out/infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Cumulative Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:10,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], totalpop = the_pop,all_months=1,
       		lty = c(rep(1, 10)), lwd = c(2, rep(1, 9)))

dev.off()
pdf(paste0("papers_out/legend_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 1, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,1,0,0))
    plot.new()
    legend("topleft", 
       legend = row.names(interventions),
       col = cols,
       text.col = cols,
	lty = c(rep(1, 10)), lwd = c(2, rep(2, 9)), bty = "n" , cex=1)
dev.off()
pdf(paste0("papers_out/daily_cases_",suffix,".pdf"), width = 5, height = 3.5)
#par(mfrow = c(2,2), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

doy = scenarios_out$doy
doy = doy[-1]

cases = scenarios_out$cases
cases = apply(cases,2, diff)
plot_scenarios(doy, cases, yday(the_data$date), the_data$cases, 
                y_lab = "Daily Diagnosed Cases", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:243,y_lim = c(0,3000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,all_months=1,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
pdf(paste0("papers_out/",suffix,"_vacs.pdf"), width = 5, height = 3.5)
x_lim = NULL
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

plot_vac_reff_correl(scenarios_out$doy, scenarios_out$vax, scenarios_out$Reff, yday(the_data$date), the_data$cases, 
                y_lab = "Vaccinated at Reff=1", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:10,y_lim = c(0,the_pop),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)
if(print_legend==1) {
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), pch = c(rep(8,3),rep(9,3),rep(10,3),8,12),
       lty = c(rep(NA, nrow(interventions)), NA,NA), bty = "n" , cex=0.8)
    mtext(paste("Distrib:",scen,"Triggers:",trig_min,"/",trig_max,"VE_I=",vac_eff_inf,"Rate=",rate,"Total Doses=",tot_vac),outer = TRUE, cex = 1.2)
}

dev.off()

pdf(paste0("papers_out/",suffix,"_infs.pdf"), width = 5, height = 3.5)
x_lim = NULL
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

startx= 366 + yday(ymd("2021-4-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-12-01"))     # End of x-axis
plot_vac_reff_correl(scenarios_out$doy, scenarios_out$inf, scenarios_out$Reff, yday(the_data$date), the_data$cases, 
                y_lab = "Infected at Reff=1", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:10,y_lim = c(0,0.25*the_pop),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)
if(print_legend==1) {
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), pch = c(rep(8,3),rep(9,3),rep(10,3),8,12),
       lty = c(rep(NA, nrow(interventions)), NA,NA), bty = "n" , cex=0.8)
    mtext(paste("Distrib:",scen,"Triggers:",trig_min,"/",trig_max,"VE_I=",vac_eff_inf,"Rate=",rate,"Total Doses=",tot_vac),outer = TRUE, cex = 1.2)
}

dev.off()
pdf(paste0("papers_out/",suffix,"_age_vacs.pdf"), width = 5, height = 3.5)
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
