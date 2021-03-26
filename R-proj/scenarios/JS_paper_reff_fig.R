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
set<-as.integer(args[10]) # which vep set to plot (1-3)
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
#cols=c("black","lightblue","blue","darkblue","pink","red","darkred","lightgray","gray","darkgray")
cols = c("black", "pink","lightgreen","lightblue","red","green","blue","darkred","darkgreen","darkblue")

int_param_names = c("vac_eff_susc", "vac_eff_pi", "vac_eff_inf", "vac_on")
interventions = matrix(c(0, 0, 0, 0,
                         0.1, 0.1, 0.1, 1,
                         0.5, 0.1, 0.1, 1,
                         0.9, 0.1, 0.1, 1,
                         0.1, 0.1, 0.5, 1,
                         0.5, 0.1, 0.5, 1,
                         0.9, 0.1, 0.5, 1,
                         0.1, 0.1, 0.9, 1,
                         0.5, 0.1, 0.9, 1,
                         0.9, 0.1, 0.9, 1,
                         0.1, 0.5, 0.1, 1,
                         0.5, 0.5, 0.1, 1,
                         0.9, 0.5, 0.1, 1,
                         0.1, 0.5, 0.5, 1,
                         0.5, 0.5, 0.5, 1,
                         0.9, 0.5, 0.5, 1,
                         0.1, 0.5, 0.9, 1,
                         0.5, 0.5, 0.9, 1,
                         0.9, 0.5, 0.9, 1,
                         0.1, 0.9, 0.1, 1,
                         0.5, 0.9, 0.1, 1,
                         0.9, 0.9, 0.1, 1,
                         0.1, 0.9, 0.5, 1,
                         0.5, 0.9, 0.5, 1,
                         0.9, 0.9, 0.5, 1,
                         0.1, 0.9, 0.9, 1,
                         0.5, 0.9, 0.9, 1,
                         0.9, 0.9, 0.9, 1),
                       byrow = TRUE, nrow = 28)

row.names(interventions) = c("No Vaccine", 
				"10% VE_S,10% VE_P,10% VE_I", "50% VE_S,10% VE_P,10% VE_I","90% VE_S,10% VE_P,10% VE_I", 
				"10% VE_S,10% VE_P,50% VE_I", "50% VE_S,10% VE_P,50% VE_I","90% VE_S,10% VE_P,50% VE_I",
				"10% VE_S,10% VE_P,90% VE_I", "50% VE_S,10% VE_P,50% VE_I","90% VE_S,10% VE_P,90% VE_I",
				"10% VE_S,50% VE_P,10% VE_I", "50% VE_S,50% VE_P,10% VE_I","90% VE_S,50% VE_P,10% VE_I", 
				"10% VE_S,50% VE_P,50% VE_I", "50% VE_S,50% VE_P,50% VE_I","90% VE_S,50% VE_P,50% VE_I",
				"10% VE_S,50% VE_P,90% VE_I", "50% VE_S,50% VE_P,50% VE_I","90% VE_S,50% VE_P,90% VE_I",
				"10% VE_S,10% VE_P,90% VE_I", "50% VE_S,90% VE_P,10% VE_I","90% VE_S,90% VE_P,10% VE_I", 
				"10% VE_S,10% VE_P,90% VE_I", "50% VE_S,90% VE_P,50% VE_I","90% VE_S,90% VE_P,50% VE_I",
				"10% VE_S,10% VE_P,90% VE_I", "50% VE_S,90% VE_P,50% VE_I","90% VE_S,90% VE_P,90% VE_I")

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)

int_rampup = 14				      # Time to achieve full intervention effect

vac_eff_hi = 0
vac_rate = as.numeric(rate)
vac_total = as.numeric(tot_vac)
vac_first = as.numeric(prior_group)
vac_exp_rate = 0

vac_init_doy = 366 + yday(ymd("2021-1-01"))     # Start of vaccination protocol
end_day = 366 + yday(ymd("2021-12-31"))

calib_doy = yday(ymd("2020-10-31"))     # End of model calibration

suffix=paste0(scen,"Vx_Re_SD_",min_sd,"_to_",max_sd,"_rate_",rate,"_tot_",tot_vac,"_trigs_",trig_min,"_",trig_max)
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

#scenarios_out = get_model_data_param_sets(interventions, int_param_names, calib_params, end_day, state)
#saveRDS(scenarios_out, file = paste0("../data/scenarios_vacc_",suffix,".rds"))

if (as.numeric(plot_em) == 0) quit()

#
# If you want to test/improve the plotting more rapidly, you can comment out the lines above and just load
# the saved rds file with the readRDS command below (provided it has been run since the latest model changes)
#
scenarios_out=readRDS(file = paste0("../data/scenarios_vacc_",suffix,".rds"))

# plot treatment scenarios against social distancing background
#pdf(paste0("out/vaccine_",suffix,".pdf"), width = 8, height = 8)
pdf(paste0("josh/",suffix,"_vacs_",set,".pdf"), width = 5, height = 3.5)
x_lim = NULL
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

startx= 366 + yday(ymd("2021-4-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-12-01"))     # End of x-axis
plot_vac_reff_correl(set,scenarios_out$doy, scenarios_out$vax, scenarios_out$Reff, yday(the_data$date), the_data$cases, 
                y_lab = "Vaccinated at Reff=1", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:10,y_lim = c(0,0.6*the_pop),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)
if(print_legend==1) {
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), pch = c(rep(8,3),rep(9,3),rep(10,3),8,12),
       lty = c(rep(NA, nrow(interventions)), NA,NA), bty = "n" , cex=0.8)
    mtext(paste("Distrib:",scen,"Triggers:",trig_min,"/",trig_max,"VE_I=",vac_eff_inf,"Rate=",rate,"Total Doses=",tot_vac),outer = TRUE, cex = 1.2)
}

dev.off()

pdf(paste0("josh/",suffix,"_infs_",set,".pdf"), width = 5, height = 3.5)
x_lim = NULL
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

delta = c(calib_params$delta1_doy, calib_params$delta2_doy, calib_params$delta3_doy, calib_params$delta4_doy, calib_params$delta5_doy, calib_params$delta6_doy)

startx= 366 + yday(ymd("2021-4-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-12-01"))     # End of x-axis
plot_vac_reff_correl(set,scenarios_out$doy, scenarios_out$inf, scenarios_out$Reff, yday(the_data$date), the_data$cases, 
                y_lab = "Infected at Reff=1", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:10,y_lim = NULL,#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop)
if(print_legend==1) {
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), pch = c(rep(8,3),rep(9,3),rep(10,3),8,12),
       lty = c(rep(NA, nrow(interventions)), NA,NA), bty = "n" , cex=0.8)
    mtext(paste("Distrib:",scen,"Triggers:",trig_min,"/",trig_max,"VE_I=",vac_eff_inf,"Rate=",rate,"Total Doses=",tot_vac),outer = TRUE, cex = 1.2)
}

dev.off()
