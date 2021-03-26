#These commands print all kinds of graphs from the "worlds" scenarios to the shiny_out sub-directory
# scenarios_out and the file "suffix" must be setup already
#note: always pass alpha on the 0-255 scale

makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}


cols = rainbow(27)
cols = makeTransparent(cols,50)

x_lim = NULL

vac_init_doy = vac_schedule[1,1]

pdf(paste0("shiny_out/infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Cumulative Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], totalpop = the_pop,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
pdf(paste0("shiny_out/deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$deaths, yday(the_data$date), NA, 
               y_lab = "Cumulative Deaths", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,2000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = NULL, scenarios_out$sd_2[,1], totalpop = the_pop,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
# plot all panels for column B of suppl fig 2
pdf(paste0("shiny_out/",suffix,".pdf"), width = 5, height=11)
par(mfrow=c(6,1),mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

doy = scenarios_out$doy
doy = doy[-1]

cases = scenarios_out$cases
cases = apply(cases,2, diff)
plot_scenarios(doy, cases, yday(the_data$date), the_data$cases, 
                #y_lab = "Daily Diagnosed Cases", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:27,y_lim = c(0,1500),#lwd=lwds,
                y_lab = "Daily Diagnosed Cases", x_lim = NULL, col_pal = cols, col_idx = 1:27,y_lim = c(0,3000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

plot_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Cumulative Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

cum_hosp = scenarios_out$cum_hosp
cum_hosp = apply(cum_hosp,2, diff)
plot_scenarios(doy, cum_hosp, yday(the_data$date), NA, 
                y_lab = "Daily Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,150),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

deaths = scenarios_out$deaths
deaths = apply(deaths,2, diff)
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                #y_lab = "Daily Deaths", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:27,y_lim = c(0,25),#lwd=lwds,
                y_lab = "Daily Deaths", x_lim = NULL, col_pal = cols, col_idx = 1:27,y_lim = c(0,30),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

plot_scenarios(scenarios_out$doy, scenarios_out$sd_2, yday(the_data$date), NA, 
                y_lab = "Social Distancing (Non-seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,1),#lwd=lwds,
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
               y_lab = "% Daily Infections New Variant", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:27,y_lim = c(0,100),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,all_months=1,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()

# plot daily measures for treatment scenarios against social distancing background
#pdf(paste0("shiny_out/daily_vaccine_effects_",suffix,".pdf"), width = 8, height = 8)
#startx= 366 + yday(ymd("2021-4-01"))     # Start of x-axis
#endx= 366 + yday(ymd("2021-12-01"))     # End of x-axis
pdf(paste0("shiny_out/daily_cases_",suffix,".pdf"), width = 5, height = 3.5)
x_lim = NULL
#par(mfrow = c(2,2), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

doy = scenarios_out$doy
doy = doy[-1]

cases = scenarios_out$cases
cases = apply(cases,2, diff)
plot_scenarios(doy, cases, yday(the_data$date), the_data$cases, 
                y_lab = "Daily Diagnosed Cases", x_lim = NULL, col_pal = cols, col_idx = 1:27,y_lim = c(0,1500),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
pdf(paste0("shiny_out/daily_deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
deaths = scenarios_out$deaths
deaths = apply(deaths,2, diff)
plot_scenarios(doy, deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Daily Deaths", x_lim = NULL, col_pal = cols, col_idx = 1:27,y_lim = c(0,20),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
#legend("topleft",
#       legend = c(0, NA, NA, NA, NA, 0.5, NA, NA, NA, NA, 1),
#       fill = colorRampPalette(colors = c(c("white", "gray30")))(11),
#       border = NA, bty = "n", y.intersp = 0.5,
#       title = "social distancing")

dev.off()
pdf(paste0("shiny_out/daily_perc_inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
inf2 = scenarios_out$inf2
inf2 = apply(inf2,2, diff)
perc_inf = 100 * inf2 / inf
startx= 366 + yday(ymd("2021-1-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-11-01"))     # End of x-axis
plot_scenarios(doy, perc_inf, yday(the_data$date), NA, 
               y_lab = "% Daily Infections New Variant", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:27,y_lim = c(0,100),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,all_months=1,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
pdf(paste0("shiny_out/daily_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,10000),#lwd=lwds, 
		delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
pdf(paste0("shiny_out/log_daily_infs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf
inf = apply(inf,2, diff)
plot_scenarios(doy, log10(inf), yday(the_data$date), NA, 
               y_lab = "Log Daily Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,4),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
pdf(paste0("shiny_out/daily_hosps_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
cum_hosp = scenarios_out$cum_hosp
cum_hosp = apply(cum_hosp,2, diff)
plot_scenarios(doy, cum_hosp, yday(the_data$date), NA, 
                y_lab = "Daily Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,150),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()

pdf(paste0("shiny_out/reff_",suffix,".pdf"), width = 5, height = 3.5)
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$Reff, yday(the_data$date), NA, 
                y_lab = "R Effective", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,2),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))),
		target=1)
dev.off()
pdf(paste0("shiny_out/snr_SD_",suffix,".pdf"), width = 5, height = 3.5)
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$sd_4, yday(the_data$date), NA, 
                y_lab = "Social Distancing (Seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,1),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
pdf(paste0("shiny_out/other_SD_",suffix,".pdf"), width = 5, height = 3.5)
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$sd_2, yday(the_data$date), NA, 
                y_lab = "Social Distancing (Non-seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,1),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
pdf(paste0("shiny_out/case_rates_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenario_rates(scenarios_out$doy, scenarios_out$cases, yday(the_data$date), the_data$cases, 
                y_lab = "2 week Case Rates / 100k Pop", x_lim = x_lim, col_pal = cols, col_idx = 1:27, y_lim = c(0,1500),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))),
		limit=100000*calib_params$dynamic_sd_limit/the_pop, hyster=100000*calib_params$dynamic_sd_hyster/the_pop)
dev.off()

# plot treatment reduction scenarios against social distancing background
#pdf(paste0("shiny_out/vaccine_reductions_",suffix,".pdf"), width = 8, height = 8)
pdf(paste0("shiny_out/case_reduct_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
x_lim = NULL
#par(mfrow = c(2,2), mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))

plot_delta_scenarios(scenarios_out$doy, scenarios_out$cases, yday(the_data$date), the_data$cases, 
                y_lab = "Reduction in Diagnosed Cases", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,120000),#lwd=lwds,
                delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()

pdf(paste0("shiny_out/death_reduct_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_delta_scenarios(scenarios_out$doy, scenarios_out$deaths, yday(the_data$date), the_data$deaths, 
                y_lab = "Reduction in Deaths", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,500),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
pdf(paste0("shiny_out/inf_reduct_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_delta_scenarios(scenarios_out$doy, scenarios_out$inf, yday(the_data$date), NA, 
               y_lab = "Reduction in Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
pdf(paste0("shiny_out/hosp_reduct_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_delta_scenarios(scenarios_out$doy, scenarios_out$cum_hosp, yday(the_data$date), NA, 
                y_lab = "Reduction in Hospitalizations", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,15000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
pdf(paste0("shiny_out/susc_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$susc_1, yday(the_data$date), NA, 
                y_lab = "Susceptible (0-19)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$susc_2, yday(the_data$date), NA, 
                y_lab = "Susceptible (20-49)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$susc_3, yday(the_data$date), NA, 
                y_lab = "Susceptible (50-69)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$susc_4, yday(the_data$date), NA, 
                y_lab = "Susceptible (seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
startx= 366 + yday(ymd("2021-1-01"))     # Start of x-axis
endx= 366 + yday(ymd("2021-11-01"))     # End of x-axis
pdf(paste0("shiny_out/vax_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$vax, yday(the_data$date), NA, 
                y_lab = "Total Vaccinated", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:27,y_lim = c(0,the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = vax_calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
pdf(paste0("shiny_out/daily_vax_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
vax = scenarios_out$vax
vax = apply(vax,2, diff)
plot_scenarios(doy, vax, yday(the_data$date), NA, 
                y_lab = "Daily Vaccinated", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:27,y_lim = c(0,12000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = vax_calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
pdf(paste0("shiny_out/vacs_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$vac_1, yday(the_data$date), NA, 
                y_lab = "Vaccinated (0-19)", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:27,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = vax_calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$vac_2, yday(the_data$date), NA, 
                y_lab = "Vaccinated (20-49)", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:27,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = vax_calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$vac_3, yday(the_data$date), NA, 
                y_lab = "Vaccinated (50-69)", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:27,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = vax_calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$vac_4, yday(the_data$date), NA, 
                y_lab = "Vaccinated (seniors)", x_lim = c(startx,endx), col_pal = cols, col_idx = 1:27,y_lim = c(0,1e6),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = vax_calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
pdf(paste0("shiny_out/age_deaths_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$deaths_1, yday(the_data$date), NA, 
                y_lab = "Deaths (0-19)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,3000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$deaths_2, yday(the_data$date), NA, 
                y_lab = "Deaths (20-49)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,3000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$deaths_3, yday(the_data$date), NA, 
                y_lab = "Deaths (50-69)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,3000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$deaths_4, yday(the_data$date), NA, 
                y_lab = "Deaths (seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,3000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
pdf(paste0("shiny_out/age_hosps_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
#par(mfrow = c(1,1), mar = 0.1 + c(3, 4, 1, 3), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$hosp_1, yday(the_data$date), NA, 
                y_lab = "Hosp (0-19)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,400),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$hosp_2, yday(the_data$date), NA, 
                y_lab = "Hosp (20-49)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,400),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$hosp_3, yday(the_data$date), NA, 
                y_lab = "Hosp (50-69)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,400),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
plot_scenarios(scenarios_out$doy, scenarios_out$hosp_4, yday(the_data$date), NA, 
                y_lab = "Hosp (seniors)", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,400),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = 0,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))
dev.off()
pdf(paste0("shiny_out/inf1_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf1, yday(the_data$date), NA, 
               y_lab = "Main Strain Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
pdf(paste0("shiny_out/inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
plot_scenarios(scenarios_out$doy, scenarios_out$inf2, yday(the_data$date), NA, 
               y_lab = "New Strain Infections", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,0.6*the_pop),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], totalpop = the_pop,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
pdf(paste0("shiny_out/daily_inf1_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf1
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections Main Strain", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,8000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
pdf(paste0("shiny_out/daily_inf2_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(3, 4, 1, 4), mgp = c(3, 0.5, 0), oma = c(3,2,0,0))
inf = scenarios_out$inf2
inf = apply(inf,2, diff)
plot_scenarios(doy, inf, yday(the_data$date), NA, 
               y_lab = "Daily Infections New Strain", x_lim = x_lim, col_pal = cols, col_idx = 1:27,y_lim = c(0,8000),#lwd=lwds,
               delta = NULL, vaccination_date = vac_init_doy, calib_date = calib_doy, scenarios_out$sd_2[,1], NULL,
       		lty = c(rep(1, nrow(interventions)+1)), lwd = c(2, rep(1, nrow(interventions))))

dev.off()
pdf(paste0("shiny_out/legend_",suffix,".pdf"), width = 5, height = 3.5)
par(mar = 0.1 + c(2, 1, 1, 4), mgp = c(2, 0.5, 0), oma = c(2,1,0,0))
    plot.new()
    legend("topleft", 
       legend = c(row.names(interventions), "Calibration End", "Vaccination Start"), 
       col = c(cols, "black","orange"), lty = c(rep(1, nrow(interventions)+1), 2,2), 
       lwd = c(2, rep(1, nrow(interventions)), 1,1), bty = "n" , cex=1.5)
dev.off()

