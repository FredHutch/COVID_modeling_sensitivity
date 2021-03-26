source("covid-model.R")
source("kc_read-data.R")

args<-commandArgs(trailingOnly=T)
dist<-args[1] # example: prop, adults, seniors
vei<-args[2] # example: 0, 0.25, etc.
rate<-args[3] # example: 5000
min_sd<-args[4] # how far to relax SD
trig_min<-args[5] # bi-weekly case rate per 100k pop for loosening SD
trig_max<-args[6] # bi-weekly case rate per 100k pop for tightening SD
vep<-args[7] # example: 0, 0.25, etc.
ves<-args[8] # example: 0, 0.25, etc.

# this loads the vaccination parameters
int_param_names = c("vac_on")
interventions = matrix(c(0, 1),
                       byrow = TRUE, nrow = 2)

row.names(interventions) = c("No Vaccine", "vax")

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)

int_rampup = 14				      # Time to achieve full intervention effect

vac_rate = as.numeric(rate)
vac_eff_inf1 = as.numeric(vei)
vac_eff_inf2 = as.numeric(vei)
vac_eff_pi1 = as.numeric(vep)
vac_eff_pi2 = as.numeric(vep)
vac_eff_susc1 = as.numeric(ves)
vac_eff_susc2 = as.numeric(ves)

vac_mutate=1
vac_mutate_time=366+yday(ymd("2021-1-01"))
new_strain_fact=1.55


vac_init_doy = 366 + yday(ymd("2021-1-15"))     # Start of vaccination protocol
vac_stop_doy = 366 + yday(ymd("2021-12-31"))     # End of vaccination protocol


suffix=paste0(dist,"_vei_",vei,"_sdmin_",min_sd,"_rate_",rate,"_trigs_",trig_min,"_",trig_max)
infile=paste0("../data/",suffix,".rds")
outfile=paste0("out/",suffix,".csv")
print(paste("Input file=",infile))

## base scenarios, columns are vaccine scenarios + no vaccine (1st)
scenarios_base=readRDS(file = infile)

# things to calculate & put in the table: 
#
# Cumulative cases from vaccine date,	Percent Reduction in cases,  Maximum daily cases since vaccine date,	
# Cumulative deaths from vaccine date,	Percent Reduction in  deaths, Maximum daily deaths,
# Cumulative hospitalizations from vaccine date, Percent Reduction in hospitalizations, Maximum current hospitalizations
# Average adult SD from vaccine date, Percent Reduction in avg adut SD, Days at max adult SD, Percent Reduction in Days at max adult SD

# vaccination date
start_date = 366 + yday("2021-1-15")

# one year later
end_date = 365 + start_date - 1

# two years later
end_date2 = 730 + start_date - 2

# helper functions
calc_max_daily = function(metric, start_idx, end_idx)
{
  apply(metric[(start_idx+1):end_idx,] - metric[start_idx:(end_idx-1),], 2, max, na.rm = TRUE)
}

calc_max_current = function(metric, start_idx, end_idx)
{
  apply(metric[start_idx:end_idx,], 2, max, na.rm = TRUE)
}

calc_min_current = function(metric, start_idx, end_idx)
{
  apply(metric[start_idx:end_idx,], 2, min, na.rm = TRUE)
}

calc_days_at_max = function(metric, start_idx, end_idx)
{
  max_array = apply(metric[start_idx:end_idx,], 2, max, na.rm = TRUE)
  days=c(rep(0,ncol(metric)))
  for (i in 1:ncol(metric)) 
  {
      for (j in start_idx:end_idx) 
      {
	  if (!is.na(metric[j,i]) && metric[j,i] >= max_array[i]-0.01)
	    days[i]=days[i]+1
      }
  }
  return(days)
}

calc_avg_current = function(metric, start_idx, end_idx)
{
  apply(metric[start_idx:end_idx,], 2, mean, na.rm = TRUE)
}

calc_quantiles = function(out_list, metric_name, end_idx, quantiles)
{
  t(sapply(1:length(out_list), 
           function(x) quantile(out_list[[x]][[metric_name]][end_idx,1:ncol(out_list[[x]][[metric_name]])], probs = quantiles) ))
}

calc_max_daily_quantiles = function(out_list, metric_name, start_idx, end_idx, quantiles)
{
  t(sapply(1:length(out_list), 
           function(x) quantile(
             apply(out_list[[x]][[metric_name]][(start_idx+1):end_idx,1:ncol(out_list[[x]][[metric_name]])] - out_list[[x]][[metric_name]][start_idx:(end_idx-1),1:ncol(out_list[[x]][[metric_name]])], 2, max, na.rm = TRUE), 
             probs = quantiles) ))
}

calc_max_current_quantiles = function(out_list, metric_name, start_idx, end_idx, quantiles)
{
  t(sapply(1:length(out_list), 
           function(x) quantile(
             apply(out_list[[x]][[metric_name]][start_idx:end_idx,1:ncol(out_list[[x]][[metric_name]])], 2, max, na.rm = TRUE), 
             probs = quantiles) ))
}

# base scenario
base_results = expand.grid(row.names(interventions))
names(base_results) = c("vaccine_eff")

start_idx = which(scenarios_base$doy == start_date)
end_idx = which(scenarios_base$doy == end_date)
end_idx2 = which(scenarios_base$doy == end_date2)

print(paste("Total infs at year 1:",scenarios_base$inf[end_idx,1],"(no vac)"))
print(paste("Total infs at year 2:",scenarios_base$inf[end_idx2,1],"(no vac)"))
print(paste("Total cases at year 1:",scenarios_base$cases[end_idx,1],"(no vac)"))
print(paste("Total cases at year 2:",scenarios_base$cases[end_idx2,1],"(no vac)"))
print(paste("Total deaths at year 1:",scenarios_base$deaths[end_idx,1],"(no vac)"))
print(paste("Total deaths at year 2:",scenarios_base$deaths[end_idx2,1],"(no vac)"))

base_results = plyr::rbind.fill(data.frame(vaccine_eff = "epidemic to 1/1/21"), base_results)

quan = c(0.1, 0.5, 0.9)
base_results$cum_deaths = c(scenarios_base$deaths[start_idx,1], # initial epidemic
                           scenarios_base$deaths[end_idx,])

base_results$new_deaths = c(scenarios_base$deaths[start_idx,1], # initial epidemic
                           scenarios_base$deaths[end_idx,] - scenarios_base$deaths[start_idx,])

base_results$max_daily_deaths = c(calc_max_daily(scenarios_base$deaths, 1, start_idx)[1],
                                  calc_max_daily(scenarios_base$deaths, start_idx, end_idx))
base_results$death_reduct = c(0,
                           signif(100*(scenarios_base$deaths[end_idx,1] - scenarios_base$deaths[end_idx,])/(scenarios_base$deaths[end_idx,1]-scenarios_base$deaths[start_idx,1]),2))

base_results$cum_cases = c(scenarios_base$cases[start_idx,1],
                           scenarios_base$cases[end_idx,])

base_results$new_cases = c(scenarios_base$cases[start_idx,1],
                           scenarios_base$cases[end_idx,] - scenarios_base$cases[start_idx,])

base_results$max_daily_cases = c(calc_max_daily(scenarios_base$cases, 1, start_idx)[1],
                                 calc_max_daily(scenarios_base$cases, start_idx, end_idx))

base_results$case_reduct = c(0,
                           signif(100*(scenarios_base$cases[end_idx,1] - scenarios_base$cases[end_idx,])/(scenarios_base$cases[end_idx,1]-scenarios_base$cases[start_idx,1]),2))

base_results$cum_infs = c(scenarios_base$inf[start_idx,1],
                          scenarios_base$inf[end_idx,]) 

base_results$cum_hosp = c(scenarios_base$cum_hosp[start_idx,1],
                          scenarios_base$cum_hosp[end_idx,]) 

base_results$new_hosp = c(scenarios_base$cum_hosp[start_idx,1],
                          scenarios_base$cum_hosp[end_idx,] - scenarios_base$cum_hosp[start_idx,]) 

base_results$max_current_hosp = c(calc_max_current(scenarios_base$hosp, 1, start_idx)[1],
                                  calc_max_current(scenarios_base$hosp, start_idx, end_idx))
base_results$hosp_reduct = c(0,
                           signif(100*(scenarios_base$cum_hosp[end_idx,1] - scenarios_base$cum_hosp[end_idx,])/
				(scenarios_base$cum_hosp[end_idx,1]-scenarios_base$cum_hosp[start_idx,1]),2))

#base_results$avg_sd = c(signif(calc_avg_current(sum(scenarios_base$sd_1,scenarios_base$sd_2,scenarios_base$sd_3,scenarios_base$sd_4)/4, 1, start_idx)[1],2),
#                                  signif(calc_avg_current(scenarios_base$sd_2, start_idx, end_idx),2))

base_results$avg_sd1 = c(signif(calc_avg_current(scenarios_base$sd_1, 1, start_idx)[1],2),
                                  signif(calc_avg_current(scenarios_base$sd_1, start_idx, end_idx),2))
base_results$avg_sd2 = c(signif(calc_avg_current(scenarios_base$sd_2, 1, start_idx)[1],2),
                                  signif(calc_avg_current(scenarios_base$sd_2, start_idx, end_idx),2))
base_results$avg_sd3 = c(signif(calc_avg_current(scenarios_base$sd_3, 1, start_idx)[1],2),
                                  signif(calc_avg_current(scenarios_base$sd_3, start_idx, end_idx),2))
base_results$avg_sd4 = c(signif(calc_avg_current(scenarios_base$sd_4, 1, start_idx)[1],2),
                                  signif(calc_avg_current(scenarios_base$sd_4, start_idx, end_idx),2))
base_results$avg_sd = c((base_results$avg_sd1 + base_results$avg_sd2 + base_results$avg_sd3 + base_results$avg_sd4)/4)


base_results$sd_at_max = c(calc_days_at_max(scenarios_base$sd_2, 1, start_idx)[1],
                                  calc_days_at_max(scenarios_base$sd_2, start_idx, end_idx))

base_results$sd_at_max_reduct = c(0,
                           signif(100*(calc_days_at_max(scenarios_base$sd_2, start_idx, end_idx)[1] - calc_days_at_max(scenarios_base$sd_2, start_idx, end_idx))/calc_days_at_max(scenarios_base$sd_2, start_idx, end_idx)[1],2))

base_results$max_Reff = c(signif(calc_max_current(scenarios_base$Reff, 1, start_idx)[1],2),
                                  signif(calc_max_current(scenarios_base$Reff, start_idx, end_idx),2))

base_results$min_Reff = c(signif(calc_min_current(scenarios_base$Reff, 1, start_idx)[1],2),
                                  signif(calc_min_current(scenarios_base$Reff, start_idx, end_idx),2))

base_results$avg_Reff = c(signif(calc_avg_current(scenarios_base$Reff, 1, start_idx)[1],2),
                                  signif(calc_avg_current(scenarios_base$Reff, start_idx, end_idx),2))

base_results$cum_deaths2 = c(scenarios_base$deaths[start_idx,1], # initial epidemic
                           scenarios_base$deaths[end_idx2,])

base_results$new_deaths2 = c(scenarios_base$deaths[start_idx,1], # initial epidemic
                           scenarios_base$deaths[end_idx2,] - scenarios_base$deaths[start_idx,])

base_results$max_daily_deaths2 = c(calc_max_daily(scenarios_base$deaths, 1, start_idx)[1],
                                  calc_max_daily(scenarios_base$deaths, start_idx, end_idx2))
base_results$death_reduct2 = c(0,
                           signif(100*(scenarios_base$deaths[end_idx2,1] - scenarios_base$deaths[end_idx2,])/(scenarios_base$deaths[end_idx2,1]-scenarios_base$deaths[start_idx,1]),2))

base_results$cum_cases2 = c(scenarios_base$cases[start_idx,1],
                           scenarios_base$cases[end_idx2,])

base_results$new_cases2 = c(scenarios_base$cases[start_idx,1],
                           scenarios_base$cases[end_idx2,] - scenarios_base$cases[start_idx,])

base_results$max_daily_cases2 = c(calc_max_daily(scenarios_base$cases, 1, start_idx)[1],
                                 calc_max_daily(scenarios_base$cases, start_idx, end_idx2))

base_results$case_reduct2 = c(0,
                           signif(100*(scenarios_base$cases[end_idx2,1] - scenarios_base$cases[end_idx2,])/(scenarios_base$cases[end_idx2,1]-scenarios_base$cases[start_idx,1]),2))

base_results$cum_hosp2 = c(scenarios_base$cum_hosp[start_idx,1],
                          scenarios_base$cum_hosp[end_idx2,]) 

base_results$new_hosp2 = c(scenarios_base$cum_hosp[start_idx,1],
                          scenarios_base$cum_hosp[end_idx2,] - scenarios_base$cum_hosp[start_idx,]) 

base_results$max_current_hosp2 = c(calc_max_current(scenarios_base$hosp, 1, start_idx)[1],
                                  calc_max_current(scenarios_base$hosp, start_idx, end_idx2))
base_results$hosp_reduct2 = c(0,
                           signif(100*(scenarios_base$cum_hosp[end_idx2,1] - scenarios_base$cum_hosp[end_idx2,])/
				(scenarios_base$cum_hosp[end_idx2,1]-scenarios_base$cum_hosp[start_idx,1]),2))

base_results$avg_sd1_2 = c(signif(calc_avg_current(scenarios_base$sd_1, 1, start_idx)[1],2),
                                  signif(calc_avg_current(scenarios_base$sd_1, start_idx, end_idx2),2))
base_results$avg_sd2_2 = c(signif(calc_avg_current(scenarios_base$sd_2, 1, start_idx)[1],2),
                                  signif(calc_avg_current(scenarios_base$sd_2, start_idx, end_idx2),2))
base_results$avg_sd3_2 = c(signif(calc_avg_current(scenarios_base$sd_3, 1, start_idx)[1],2),
                                  signif(calc_avg_current(scenarios_base$sd_3, start_idx, end_idx2),2))
base_results$avg_sd4_2 = c(signif(calc_avg_current(scenarios_base$sd_4, 1, start_idx)[1],2),
                                  signif(calc_avg_current(scenarios_base$sd_4, start_idx, end_idx2),2))
base_results$avg_sd_2 = c((base_results$avg_sd1_2 + base_results$avg_sd2_2 + base_results$avg_sd3_2 + base_results$avg_sd4_2)/4)


base_results$sd_at_max2 = c(calc_days_at_max(scenarios_base$sd_2, 1, start_idx)[1],
                                  calc_days_at_max(scenarios_base$sd_2, start_idx, end_idx2))

base_results$sd_at_max_reduct2 = c(0,
                           signif(100*(calc_days_at_max(scenarios_base$sd_2, start_idx, end_idx2)[1] - calc_days_at_max(scenarios_base$sd_2, start_idx, end_idx2))/calc_days_at_max(scenarios_base$sd_2, start_idx, end_idx2)[1],2))

base_results$max_Reff2 = c(signif(calc_max_current(scenarios_base$Reff, 1, start_idx)[1],2),
                                  signif(calc_max_current(scenarios_base$Reff, start_idx, end_idx2),2))

base_results$min_Reff2 = c(signif(calc_min_current(scenarios_base$Reff, 1, start_idx)[1],2),
                                  signif(calc_min_current(scenarios_base$Reff, start_idx, end_idx2),2))

base_results$avg_Reff2 = c(signif(calc_avg_current(scenarios_base$Reff, 1, start_idx)[1],2),
                                  signif(calc_avg_current(scenarios_base$Reff, start_idx, end_idx2),2))


write("scenario,min_sd,ve_s,ve_p,ve_i,vac_rate,cases since vax,%case reduct,max daily cases,hosp since vax,%hosp reduct,max current hosp,deaths since vax,%death reduct,max daily deaths,avg SD,avg child SD,days at max SD,%reduct days at max,max Reff,min Reff, avg Reff,tot cases,tot hosp,tot deaths,loosen,tighten,cases since vax yr2,%case reduct yr2,max daily cases yr2,hosp since vax yr2,%hosp reduct yr2,max current hosp yr2,deaths since vax yr2,%death reduct yr2,max daily deaths yr2,avg SD yr2,avg child SD yr2,days at max SD yr2,%reduct days at max yr2,max Reff yr2,min Reff yr2, avg Reff yr2,tot cases yr2,tot hosp yr2,tot deaths yr2,tot_infs", file = outfile, append=FALSE)
for (i in 1:nrow(base_results))
{
    ve_s=0
    ve_p=0
    ve_i=0
    if (i==1) {
	scen_name = "epidemic to 1/1/21"
	ve_s=0
	ve_i=0
	ve_p=0
	vrate = 0
    } else if (i==2) {
	ve_s=0
	ve_i=0
	ve_p=0
	vrate = 0
	scen_name = "No Vaccine"
    } else{
	ve_s=ves
	ve_p=vep
	ve_i=vei
	vrate=vac_rate
	scen_name = dist
	scen_name = gsub (",", " ", scen_name)
    }
    write(paste(scen_name,min_sd,ve_s,ve_p,ve_i,vrate,
	base_results$new_cases[i],base_results$case_reduct[i],base_results$max_daily_cases[i],
	base_results$new_hosp[i],base_results$hosp_reduct[i],base_results$max_current_hosp[i],
	base_results$new_deaths[i],base_results$death_reduct[i],base_results$max_daily_deaths[i],
	base_results$avg_sd[i], base_results$avg_sd1[i],
	base_results$sd_at_max[i],base_results$sd_at_max_reduct[i],
	base_results$max_Reff[i],base_results$min_Reff[i],base_results$avg_Reff[i],
	base_results$cum_cases[i],base_results$cum_hosp[i],base_results$cum_deaths[i],
	trig_min,trig_max,
	base_results$new_cases2[i],base_results$case_reduct2[i],base_results$max_daily_cases2[i],
	base_results$new_hosp2[i],base_results$hosp_reduct2[i],base_results$max_current_hosp2[i],
	base_results$new_deaths2[i],base_results$death_reduct2[i],base_results$max_daily_deaths2[i],
	base_results$avg_sd_2[i], base_results$avg_sd1_2[i],
	base_results$sd_at_max2[i],base_results$sd_at_max_reduct2[i],
	base_results$max_Reff2[i],base_results$min_Reff2[i],base_results$avg_Reff2[i],
	base_results$cum_cases2[i],base_results$cum_hosp2[i],base_results$cum_deaths2[i],
	base_results$cum_infs[i],sep=","), 
	file = outfile, append=TRUE) 
}
