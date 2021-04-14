setwd("..")
source("covid-model.R")
source("kc_read-data.R")


args<-commandArgs(trailingOnly=T)
infile<-args[1] # name of the RDS file
desc<-args[2] # what variable is changing
outfile<-args[3]
num_vals<-as.numeric(args[4]) # number of values used
val_array=vector (length=num_vals)
for (i in 1:num_vals)
{
    val_array[i]<-args[4+i]
}

scenarios_base=readRDS(file = infile)
print(names(scenarios_base))

header<-"time,date"
for (i in 1:num_vals) header<-paste(header,paste0(desc,i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("cumul infs",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("daily infs",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("daily vinfs",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("daily cases",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("daily vcases",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("daily hospitalizations",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("daily deaths",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("daily vdeaths",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("daily SD",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("% new variant",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("total vax",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("% vaccinated",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("0-19 vax",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("% 0-19 vax",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("20-49 vax",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("% 20-49 vax",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("50-69 vax",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("% 50-69 vax",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("70+ vax",i),sep=",")
for (i in 1:num_vals) header<-paste(header,paste0("% 70+ vax",i),sep=",")

write(header, file = outfile, append=FALSE)
for (i in 1:nrow(scenarios_base$inf))
{
    no_NAs=T
    for (j in 1:ncol(scenarios_base$inf))
	if (is.na(scenarios_base$inf[i,j]) || 
	    is.na(scenarios_base$cases[i,j]) || is.na(scenarios_base$cases[i-1,j]) || 
	    is.na(scenarios_base$deaths[i,j]) ||is.na(scenarios_base$deaths[i-1,j]) ||
	    is.na(scenarios_base$cum_hosp[i,j]) || is.na(scenarios_base$cum_hosp[i-1,j]))
	{
	    no_NAs=F
	    break
	}
    if (!no_NAs) next

    doy=scenarios_base$doy[i]
    date = as.Date(doy, origin = "2020-01-01")
    line<-paste(i,date,sep=",")
    for (j in 1:ncol(scenarios_base$inf))
	line<-paste(line,val_array[j],sep=",")

    for (j in 1:ncol(scenarios_base$inf))
	line<-paste(line,scenarios_base$inf[i,j],sep=",")

    infs = scenarios_base$inf
    infs = apply(infs,2, diff)
    for (j in 1:ncol(scenarios_base$inf))
	if (i==1)
	    line<-paste(line,scenarios_base$inf[1,j],sep=",")
	else
	    line<-paste(line,infs[i-1,j],sep=",")

    vinfs = scenarios_base$vinf
    vinfs = apply(vinfs,2, diff)
    for (j in 1:ncol(scenarios_base$vinf))
	if (i==1)
	    line<-paste(line,scenarios_base$vinf[1,j],sep=",")
	else
	    line<-paste(line,vinfs[i-1,j],sep=",")

    cases = scenarios_base$cases
    cases = apply(cases,2, diff)
    for (j in 1:ncol(cases))
	if (i==1)
	    line<-paste(line,scenarios_base$cases[1,j],sep=",")
	else
	    line<-paste(line,cases[i-1,j],sep=",")

    vcases = scenarios_base$vcases
    vcases = apply(vcases,2, diff)
    for (j in 1:ncol(vcases))
	if (i==1)
	    line<-paste(line,scenarios_base$vcases[1,j],sep=",")
	else
	    line<-paste(line,vcases[i-1,j],sep=",")

    hosps = scenarios_base$cum_hosp
    hosps = apply(hosps,2, diff)
    for (j in 1:ncol(hosps))
	if (i==1)
	    line<-paste(line,scenarios_base$cum_hosp[1,j],sep=",")
	else
	    line<-paste(line,hosps[i-1,j],sep=",")

    deaths = scenarios_base$deaths
    deaths = apply(deaths,2, diff)
    for (j in 1:ncol(deaths))
	if (i==1)
	    line<-paste(line,scenarios_base$deaths[1,j],sep=",")
	else
	    line<-paste(line,deaths[i-1,j],sep=",")

    vdeaths = scenarios_base$vdeaths
    vdeaths = apply(vdeaths,2, diff)
    for (j in 1:ncol(vdeaths))
	if (i==1)
	    line<-paste(line,scenarios_base$vdeaths[1,j],sep=",")
	else
	    line<-paste(line,vdeaths[i-1,j],sep=",")

    sd_other = scenarios_base$sd_2
    for (j in 1:ncol(sd_other))
    {
	line<-paste(line,sd_other[i,j],sep=",")
	#print(paste(i,j,sd_other[i,j]))
    }
    inf = scenarios_base$inf
    inf = apply(inf,2, diff)
    inf2 = scenarios_base$inf2
    inf2 = apply(inf2,2, diff)
    perc_inf = 100 * inf2 / inf
    for (j in 1:ncol(perc_inf))
	if (i==1)
	    line<-paste(line,0,sep=",")
	else
	{
	    line<-paste(line,perc_inf[i-1,j],sep=",")
	    #print(paste(i,j,perc_inf[i-1,j]))
	}

    vax = scenarios_base$vax
    for (j in 1:ncol(vax))
    {
	line<-paste(line,vax[i,j],sep=",")
    }

    vax = scenarios_base$vax
    perc_vax = 100*vax/scenarios_base$pop
    for (j in 1:ncol(vax))
    {
	line<-paste(line,perc_vax[i,j],sep=",")
    }
    vax = scenarios_base$vac_1
    for (j in 1:ncol(vax))
    {
	line<-paste(line,vax[i,j],sep=",")
    }
    perc_vax = scenarios_base$vac_1/scenarios_base$pop_1
    for (j in 1:ncol(vax))
    {
	line<-paste(line,perc_vax[i,j],sep=",")
    }
    vax = scenarios_base$vac_2
    for (j in 1:ncol(vax))
    {
	line<-paste(line,vax[i,j],sep=",")
    }
    perc_vax = scenarios_base$vac_2/scenarios_base$pop_2
    for (j in 1:ncol(vax))
    {
	line<-paste(line,perc_vax[i,j],sep=",")
    }
    vax = scenarios_base$vac_3
    for (j in 1:ncol(vax))
    {
	line<-paste(line,vax[i,j],sep=",")
    }
    perc_vax = scenarios_base$vac_3/scenarios_base$pop_3
    for (j in 1:ncol(vax))
    {
	line<-paste(line,perc_vax[i,j],sep=",")
    }
    vax = scenarios_base$vac_4
    for (j in 1:ncol(vax))
    {
	line<-paste(line,vax[i,j],sep=",")
    }
    perc_vax = scenarios_base$vac_4/scenarios_base$pop_4
    for (j in 1:ncol(vax))
    {
	line<-paste(line,perc_vax[i,j],sep=",")
    }

    print(line)
    write(line, file = outfile, append=TRUE) 
}
