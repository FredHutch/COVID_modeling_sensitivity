
#########################################################
### A) Installing and loading required packages
#########################################################

if (!require("lattice")) {
   install.packages("gplots", dependencies = TRUE)
   library(gplots)
   }
if (!require("gplots")) {
   install.packages("gplots", dependencies = TRUE)
   library(gplots)
   }
if (!require("RColorBrewer")) {
   install.packages("RColorBrewer", dependencies = TRUE)
   library(RColorBrewer)
   }
library(mgcv)

#########################################################
### B) Reading in data and transform it into matrix format
#########################################################
data <- read.csv("seniors_SD_0.2_to_0.6_rate_5000_tot_1100000_trigs_100_300.csv", comment.char="#", header=T)
#scenario,min_sd,sd_delta,ve_s,ve_p,ve_i,vac_rate,vac_total,cases since vax,%case reduct,max daily cases,hosp since vax,%hosp reduct,max current hosp,deaths since vax,%death reduct,max daily deaths,avg SD,avg child SD,days at max SD,%reduct days at max,max Reff.min Reff, avg Reff,tot cases,tot hosp,tot deaths

rnames <- data[1,]  
mat_data <- data.matrix(data[,1:ncol(data)])
colnames(mat_data)<- rnames   
library(latticeExtra)
col.l <- colorRampPalette(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'))
col.divs<-20

#inputs of interest
ve_s<-vector(length=nrow(mat_data))
ve_p<-vector(length=nrow(mat_data))
ve_inf<-vector(length=nrow(mat_data))

#outputs of interest
max_daily_cases<-vector(length=nrow(mat_data))
cases<-vector(length=nrow(mat_data))
deaths<-vector(length=nrow(mat_data))
max_daily_deaths<-vector(length=nrow(mat_data))
days_at_max_SD<-vector(length=nrow(mat_data))
avg_SD<-vector(length=nrow(mat_data))

pdf("JS_paper_figs_2_3.pdf",width=4,height=4)
par.settings=list(axis.text=list(fontfamily="sans",font=10,fontface="bold"),
	xlab.text=list(fontfamily="sans",font=14,fontface="bold"),
        ylab.text=list(fontfamily="sans",font=14,fontface="bold"),
        main.text=list(fontfamily="times",font=14,fontface="bold"))
par(mfrow = c(3,2))

start_cases=as.integer(mat_data[1,9])
start_deaths=as.integer(mat_data[1,15])

slice_cnt=0
max_cases=0
max_deaths=0
min_cases=1e6
min_deaths=10000

for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,6])==0.1 )
    {
	slice_cnt=slice_cnt+1
	ve_s[slice_cnt]<-as.numeric(mat_data[i,4])
	ve_p[slice_cnt]<-as.numeric(mat_data[i,5])
	#ve_inf[slice_cnt]<-as.numeric(mat_data[i,6])
	max_daily_cases[slice_cnt]<-as.numeric(mat_data[i,11])
	if (max_daily_cases[slice_cnt] > max_cases) max_cases=max_daily_cases[slice_cnt]
	cases[slice_cnt]<-as.numeric(mat_data[i,9])
	if (cases[slice_cnt] > max_cases) max_cases=cases[slice_cnt]
	if (cases[slice_cnt] < min_cases) min_cases=cases[slice_cnt]
	max_daily_deaths[slice_cnt]<-as.numeric(mat_data[i,17])
	if (max_daily_deaths[slice_cnt] > max_deaths) max_deaths=max_daily_deaths[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,15])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,20])
	avg_SD[slice_cnt]<-as.numeric(mat_data[i,18])
    }
}
print(slice_cnt)
part_A.df = data.frame(x = ve_s[1:slice_cnt],y=ve_p[1:slice_cnt],z=deaths[1:slice_cnt])

part_B.df = data.frame(x = ve_s[1:slice_cnt],y=ve_p[1:slice_cnt],z=cases[1:slice_cnt])

slice_cnt=0

for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,6])==0.5 )
    {
	slice_cnt=slice_cnt+1
	ve_s[slice_cnt]<-as.numeric(mat_data[i,4])
	ve_p[slice_cnt]<-as.numeric(mat_data[i,5])
	#ve_inf[slice_cnt]<-as.numeric(mat_data[i,6])
	max_daily_cases[slice_cnt]<-as.numeric(mat_data[i,11])
	if (max_daily_cases[slice_cnt] > max_cases) max_cases=max_daily_cases[slice_cnt]
	cases[slice_cnt]<-as.numeric(mat_data[i,9])
	if (cases[slice_cnt] > max_cases) max_cases=cases[slice_cnt]
	if (cases[slice_cnt] < min_cases) min_cases=cases[slice_cnt]
	max_daily_deaths[slice_cnt]<-as.numeric(mat_data[i,17])
	if (max_daily_deaths[slice_cnt] > max_deaths) max_deaths=max_daily_deaths[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,15])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,20])
	avg_SD[slice_cnt]<-as.numeric(mat_data[i,18])
    }
}
part_C.df = data.frame(x = ve_s[1:slice_cnt],y=ve_p[1:slice_cnt],z=deaths[1:slice_cnt])

part_D.df = data.frame(x = ve_s[1:slice_cnt],y=ve_p[1:slice_cnt],z=cases[1:slice_cnt])


slice_cnt=0


for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,6])==0.9 )
    {
	slice_cnt=slice_cnt+1
	ve_s[slice_cnt]<-as.numeric(mat_data[i,4])
	ve_p[slice_cnt]<-as.numeric(mat_data[i,5])
	#ve_inf[slice_cnt]<-as.numeric(mat_data[i,6])
	max_daily_cases[slice_cnt]<-as.numeric(mat_data[i,11])
	if (max_daily_cases[slice_cnt] > max_cases) max_cases=max_daily_cases[slice_cnt]
	cases[slice_cnt]<-as.numeric(mat_data[i,9])
	if (cases[slice_cnt] > max_cases) max_cases=cases[slice_cnt]
	if (cases[slice_cnt] < min_cases) min_cases=cases[slice_cnt]
	max_daily_deaths[slice_cnt]<-as.numeric(mat_data[i,17])
	if (max_daily_deaths[slice_cnt] > max_deaths) max_deaths=max_daily_deaths[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,15])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,20])
	avg_SD[slice_cnt]<-as.numeric(mat_data[i,18])
    }
}
part_E.df = data.frame(x = ve_s[1:slice_cnt],y=ve_p[1:slice_cnt],z=deaths[1:slice_cnt])

part_F.df = data.frame(x = ve_s[1:slice_cnt],y=ve_p[1:slice_cnt],z=cases[1:slice_cnt])

#VEinf vs VEsusc plots

slice_cnt=0


for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,5])==0.1 )
    {
	slice_cnt=slice_cnt+1
	ve_s[slice_cnt]<-as.numeric(mat_data[i,4])
	#ve_p[slice_cnt]<-as.numeric(mat_data[i,5])
	ve_inf[slice_cnt]<-as.numeric(mat_data[i,6])
	max_daily_cases[slice_cnt]<-as.numeric(mat_data[i,11])
	if (max_daily_cases[slice_cnt] > max_cases) max_cases=max_daily_cases[slice_cnt]
	cases[slice_cnt]<-as.numeric(mat_data[i,9])
	if (cases[slice_cnt] > max_cases) max_cases=cases[slice_cnt]
	if (cases[slice_cnt] < min_cases) min_cases=cases[slice_cnt]
	max_daily_deaths[slice_cnt]<-as.numeric(mat_data[i,17])
	if (max_daily_deaths[slice_cnt] > max_deaths) max_deaths=max_daily_deaths[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,15])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,20])
	avg_SD[slice_cnt]<-as.numeric(mat_data[i,18])
    }
}

part_G.df = data.frame(x = ve_s[1:slice_cnt],y=ve_inf[1:slice_cnt],z=deaths[1:slice_cnt])

part_H.df = data.frame(x = ve_s[1:slice_cnt],y=ve_inf[1:slice_cnt],z=cases[1:slice_cnt])

slice_cnt=0


for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,5])==0.5 )
    {
	slice_cnt=slice_cnt+1
	ve_s[slice_cnt]<-as.numeric(mat_data[i,4])
	#ve_p[slice_cnt]<-as.numeric(mat_data[i,5])
	ve_inf[slice_cnt]<-as.numeric(mat_data[i,6])
	max_daily_cases[slice_cnt]<-as.numeric(mat_data[i,11])
	if (max_daily_cases[slice_cnt] > max_cases) max_cases=max_daily_cases[slice_cnt]
	cases[slice_cnt]<-as.numeric(mat_data[i,9])
	if (cases[slice_cnt] > max_cases) max_cases=cases[slice_cnt]
	if (cases[slice_cnt] < min_cases) min_cases=cases[slice_cnt]
	max_daily_deaths[slice_cnt]<-as.numeric(mat_data[i,17])
	if (max_daily_deaths[slice_cnt] > max_deaths) max_deaths=max_daily_deaths[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,15])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,20])
	avg_SD[slice_cnt]<-as.numeric(mat_data[i,18])
    }
}

part_I.df = data.frame(x = ve_s[1:slice_cnt],y=ve_inf[1:slice_cnt],z=deaths[1:slice_cnt])

part_J.df = data.frame(x = ve_s[1:slice_cnt],y=ve_inf[1:slice_cnt],z=cases[1:slice_cnt])

slice_cnt=0


for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,5])==0.9 )
    {
	slice_cnt=slice_cnt+1
	ve_s[slice_cnt]<-as.numeric(mat_data[i,4])
	#ve_p[slice_cnt]<-as.numeric(mat_data[i,5])
	ve_inf[slice_cnt]<-as.numeric(mat_data[i,6])
	max_daily_cases[slice_cnt]<-as.numeric(mat_data[i,11])
	if (max_daily_cases[slice_cnt] > max_cases) max_cases=max_daily_cases[slice_cnt]
	cases[slice_cnt]<-as.numeric(mat_data[i,9])
	if (cases[slice_cnt] > max_cases) max_cases=cases[slice_cnt]
	if (cases[slice_cnt] < min_cases) min_cases=cases[slice_cnt]
	max_daily_deaths[slice_cnt]<-as.numeric(mat_data[i,17])
	if (max_daily_deaths[slice_cnt] > max_deaths) max_deaths=max_daily_deaths[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,15])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,20])
	avg_SD[slice_cnt]<-as.numeric(mat_data[i,18])
    }
}

part_K.df = data.frame(x = ve_s[1:slice_cnt],y=ve_inf[1:slice_cnt],z=deaths[1:slice_cnt])

part_L.df = data.frame(x = ve_s[1:slice_cnt],y=ve_inf[1:slice_cnt],z=cases[1:slice_cnt])

slice_cnt=0


for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,4])==0.1 )
    {
	slice_cnt=slice_cnt+1
	#ve_s[slice_cnt]<-as.numeric(mat_data[i,4])
	ve_p[slice_cnt]<-as.numeric(mat_data[i,5])
	ve_inf[slice_cnt]<-as.numeric(mat_data[i,6])
	max_daily_cases[slice_cnt]<-as.numeric(mat_data[i,11])
	if (max_daily_cases[slice_cnt] > max_cases) max_cases=max_daily_cases[slice_cnt]
	cases[slice_cnt]<-as.numeric(mat_data[i,9])
	if (cases[slice_cnt] > max_cases) max_cases=cases[slice_cnt]
	if (cases[slice_cnt] < min_cases) min_cases=cases[slice_cnt]
	max_daily_deaths[slice_cnt]<-as.numeric(mat_data[i,17])
	if (max_daily_deaths[slice_cnt] > max_deaths) max_deaths=max_daily_deaths[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,15])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,20])
	avg_SD[slice_cnt]<-as.numeric(mat_data[i,18])
    }
}

part_M.df = data.frame(x = ve_p[1:slice_cnt],y=ve_inf[1:slice_cnt],z=deaths[1:slice_cnt])

part_N.df = data.frame(x = ve_p[1:slice_cnt],y=ve_inf[1:slice_cnt],z=cases[1:slice_cnt])


slice_cnt=0


for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,4])==0.5 )
    {
	slice_cnt=slice_cnt+1
	#ve_s[slice_cnt]<-as.numeric(mat_data[i,4])
	ve_p[slice_cnt]<-as.numeric(mat_data[i,5])
	ve_inf[slice_cnt]<-as.numeric(mat_data[i,6])
	max_daily_cases[slice_cnt]<-as.numeric(mat_data[i,11])
	if (max_daily_cases[slice_cnt] > max_cases) max_cases=max_daily_cases[slice_cnt]
	cases[slice_cnt]<-as.numeric(mat_data[i,9])
	if (cases[slice_cnt] > max_cases) max_cases=cases[slice_cnt]
	if (cases[slice_cnt] < min_cases) min_cases=cases[slice_cnt]
	max_daily_deaths[slice_cnt]<-as.numeric(mat_data[i,17])
	if (max_daily_deaths[slice_cnt] > max_deaths) max_deaths=max_daily_deaths[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,15])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,20])
	avg_SD[slice_cnt]<-as.numeric(mat_data[i,18])
    }
}

part_O.df = data.frame(x = ve_p[1:slice_cnt],y=ve_inf[1:slice_cnt],z=deaths[1:slice_cnt])

part_P.df = data.frame(x = ve_p[1:slice_cnt],y=ve_inf[1:slice_cnt],z=cases[1:slice_cnt])

slice_cnt=0


for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,4])==0.9 )
    {
	slice_cnt=slice_cnt+1
	#ve_s[slice_cnt]<-as.numeric(mat_data[i,4])
	ve_p[slice_cnt]<-as.numeric(mat_data[i,5])
	ve_inf[slice_cnt]<-as.numeric(mat_data[i,6])
	max_daily_cases[slice_cnt]<-as.numeric(mat_data[i,11])
	if (max_daily_cases[slice_cnt] > max_cases) max_cases=max_daily_cases[slice_cnt]
	cases[slice_cnt]<-as.numeric(mat_data[i,9])
	if (cases[slice_cnt] > max_cases) max_cases=cases[slice_cnt]
	if (cases[slice_cnt] < min_cases) min_cases=cases[slice_cnt]
	max_daily_deaths[slice_cnt]<-as.numeric(mat_data[i,17])
	if (max_daily_deaths[slice_cnt] > max_deaths) max_deaths=max_daily_deaths[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,15])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,20])
	avg_SD[slice_cnt]<-as.numeric(mat_data[i,18])
    }
}

part_Q.df = data.frame(x = ve_p[1:slice_cnt],y=ve_inf[1:slice_cnt],z=deaths[1:slice_cnt])

part_R.df = data.frame(x = ve_p[1:slice_cnt],y=ve_inf[1:slice_cnt],z=cases[1:slice_cnt])

levelplot(z ~ x * y, part_A.df, xlab = "VEsusc",ylab = "VEsymp",main = paste("Post-Vaccine Deaths (",start_deaths,"at start)\nVEinf=10%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_deaths,to=max_deaths,length=col.divs))
levelplot(z ~ x * y, part_C.df, xlab = "VEsusc",ylab = "VEsymp",main = paste("Post-Vaccine Deaths (",start_deaths,"at start)\nVEinf=50%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_deaths,to=max_deaths,length=col.divs))
levelplot(z ~ x * y, part_E.df, xlab = "VEsusc",ylab = "VEsymp",main = paste("Post-Vaccine Deaths (",start_deaths,"at start)\nVEinf=90%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_deaths,to=max_deaths,length=col.divs))
levelplot(z ~ x * y, part_B.df, xlab = "VEsusc",ylab = "VEsymp",main = paste("Post-Vaccine Cases (",start_cases,"at start)\nVEinf=10%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_cases,to=max_cases,length=col.divs))
levelplot(z ~ x * y, part_D.df, xlab = "VEsusc",ylab = "VEsymp",main = paste("Post-Vaccine Cases (",start_cases,"at start)\nVEinf=50%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_cases,to=max_cases,length=col.divs))
levelplot(z ~ x * y, part_F.df, xlab = "VEsusc",ylab = "VEsymp",main = paste("Post-Vaccine Cases (",start_cases,"at start)\nVEinf=90%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_cases,to=max_cases,length=col.divs))
levelplot(z ~ x * y, part_G.df, xlab = "VEsusc",ylab = "VEinf",main = paste("Post-Vaccine Deaths (",start_deaths,"at start)\nVEsymp=10%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_deaths,to=max_deaths,length=col.divs))
levelplot(z ~ x * y, part_I.df, xlab = "VEsusc",ylab = "VEinf",main = paste("Post-Vaccine Deaths (",start_deaths,"at start)\nVEsymp=50%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_deaths,to=max_deaths,length=col.divs))
levelplot(z ~ x * y, part_K.df, xlab = "VEsusc",ylab = "VEinf",main = paste("Post-Vaccine Deaths (",start_deaths,"at start)\nVEsymp=90%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_deaths,to=max_deaths,length=col.divs))
levelplot(z ~ x * y, part_H.df, xlab = "VEsusc",ylab = "VEinf",main = paste("Post-Vaccine Cases (",start_cases,"at start)\nVEsymp=10%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_cases,to=max_cases,length=col.divs))
levelplot(z ~ x * y, part_J.df, xlab = "VEsusc",ylab = "VEinf",main = paste("Post-Vaccine Cases (",start_cases,"at start)\nVEsymp=50%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_cases,to=max_cases,length=col.divs))
levelplot(z ~ x * y, part_L.df, xlab = "VEsusc",ylab = "VEinf",main = paste("Post-Vaccine Cases (",start_cases,"at start)\nVEsymp=90%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_cases,to=max_cases,length=col.divs))
levelplot(z ~ x * y, part_M.df, xlab = "VEsymp",ylab = "VEinf",main = paste("Post-Vaccine Deaths (",start_deaths,"at start)\nVEsusc=10%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_deaths,to=max_deaths,length=col.divs))
levelplot(z ~ x * y, part_O.df, xlab = "VEsymp",ylab = "VEinf",main = paste("Post-Vaccine Deaths (",start_deaths,"at start)\nVEsusc=50%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_deaths,to=max_deaths,length=col.divs))
levelplot(z ~ x * y, part_Q.df, xlab = "VEsymp",ylab = "VEinf",main = paste("Post-Vaccine Deaths (",start_deaths,"at start)\nVEsusc=90%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_deaths,to=max_deaths,length=col.divs))
levelplot(z ~ x * y, part_N.df, xlab = "VEsymp",ylab = "VEinf",main = paste("Post-Vaccine Cases (",start_cases,"at start)\nVEsusc=10%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_cases,to=max_cases,length=col.divs))
levelplot(z ~ x * y, part_P.df, xlab = "VEsymp",ylab = "VEinf",main = paste("Post-Vaccine Cases (",start_cases,"at start)\nVEsusc=50%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_cases,to=max_cases,length=col.divs))
levelplot(z ~ x * y, part_R.df, xlab = "VEsymp",ylab = "VEinf",main = paste("Post-Vaccine Cases (",start_cases,"at start)\nVEsusc=90%"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      xlim=c(0.1,0.9),ylim=c(0.1,0.9),at=seq(from=min_cases,to=max_cases,length=col.divs))

dev.off()
warnings()
