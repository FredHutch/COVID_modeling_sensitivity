
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
data <- read.csv("basic_sens_final_v2.csv", comment.char="#", header=T)
#scenario,min_sd,ve_s,ve_p,ve_i,vac_rate,new_strain_fact,cases since vax,%case reduct,max daily cases,hosp since vax,%hosp reduct,max current hosp,deaths since vax,%death reduct,max daily deaths,avg SD,avg child SD,days at max SD,%reduct days at max,max Reff,min Reff, avg Reff,tot cases,tot hosp,tot deaths,loosen,tighten,max_sd,cases since vax yr2,%case reduct yr2,max daily cases yr2,hosp since vax yr2,%hosp reduct yr2,max current hosp yr2,deaths since vax yr2,%death reduct yr2,max daily deaths yr2,avg SD yr2,avg child SD yr2,days at max SD yr2,%reduct days at max yr2,max Reff yr2,min Reff yr2, avg Reff yr2,tot cases yr2,tot hosp yr2,tot deaths yr2,tot_infs

rnames <- data[1,]  
mat_data <- data.matrix(data[,1:ncol(data)])
colnames(mat_data)<- rnames   
library(latticeExtra)
col.l <- colorRampPalette(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'))
col.divs<-20
ves_idx=3
vep_idx=4
vei_idx=5
rate_idx=6
loose_idx=27
tight_idx=28
tot_inf_idx=49
tot_death_idx=26
days_max_idx=19

#inputs of interest
ve_s<-vector(length=nrow(mat_data))
ve_p<-vector(length=nrow(mat_data))
ve_inf<-vector(length=nrow(mat_data))
trig_max<-vector(length=nrow(mat_data))
vac_rate<-vector(length=nrow(mat_data))

#outputs of interest
infs<-vector(length=nrow(mat_data))
deaths<-vector(length=nrow(mat_data))
days_at_max_SD<-vector(length=nrow(mat_data))

#pdf("DR_paper_fig4.pdf",width=8,height=8)
pdf("HEATMAP_fig3.pdf")
par.settings=list(axis.text=list(fontfamily="sans",font=10,fontface="bold"),
	xlab.text=list(fontfamily="sans",font=14,fontface="bold"),
        ylab.text=list(fontfamily="sans",font=14,fontface="bold"),
        main.text=list(fontfamily="times",font=14,fontface="bold"))
par(mfrow = c(3,3))

slice_cnt=0
max_infs=0
max_deaths=0
max_days=0
min_infs=1e6
min_deaths=10000
min_days=10000

for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,loose_idx])==20 && (as.numeric(mat_data[i,vei_idx])==0.1) && as.numeric(mat_data[i,ves_idx])==0.9 && 
	as.numeric(mat_data[i,vep_idx])==0.1 && as.numeric(mat_data[i,tight_idx])<=650 )
    {
	slice_cnt=slice_cnt+1
	vac_rate[slice_cnt]<-as.numeric(mat_data[i,rate_idx])
	trig_max[slice_cnt]<-as.numeric(mat_data[i,tight_idx])
	infs[slice_cnt]<-as.numeric(mat_data[i,tot_inf_idx])
	if (infs[slice_cnt] > max_infs) max_infs=infs[slice_cnt]
	if (infs[slice_cnt] < min_infs) min_infs=infs[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,tot_death_idx])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,days_max_idx])
	if (days_at_max_SD[slice_cnt] > max_days) max_days=days_at_max_SD[slice_cnt]
	if (days_at_max_SD[slice_cnt] < min_days) min_days=days_at_max_SD[slice_cnt]
    }
}
print(slice_cnt)
print(paste("Min infs=",min_infs))
print(paste("Maximum infs=",max_infs))

print(paste("Min deaths=",min_deaths))
print(paste("Maximum deaths=",max_deaths))

print(paste("Min days=",min_days))
print(paste("Maximum days=",max_days))

part_A.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=infs[1:slice_cnt])
part_B.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=deaths[1:slice_cnt])
part_C.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=days_at_max_SD[1:slice_cnt])

slice_cnt=0

for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,loose_idx])==20 && (as.numeric(mat_data[i,vei_idx])==0 || as.numeric(mat_data[i,vei_idx])==0.1)  && as.numeric(mat_data[i,ves_idx])==0.1 && 
	as.numeric(mat_data[i,vep_idx])==0.9 && as.numeric(mat_data[i,tight_idx])<=650 )
    {
	slice_cnt=slice_cnt+1
	vac_rate[slice_cnt]<-as.numeric(mat_data[i,rate_idx])
	trig_max[slice_cnt]<-as.numeric(mat_data[i,tight_idx])
	infs[slice_cnt]<-as.numeric(mat_data[i,tot_inf_idx])
	if (infs[slice_cnt] > max_infs) max_infs=infs[slice_cnt]
	if (infs[slice_cnt] < min_infs) min_infs=infs[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,tot_death_idx])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,days_max_idx])
	if (days_at_max_SD[slice_cnt] > max_days) max_days=days_at_max_SD[slice_cnt]
	if (days_at_max_SD[slice_cnt] < min_days) min_days=days_at_max_SD[slice_cnt]
    }
}
print(slice_cnt)
print(paste("Min infs=",min_infs))
print(paste("Maximum infs=",max_infs))

print(paste("Min deaths=",min_deaths))
print(paste("Maximum deaths=",max_deaths))

print(paste("Min days=",min_days))
print(paste("Maximum days=",max_days))

part_D.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=infs[1:slice_cnt])
part_E.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=deaths[1:slice_cnt])
part_F.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=days_at_max_SD[1:slice_cnt])

slice_cnt=0

for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,loose_idx])==20 && (as.numeric(mat_data[i,vei_idx])==0 || as.numeric(mat_data[i,vei_idx])==0.1)  && as.numeric(mat_data[i,ves_idx])==0.5 && 
	as.numeric(mat_data[i,vep_idx])==0.1 && as.numeric(mat_data[i,tight_idx])<=650 )
    {
	slice_cnt=slice_cnt+1
	vac_rate[slice_cnt]<-as.numeric(mat_data[i,rate_idx])
	trig_max[slice_cnt]<-as.numeric(mat_data[i,tight_idx])
	infs[slice_cnt]<-as.numeric(mat_data[i,tot_inf_idx])
	if (infs[slice_cnt] > max_infs) max_infs=infs[slice_cnt]
	if (infs[slice_cnt] < min_infs) min_infs=infs[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,tot_death_idx])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,days_max_idx])
	if (days_at_max_SD[slice_cnt] > max_days) max_days=days_at_max_SD[slice_cnt]
	if (days_at_max_SD[slice_cnt] < min_days) min_days=days_at_max_SD[slice_cnt]
    }
}
print(slice_cnt)
print(paste("Min infs=",min_infs))
print(paste("Maximum infs=",max_infs))

print(paste("Min deaths=",min_deaths))
print(paste("Maximum deaths=",max_deaths))

print(paste("Min days=",min_days))
print(paste("Maximum days=",max_days))

part_G.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=infs[1:slice_cnt])
part_H.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=deaths[1:slice_cnt])
part_I.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=days_at_max_SD[1:slice_cnt])

slice_cnt=0

for (i in 3:nrow(mat_data))
{
    if (as.numeric(mat_data[i,loose_idx])==20 && (as.numeric(mat_data[i,vei_idx])==0 || as.numeric(mat_data[i,vei_idx])==0.1)  && as.numeric(mat_data[i,ves_idx])==0.1 && 
	as.numeric(mat_data[i,vep_idx])==0.5 && as.numeric(mat_data[i,tight_idx])<=650 )
    {
	slice_cnt=slice_cnt+1
	vac_rate[slice_cnt]<-as.numeric(mat_data[i,rate_idx])
	trig_max[slice_cnt]<-as.numeric(mat_data[i,tight_idx])
	infs[slice_cnt]<-as.numeric(mat_data[i,tot_inf_idx])
	if (infs[slice_cnt] > max_infs) max_infs=infs[slice_cnt]
	if (infs[slice_cnt] < min_infs) min_infs=infs[slice_cnt]
	deaths[slice_cnt]<-as.numeric(mat_data[i,tot_death_idx])
	if (deaths[slice_cnt] > max_deaths) max_deaths=deaths[slice_cnt]
	if (deaths[slice_cnt] < min_deaths) min_deaths=deaths[slice_cnt]
	days_at_max_SD[slice_cnt]<-as.numeric(mat_data[i,days_max_idx])
	if (days_at_max_SD[slice_cnt] > max_days) max_days=days_at_max_SD[slice_cnt]
	if (days_at_max_SD[slice_cnt] < min_days) min_days=days_at_max_SD[slice_cnt]
    }
}
print(slice_cnt)
print(paste("Min infs=",min_infs))
print(paste("Maximum infs=",max_infs))

print(paste("Min deaths=",min_deaths))
print(paste("Maximum deaths=",max_deaths))

print(paste("Min days=",min_days))
print(paste("Maximum days=",max_days))

part_J.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=infs[1:slice_cnt])
part_K.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=deaths[1:slice_cnt])
part_L.df = data.frame(x = vac_rate[1:slice_cnt],y=trig_max[1:slice_cnt],z=days_at_max_SD[1:slice_cnt])

levelplot(z ~ x * y, part_A.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Total Infections\n(VEsusc=90%,VEsym=10%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_infs,to=max_infs,length=col.divs),cex=2)

levelplot(z ~ x * y, part_B.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Total Deaths\n(VEsusc=90%,VEsym=10%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_deaths,to=max_deaths,length=col.divs),cex=2)

levelplot(z ~ x * y, part_C.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Days at Social Distancing Maximum\n(VEsusc=90%,VEsym=10%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_days,to=max_days,length=col.divs),cex=2)

levelplot(z ~ x * y, part_D.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Total Infections\n(VEsusc=10%,VEsym=90%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_infs,to=max_infs,length=col.divs),cex=2)

levelplot(z ~ x * y, part_E.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Total Deaths\n(VEsusc=10%,VEsym=90%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_deaths,to=max_deaths,length=col.divs),cex=2)

levelplot(z ~ x * y, part_F.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Days at Social Distancing Maximum\n(VEsusc=10%,VEsym=90%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_days,to=max_days,length=col.divs),cex=2)

levelplot(z ~ x * y, part_G.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Total Infections\n(VEsusc=50%,VEsym=10%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_infs,to=max_infs,length=col.divs),cex=2)

levelplot(z ~ x * y, part_H.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Total Deaths\n(VEsusc=50%,VEsym=10%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_deaths,to=max_deaths,length=col.divs),cex=2)

levelplot(z ~ x * y, part_I.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Days at Social Distancing Maximum\n(VEsusc=50%,VEsym=10%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_days,to=max_days,length=col.divs),cex=2)

levelplot(z ~ x * y, part_J.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Total Infections\n(VEsusc=10%,VEsym=50%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_infs,to=max_infs,length=col.divs),cex=2)

levelplot(z ~ x * y, part_K.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Total Deaths\n(VEsusc=10%,VEsym=50%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_deaths,to=max_deaths,length=col.divs),cex=2)

levelplot(z ~ x * y, part_L.df, xlab = "Vaccination Rate",ylab = "Lockdown Threshold",main = paste("Days at Social Distancing Maximum\n(VEsusc=10%,VEsym=50%,VEinf=10%)"),
	scales=list(tck=c(1,0)),cuts=20, contour=TRUE, col.regions=col.l,panel=panel.2dsmoother, method="loess",args=list(degree=2,span=1),
      ylim=c(200,650),xlim=c(2000,11000),at=seq(from=min_days,to=max_days,length=col.divs),cex=2)

dev.off()
warnings()
