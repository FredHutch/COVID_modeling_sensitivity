# defaults and high.low for 6 parameters: SDmin, Cmin, Cmax, VRate, Coverage and Import Rate
int_param_names = c("dynamic_sd_min","dynamic_sd_min_snrs",
	"dynamic_sd_limit","dynamic_sd_hyster", 
	"vac_final_rate","vac_coverage")
	#"vac_final_rate","vac_coverage","new_strain_intros")

#729 combos (3 SDmin * 3 Cmin * 3 Cmax  * 3 VRates * 3 Coverages * 3 Import Rates)
#243 combos (3 SDmin * 3 Cmin * 3 Cmax  * 3 VRates * 3 Coverages)
interventions= matrix(NA, nrow = 243, ncol = length(int_param_names))
int_labels = vector(length=243)
i=1
for (SDmin in c(0.1,0.2,0.3))
{
    for (Cmin in c(50,100,200))
    {
	for (Cmax in c(200,350,500))
	{
	    for (Vrate in c(5000,10000,15000))
	    {
		for (Cover in c(0.7,0.8,0.9))
		{
		    #for (Imports in c(0.05,0.25,1.25))
		    #{
			SDlimit=((Cmin + Cmax)/2) * the_pop / 100000
			SDhyster=((Cmax - Cmin)/2) * the_pop / 100000
			interventions[i,1]=SDmin
			interventions[i,2]=SDmin+0.2
			interventions[i,3]=SDlimit
			interventions[i,4]=SDhyster
			interventions[i,5]=Vrate
			interventions[i,6]=Cover
		#	interventions[i,7]=Imports
			int_labels[i] = paste0("SDmin=",SDmin,",Cmin=",Cmin,
				",Cmax=",Cmax,",Vrate=",Vrate,",Cover=",Cover)
#				"Cmax=",Cmax,"Vrate=",Vrate,"Cover=",Cover,"Imports=",Imports)
			i=i+1
		#    }
		}
	    }
	}
    }
}
row.names(interventions) = int_labels

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)


