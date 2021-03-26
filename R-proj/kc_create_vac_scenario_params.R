# the calibrated parameter values and vaccination scenarios

##########################################################################################
# These "interventions" are really vaccination protocols

#NOTE: Multiple vaccination efficacy matrices are declared here for convenience 
#The file kc_create_scenario_multi_vac.R will use whichever appears last in this file

#Values for VE_S and Ve_p taken from the equation Veff = 1 - (1-VE_S)(1-Ve_p)
#Matrix columns are as defined in "int_param_names"
##########################################################################################

int_param_names = c("vac_eff_susc", "vac_eff_pi","vac_on")

#Matrix with options for various efficacies for Ve_susc and Ve_p
#a.	VES=10%, VEP=10%
#b.	VES=40%, VEP=10%
#c.	VES=70%, VEP=10%
#d.	VES=10%, VEP=40%
#e.	VES=40%, VEP=40%
#f.	VES=70%, VEP=40%
#g.	VES=10%, VEP=70%
#h.	VES=40%, VEP=70%
#i.	VES=70%, VEP=70%
#i.	VES=10%, VEP=90%
#i.	VES=90%, VEP=10%
#
interventions = matrix(c(0, 0, 0,
                         0.1, 0.1, 1,
                         0.4, 0.1, 1,
                         0.7, 0.1, 1,
                         0.1, 0.4, 1,
                         0.4, 0.4, 1,
                         0.7, 0.4, 1,
                         0.1, 0.7, 1,
                         0.4, 0.7, 1,
                         0.7, 0.7, 1,
                         0.1, 0.9, 1,
                         0.9, 0.1, 1),
                       byrow = TRUE, nrow = 12)

row.names(interventions) = c("No Vaccine", "10% VE_S,10% VE_P", "40% VE_S,10% VE_P","70% VE_S,10% VE_P", 
				"10% VE_S,40% VE_P", "40% VE_S,40% VE_P","70% VE_S,40% VE_P",
				"10% VE_S,70% VE_P", "40% VE_S,70% VE_P","70% VE_S,70% VE_P","10% VE_S,90% VE_P","90% VE_S,10% VE_P")

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)

int_rampup = 14				      # Time to achieve full intervention effect

# extend sims to one year post-vaccination start
end_day = 366 + yday("2021-12-31")


