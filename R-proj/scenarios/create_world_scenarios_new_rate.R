int_param_names = c("dynamic_sd_min","dynamic_sd_min_snrs","dynamic_sd_limit","dynamic_sd_hyster", "vac_final_rate","vac_on")

#27 combos (3 SDmin x 3 TRIGmax  x 3 VRates)
interventions = matrix(c( 
	    0.1,0.3, ((trig_min + 200)/2) * the_pop / 100000,((200 - trig_min)/2) * the_pop / 100000, 5000, 1,
	    0.2,0.4, ((trig_min + 200)/2) * the_pop / 100000,((200 - trig_min)/2) * the_pop / 100000, 5000, 1,
	    0.3,0.5, ((trig_min + 200)/2) * the_pop / 100000,((200 - trig_min)/2) * the_pop / 100000, 5000, 1,
	    0.1,0.3, ((trig_min + 350)/2) * the_pop / 100000,((350 - trig_min)/2) * the_pop / 100000, 5000, 1,
	    0.2,0.4, ((trig_min + 350)/2) * the_pop / 100000,((350 - trig_min)/2) * the_pop / 100000, 5000, 1,
	    0.3,0.5, ((trig_min + 350)/2) * the_pop / 100000,((350 - trig_min)/2) * the_pop / 100000, 5000, 1,
	    0.1,0.3, ((trig_min + 500)/2) * the_pop / 100000,((500 - trig_min)/2) * the_pop / 100000, 5000, 1,
	    0.2,0.4, ((trig_min + 500)/2) * the_pop / 100000,((500 - trig_min)/2) * the_pop / 100000, 5000, 1,
	    0.3,0.5, ((trig_min + 500)/2) * the_pop / 100000,((500 - trig_min)/2) * the_pop / 100000, 5000, 1,
	    0.1,0.3, ((trig_min + 200)/2) * the_pop / 100000,((200 - trig_min)/2) * the_pop / 100000, 10000, 1,
	    0.2,0.4, ((trig_min + 200)/2) * the_pop / 100000,((200 - trig_min)/2) * the_pop / 100000, 10000, 1,
	    0.3,0.5, ((trig_min + 200)/2) * the_pop / 100000,((200 - trig_min)/2) * the_pop / 100000, 10000, 1,
	    0.1,0.3, ((trig_min + 350)/2) * the_pop / 100000,((350 - trig_min)/2) * the_pop / 100000, 10000, 1,
	    0.2,0.4, ((trig_min + 350)/2) * the_pop / 100000,((350 - trig_min)/2) * the_pop / 100000, 10000, 1,
	    0.3,0.5, ((trig_min + 350)/2) * the_pop / 100000,((350 - trig_min)/2) * the_pop / 100000, 10000, 1,
	    0.1,0.3, ((trig_min + 500)/2) * the_pop / 100000,((500 - trig_min)/2) * the_pop / 100000, 10000, 1,
	    0.2,0.4, ((trig_min + 500)/2) * the_pop / 100000,((500 - trig_min)/2) * the_pop / 100000, 10000, 1,
	    0.3,0.5, ((trig_min + 500)/2) * the_pop / 100000,((500 - trig_min)/2) * the_pop / 100000, 10000, 1,
	    0.1,0.3, ((trig_min + 200)/2) * the_pop / 100000,((200 - trig_min)/2) * the_pop / 100000, 15000, 1,
	    0.2,0.4, ((trig_min + 200)/2) * the_pop / 100000,((200 - trig_min)/2) * the_pop / 100000, 15000, 1,
	    0.3,0.5, ((trig_min + 200)/2) * the_pop / 100000,((200 - trig_min)/2) * the_pop / 100000, 15000, 1,
	    0.1,0.3, ((trig_min + 350)/2) * the_pop / 100000,((350 - trig_min)/2) * the_pop / 100000, 15000, 1,
	    0.2,0.4, ((trig_min + 350)/2) * the_pop / 100000,((350 - trig_min)/2) * the_pop / 100000, 15000, 1,
	    0.3,0.5, ((trig_min + 350)/2) * the_pop / 100000,((350 - trig_min)/2) * the_pop / 100000, 15000, 1,
	    0.1,0.3, ((trig_min + 500)/2) * the_pop / 100000,((500 - trig_min)/2) * the_pop / 100000, 15000, 1,
	    0.2,0.4, ((trig_min + 500)/2) * the_pop / 100000,((500 - trig_min)/2) * the_pop / 100000, 15000, 1,
	    0.3,0.5, ((trig_min + 500)/2) * the_pop / 100000,((500 - trig_min)/2) * the_pop / 100000, 15000, 1),
                       byrow = TRUE, nrow = 27)

row.names(interventions) = c(
			"SDmin=0.1,Cmax=200,Vax Rate=5000/day",
			"SDmin=0.2,Cmax=200,Vax Rate=5000/day",
			"SDmin=0.3,Cmax=200,Vax Rate=5000/day",
			"SDmin=0.1,Cmax=350,Vax Rate=5000/day",
			"SDmin=0.2,Cmax=350,Vax Rate=5000/day",
			"SDmin=0.3,Cmax=350,Vax Rate=5000/day",
			"SDmin=0.1,Cmax=500,Vax Rate=5000/day",
			"SDmin=0.2,Cmax=500,Vax Rate=5000/day",
			"SDmin=0.3,Cmax=500,Vax Rate=5000/day",
			"SDmin=0.1,Cmax=200,Vax Rate=10000/day",
			"SDmin=0.2,Cmax=200,Vax Rate=10000/day",
			"SDmin=0.3,Cmax=200,Vax Rate=10000/day",
			"SDmin=0.1,Cmax=350,Vax Rate=10000/day",
			"SDmin=0.2,Cmax=350,Vax Rate=10000/day",
			"SDmin=0.3,Cmax=350,Vax Rate=10000/day",
			"SDmin=0.1,Cmax=500,Vax Rate=10000/day",
			"SDmin=0.2,Cmax=500,Vax Rate=10000/day",
			"SDmin=0.3,Cmax=500,Vax Rate=10000/day",
			"SDmin=0.1,Cmax=200,Vax Rate=15000/day",
			"SDmin=0.2,Cmax=200,Vax Rate=15000/day",
			"SDmin=0.3,Cmax=200,Vax Rate=15000/day",
			"SDmin=0.1,Cmax=350,Vax Rate=15000/day",
			"SDmin=0.2,Cmax=350,Vax Rate=15000/day",
			"SDmin=0.3,Cmax=350,Vax Rate=15000/day",
			"SDmin=0.1,Cmax=500,Vax Rate=15000/day",
			"SDmin=0.2,Cmax=500,Vax Rate=15000/day",
			"SDmin=0.3,Cmax=500,Vax Rate=15000/day")

colnames(interventions) = int_param_names
interventions_abbr = row.names(interventions)


