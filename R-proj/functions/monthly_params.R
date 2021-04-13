#function to interpolate between monthly avg values (set mid month)
monthly_interpolation = function(time,parameters,val1,val2,val3,val4,val5,val6,val7,val8,val9,val10) {
    with(parameters, {
	# first calculate deltas and mid_month dates in model time
	day0_doy = first_case_doy - delta0offset
	mid_month_1 = DiagDate1 - day0_doy # mid-march
	mid_month_2 = DiagDate2 - day0_doy # mid-april
	mid_month_3 = DiagDate3 - day0_doy # mid-may
	mid_month_4 = DiagDate4 - day0_doy # mid-june
	mid_month_5 = DiagDate5 - day0_doy # mid-july
	mid_month_6 = DiagDate6 - day0_doy # mid-aug
	mid_month_7 = DiagDate7 - day0_doy # mid-sept
	mid_month_8 = DiagDate8 - day0_doy # mid-oct
	mid_month_9 = DiagDate9 - day0_doy # mid-nov
	mid_month_10 = DiagDate10 - day0_doy # mid-dec

  	local_calib_doy = parameters$calib_doy - day0_doy

	# set the monthly rates (note: hosp_rates and death_rates currently unused)
	#
	if (time < mid_month_1) { 
	    value = val1
	} else if (time < mid_month_2) { 
	    value = val1 + (time - mid_month_1) / (mid_month_2 - mid_month_1) * (val2 - val1) 
	} else if (time < mid_month_3) { 
	    value = val2 + (time - mid_month_2) / (mid_month_3 - mid_month_2) * (val3 - val2) 
	} else if (time < mid_month_4) { 
	    value = val3 + (time - mid_month_3) / (mid_month_4 - mid_month_3) * (val4 - val3) 
	} else if (time < mid_month_5) { 
	    value = val4 + (time - mid_month_4) / (mid_month_5 - mid_month_4) * (val5 - val4) 
	} else if (time < mid_month_6) { 
	    value = val5 + (time - mid_month_5) / (mid_month_6 - mid_month_5) * (val6 - val5) 
	} else if (time < mid_month_7) { 
	    value = val6 + (time - mid_month_6) / (mid_month_7 - mid_month_6) * (val7 - val6) 
	} else if (time < mid_month_8 && time < local_calib_doy) { 
	    value = val7 + (time - mid_month_7) / (mid_month_8 - mid_month_7) * (val8 - val7) 
	} else if (time < mid_month_9 && time < local_calib_doy) { 
	    value = val8 + (time - mid_month_8) / (mid_month_9 - mid_month_8) * (val9 - val8) 
	} else if (time < mid_month_10 && time < local_calib_doy) { 
	    value = val9 + (time - mid_month_9) / (mid_month_10 - mid_month_9) * (val10 - val9) 
	} else  { 
	    value = val10 # hold as final value 
	}
        return (value)
    })
}

#function to transition to a new monthly value and hold it to the end of the month
# this is used during calibration to keep the different intervals independent
# (i.e. uncalibrated future value doesn't effect current fit)
trans_to_monthly = function(time,parameters,val1,val2,val3,val4,val5,val6,val7,val8,val9) {
    with(parameters, {
	# first calculate deltas and mid_month dates in model time
	day0_doy = first_case_doy - delta0offset
	delta3 = delta3_doy - day0_doy # may 1st
	delta4 = delta4_doy - day0_doy # june 1st
	delta5 = delta5_doy - day0_doy # july 1st
	delta6 = delta6_doy - day0_doy # aug 1st
	delta7 = delta7_doy - day0_doy # sept 1st
	delta8 = delta8_doy - day0_doy # oct 1st
	delta9 = delta9_doy - day0_doy # nov 1st
	delta10 = delta10_doy - day0_doy # dec 1st

	mid_month_3 = DiagDate3 - day0_doy # mid-may
	mid_month_4 = DiagDate4 - day0_doy # mid-june
	mid_month_5 = DiagDate5 - day0_doy # mid-july
	mid_month_6 = DiagDate6 - day0_doy # mid-aug
	mid_month_7 = DiagDate7 - day0_doy # mid-sept
	mid_month_8 = DiagDate8 - day0_doy # mid-oct
	mid_month_9 = DiagDate9 - day0_doy # mid-nov
	mid_month_10 = DiagDate10 - day0_doy # mid-dec

        local_calib_doy = parameters$calib_doy - day0_doy

	value = if (time < delta3 ) { val1 }
		else if (time >= delta3 & time < mid_month_3) { val1 + (time - delta3) / (mid_month_3 - delta3) * (val2 - val1) }
		else if (time >= mid_month_3 & time < delta4) { val2 }
		else if (time >= delta4 & time < mid_month_4) { val2 + (time - delta4) / (mid_month_4 - delta4) * (val3 - val2) }
		else if (time >= mid_month_4 & time < delta5) { val3 }
		else if (time >= delta5 & time < mid_month_5) { val3 + (time - delta5) / (mid_month_5 - delta5) * (val4 - val3) }
		else if (time >= mid_month_5 & time < delta6) { val4 }
		else if (time >= delta6 & time < mid_month_6) { val4 + (time - delta6) / (mid_month_6 - delta6) * (val5 - val4) }
		else if (time >= mid_month_6 & time < delta7) { val5 }
		else if (time >= delta7 & time < mid_month_7) { val5 + (time - delta7) / (mid_month_7 - delta7) * (val6 - val5) }
		else if (time >= mid_month_7 & time < delta8) { val6 }
		else if (time >= delta8 & time < mid_month_8) { val6 + (time - delta8) / (mid_month_8 - delta8) * (val7 - val6) }
		else if (time >= mid_month_8 & time < delta9) { val7 }
		else if (time >= delta9 & time < mid_month_9 && time < local_calib_doy) { val7 + (time - delta9) / (mid_month_9 - delta9) * (val8 - val7) }
		else if (time >= mid_month_9 & time < delta10) { val8 }
		else if (time >= delta10 & time < mid_month_10 && time < local_calib_doy) { val8 + (time - delta10) / (mid_month_10 - delta10) * (val9 - val8) }
		else { val9 } # hold as final value 

        return (value)
    })
}

#function to return new monthly value of social distancing (held to the end of the month)
#
sd_monthly = function(time,parameters,sd,sd2,sd3,sd4,sd5,sd6,sd7,sd8,sd9,sd_adj) {
    with(parameters, {
	# first calculate deltas and mid_month dates in model time
	day0_doy = first_case_doy - delta0offset
	delta1 = delta1_doy - day0_doy
	delta2 = delta2_doy - day0_doy
	delta3 = delta3_doy - day0_doy # may 1st
	delta4 = delta4_doy - day0_doy # june 1st
	delta5 = delta5_doy - day0_doy # july 1st
	delta6 = delta6_doy - day0_doy # aug 1st
	delta7 = delta7_doy - day0_doy # sept 1st
	delta8 = delta8_doy - day0_doy # oct 1st
	delta9 = delta9_doy - day0_doy # nov 1st
	delta10 = delta10_doy - day0_doy # dec 1st
	delta11 = delta11_doy - day0_doy # jan 1st

	mid_month_3 = delta3 + sd_trans # sometime mid-may
	mid_month_4 = delta4 + sd_trans # sometime mid-june
	mid_month_5 = delta5 + sd_trans # sometime mid-july
	mid_month_6 = delta6 + sd_trans # sometime mid-aug
	mid_month_7 = delta7 + sd_trans # sometime mid-sept
	mid_month_8 = delta8 + sd_trans # sometime mid-oct
	mid_month_9 = delta9 + sd_trans # sometime mid-nov
	mid_month_10 = delta10 + sd_trans # sometime mid-dec
	mid_month_11 = delta11 + sd_trans # sometime mid-jan

        local_calib_doy = parameters$calib_doy - day0_doy

	value = if (time < delta1) { c(0,0,0,0) }
            else if (time >= delta1 & time < delta2) { (time - delta1) / (delta2 - delta1) * sd }
            else if (time >= delta2 & time < delta3) { sd }
            else if (time >= delta3 & time < mid_month_3) { sd + (time - delta3) / (mid_month_3 - delta3) * (sd2 - sd) }
            else if (time >= mid_month_3 & time < delta4) { sd2 }
            else if (time >= delta4 & time < mid_month_4) { sd2 + (time - delta4) / (mid_month_4 - delta4) * (sd3 - sd2) }
            else if (time >= mid_month_4 & time < delta5) { sd3 }
            else if (time >= delta5 & time < mid_month_5) { sd3 + (time - delta5) / (mid_month_5 - delta5) * (sd4 - sd3) }
            else if (time >= mid_month_5 & time < delta6) { sd4 }
            else if (time >= delta6 & time < mid_month_6) { sd4 + (time - delta6) / (mid_month_6 - delta6) * (sd5 - sd4) }
            else if (time >= mid_month_6 & time < delta7) { sd5 }
            else if (time >= delta7 & time < mid_month_7) { sd5 + (time - delta7) / (mid_month_7 - delta7) * (sd6 - sd5) }
            else if (time >= mid_month_7 & time < delta8) { sd6 }
            else if (time >= delta8 & time < mid_month_8) { sd6 + (time - delta8) / (mid_month_8 - delta8) * (sd7 - sd6) }
            else if (time >= mid_month_8 & time < delta9 && time < local_calib_doy) { sd7 }
            else if (time >= delta9 & time < mid_month_9 && time < local_calib_doy) { sd7 + (time - delta9) / (mid_month_9 - delta9) * (sd8 - sd7) }
            else if (time >= mid_month_9 & time < delta10 && time < local_calib_doy) { sd8 }
            else if (time >= delta10 & time < mid_month_10 && time < local_calib_doy) { sd8 + (time - delta10) / (mid_month_10 - delta10) * (sd9 - sd8) }
            else if (time >= mid_month_10 & time < delta11 && time < local_calib_doy) { sd9 }
            else if (time >= delta11 & time < mid_month_11 && time < local_calib_doy) { sd9 + (time - delta11) / (mid_month_11 - delta11) * (sd_adj - sd9) }
            else  { sd_adj } # change to dynamic sd values 
        return (value)
    })
}

calc_h_i = function(time, parameters) {
  with(parameters, { 
    h1_i = c(h1_1, h1_2, h1_3, h1_4)      # hospitalization rates during lockdown
    h2_i = c(h2_1, h2_2, h2_3, h2_4)      # hospitalization rates during May
    h3_i = c(h3_1, h3_2, h3_3, h3_4)      # hospitalization rates during June
    h4_i = c(h4_1, h4_2, h4_3, h4_4)      # hospitalization rates during July
    h5_i = c(h5_1, h5_2, h5_3, h5_4)      # hospitalization rates during Aug
    h6_i = c(h6_1, h6_2, h6_3, h6_4)      # hospitalization rates during Sept
    h7_i = c(h7_1, h7_2, h7_3, h7_4)      # hospitalization rates during Oct
    h8_i = c(h8_1, h8_2, h8_3, h8_4)      # hospitalization rates during Nov
    h9_i = c(h9_1, h9_2, h9_3, h9_4)      # hospitalization rates during Dec

    # set hospitalization rates based on time period (fitted values)
    h_i = trans_to_monthly(time,parameters,h1_i,h2_i,h3_i,h4_i,h5_i,h6_i,h7_i,h8_i,h9_i)

    return (h_i)
  })
}

calc_cfr = function(time, parameters) {
  with(parameters, { 
    cfr1_i = c(cfr1_1, cfr1_2, cfr1_3, cfr1_4)      # CFR during lockdown
    cfr2_i = c(cfr2_1, cfr2_2, cfr2_3, cfr2_4)      # CFR during May
    cfr3_i = c(cfr3_1, cfr3_2, cfr3_3, cfr3_4)      # CFR during June
    cfr4_i = c(cfr4_1, cfr4_2, cfr4_3, cfr4_4)      # CFR during July
    cfr5_i = c(cfr5_1, cfr5_2, cfr5_3, cfr5_4)      # CFR during Aug
    cfr6_i = c(cfr6_1, cfr6_2, cfr6_3, cfr6_4)      # CFR during Sept
    cfr7_i = c(cfr7_1, cfr7_2, cfr7_3, cfr7_4)      # CFR during Oct
    cfr8_i = c(cfr8_1, cfr8_2, cfr8_3, cfr8_4)      # CFR during Nov
    cfr9_i = c(cfr9_1, cfr9_2, cfr9_3, cfr9_4)      # CFR during Dec

    # set CFR based on time period (fitted values)
    cfr = trans_to_monthly(time,parameters,cfr1_i,cfr2_i,cfr3_i,cfr4_i,cfr5_i,cfr6_i,cfr7_i,cfr8_i,cfr9_i)

    return (cfr)
  })
}

calc_rho_S = function(time, parameters) {
  with(parameters, { 
    rho_S1_i = c(rho_S1_1, rho_S1_2, rho_S1_3, rho_S1_4)      # rho_S during lockdown
    rho_S2_i = c(rho_S2_1, rho_S2_2, rho_S2_3, rho_S2_4)      # rho_S during May
    rho_S3_i = c(rho_S3_1, rho_S3_2, rho_S3_3, rho_S3_4)      # rho_S during June
    rho_S4_i = c(rho_S4_1, rho_S4_2, rho_S4_3, rho_S4_4)      # rho_S during July
    rho_S5_i = c(rho_S5_1, rho_S5_2, rho_S5_3, rho_S5_4)      # rho_S during Aug
    rho_S6_i = c(rho_S6_1, rho_S6_2, rho_S6_3, rho_S6_4)      # rho_S during Sept
    rho_S7_i = c(rho_S7_1, rho_S7_2, rho_S7_3, rho_S7_4)      # rho_S during Oct
    rho_S8_i = c(rho_S8_1, rho_S8_2, rho_S8_3, rho_S8_4)      # rho_S during Nov
    rho_S9_i = c(rho_S9_1, rho_S9_2, rho_S9_3, rho_S9_4)      # rho_S during Dec
    
    rho_S = trans_to_monthly(time,parameters,rho_S1_i,rho_S2_i,rho_S3_i,rho_S4_i,rho_S5_i,rho_S6_i,
	rho_S7_i,rho_S8_i,rho_S9_i)

    return (rho_S)
  })
}

calc_diag_rate = function(time, state, parameters) {
par <- as.list(c(state, parameters))
  with(par, { 
    # non-vaccinated populations (s, vs and cumulatives shared between strains)
    S_i = matrix( state[S_idx], nrow = 4, ncol = 1)         # current and cumulative number of susceptible individuals
    VS_i = matrix( state[VS_idx], nrow = 4, ncol = 1)         # current and cumulative number of susceptible vacinees
    E1_i = matrix( state[E_idx], nrow = 4, ncol = 1)         # current number of exposed individuals(strain 1)
    A1_i = matrix( state[A_idx], nrow = 4, ncol = 1)         # Current number of Asymptomatic individuals(strain 1)
    P1_i = matrix( state[P_idx], nrow = 4, ncol = 1)         # Current number of Pre-symptomatic individuals(strain 1)
    I1_i = matrix( state[I_idx], nrow = 4, ncol = 1)         # Current number of Infected individuals(strain 1)
    E2_i = matrix( state[E_idx+strainwise_entries], nrow = 4, ncol = 1)         # same for strain 2
    A2_i = matrix( state[A_idx+strainwise_entries], nrow = 4, ncol = 1)         
    P2_i = matrix( state[P_idx+strainwise_entries], nrow = 4, ncol = 1)        
    I2_i = matrix( state[I_idx+strainwise_entries], nrow = 4, ncol = 1)       

    # Vaccinated populations
    VE1_i = matrix( state[VE_idx], nrow = 4, ncol = 1)         # Current number of Exposed vacinees
    VA1_i = matrix( state[VA_idx], nrow = 4, ncol = 1)         # Current number of Asymptomatic vacinees
    VP1_i = matrix( state[VP_idx], nrow = 4, ncol = 1)         # Current number of Pre-symptomatic vacinees
    VI1_i = matrix( state[VI_idx], nrow = 4, ncol = 1)         # Current number of Infected vacinees
    VE2_i = matrix( state[VE_idx+strainwise_entries], nrow = 4, ncol = 1)   
    VA2_i = matrix( state[VA_idx+strainwise_entries], nrow = 4, ncol = 1)   
    VP2_i = matrix( state[VP_idx+strainwise_entries], nrow = 4, ncol = 1)  
    VI2_i = matrix( state[VI_idx+strainwise_entries], nrow = 4, ncol = 1) 

    Tests = monthly_interpolation(time,parameters,Tests1,Tests2,Tests3,Tests4,Tests5,Tests6,Tests7,Tests8,Tests9,Tests10)

    #print(paste("time=",time,"Tests=(",Tests[1],",",Tests[2],",",Tests[3],",",Tests[4],")"))
    Testing_Pop = rho_A * (A1_i + P1_i + A2_i + P2_i + VA1_i + VP1_i + VA2_i + VP2_i) + 
		    rho_S * (S_i + E1_i + E2_i + VS_i + VE1_i + VE2_i) +  
			I1_i + I2_i + VI1_i + VI2_i
    #print(paste("time=",time,"Testing_Pop=(",Testing_Pop[1],",",Testing_Pop[2],",",Testing_Pop[3],",",Testing_Pop[4],")"))

    d_i = c(0,0,0,0)
    for (i in 1:4) {
	if (Testing_Pop[i]>0) {
	    d_i[i] = (Tests[i] / Testing_Pop[i])
	}
    }
    return (d_i)
  })
}

#data points taken from KC web-site for 1st dose rates, averaged over 7 days and then lagged by 2 weeks
#before counting vaccinee as being protected.
#
calc_vax_rate = function(time, parameters) {
  with(parameters, { 
	day0_doy = first_case_doy - delta0offset

	vac_segments=nrow(vac_schedule)

	if (vac_segments <= 1)
	    return(vac_final_rate)

	start_time<-vector(length=vac_segments-1)
	end_time<-vector(length=vac_segments-1)
	start_val<-vector(length=vac_segments-1)
	end_val<-vector(length=vac_segments-1)
	for (seg in 1:vac_segments-1)
	{
	    start_time[seg] = vac_schedule[seg,1] - day0_doy
	    start_val[seg] = vac_schedule[seg,2]
	    end_time[seg] = vac_schedule[(seg+1),1] - day0_doy
	    end_val[seg] = vac_schedule[(seg+1),2]
	}

	if (vac_segments == 0 || time < start_time[1])
	    vax_rate = 0
	else 
	{
	    for (seg in 1:(vac_segments-1))
	    {
	    #print(paste("time=",time,"start=",start_time[seg],"end=",end_time[seg]))
		if (time < start_time[seg])
		    return(0)
		else if (time >= start_time[seg] && time < end_time[seg])
		{
		    vax_daily_adj = (end_val[seg] - start_val[seg]) / (end_time[seg] - start_time[seg])
		    vax_rate = start_val[seg] + (time - start_time[seg]) * vax_daily_adj
		    return (vax_rate)
		}
	    }
	    if (time >= end_time[vac_segments-1] && time < end_time[vac_segments-1]+14) # 2 week transition to "final rate"
	    {
		vax_daily_adj = (vac_final_rate - end_val[vac_segments-1]) / 14
		vax_rate = end_val[vac_segments-1] + (time - end_time[vac_segments-1]) * vax_daily_adj
	    }
	    else
		vax_rate = vac_final_rate  
	}

        return (vax_rate)
  })
}
