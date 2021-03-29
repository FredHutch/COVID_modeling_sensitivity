# this is the definition of the model
#note: vaccine efficacies must be global (not in params) to work for 2nd strain

model_eq = function(time, state, parameters, Reff = F) {
  parameters$h_i = calc_h_i(time,parameters)
  parameters$cfr = calc_cfr(time,parameters)
  parameters$rho_S = calc_rho_S(time,parameters)
  parameters$vac_rate = calc_vax_rate(time,parameters)
  #print(paste("time=",time,"vax_rate=",parameters$vac_rate,"total_vax=",sum(state[Vtot_idx])))

  parameters$rho_A =parameters$rho_S
  parameters$d_i = calc_diag_rate(time, state, parameters)

  par <- as.list(c(state, parameters))
    with(par, { 

    vac_eff_pi <<- vac_eff_pi1
    vac_eff_susc <<- vac_eff_susc1
    vac_eff_inf <<- vac_eff_inf1

    inf_intros <<-c(0,0,0,0)

    # handle normal operation 1st
    if (Reff == F) 
    {
	dstate1 = model_eq_set(E_idx,time, state, parameters)

	# fill in key parameters for 2nd strain (or vaccine group)
	if (vac_mutate==1 && time > vac_mutate_time)
	{
	    parameters$bstar = parameters$bstar * new_strain_fact
	    parameters$severity = parameters$severity*new_strain_severity
	    parameters$vac_on=0 # don't double vaccinate!
	    pvac_exp_rate=vac_exp_rate
	    vac_exp_rate<<-0 # don't double expire vaccines!

	    if (expon_imports ==1)
		inf_intros <<-new_strain_intros*(1+(time-vac_mutate_time)/7) * the_age_prop
	    else
		inf_intros <<-new_strain_intros*the_age_prop

	    vac_eff_pi <<- vac_eff_pi2
	    vac_eff_susc <<- vac_eff_susc2
	    vac_eff_inf <<- vac_eff_inf2

	    dstate2 = model_eq_set(Vcum_hosp_idx+4,time, state, parameters)
	    vac_exp_rate<<-pvac_exp_rate

	    result=dstate1[[1]][1:shared_entries]+dstate2[[1]][1:shared_entries]

	    # add 1st strain dedicated compartments (exclude s, vs, vex, n and cumulatives, but include sd_adj)
	    result=c(result,dstate1[[1]][(shared_entries+1):length(dstate1[[1]])])

	    # add 2nd strain dedicated compartments (exclude s, vs, , vex, n, sd_adj and cumulatives)
	    result=c(result,dstate2[[1]][(shared_entries+adj_once_entries+1):length(dstate2[[1]])])
	    dstate = list(result)

	    return (dstate)
	}
	else
	{
	    dstate2=rep(0,(length(dstate1[[1]])-(shared_entries+adj_once_entries)))
	    dstate = list(c(dstate1[[1]],dstate2))
	    return (dstate)
	}
    }
    # handle  Reff calculation w/ 2 strains here!
    else
    {
	Reff1 = model_eq_set(E_idx,time, state, parameters, T)

	if (vac_mutate==1 && time > vac_mutate_time)
	{
	    parameters$bstar = parameters$bstar * new_strain_fact
	    parameters$vac_on=0 # don't double vaccinate!
	    parameters$severity = parameters$severity*new_strain_severity

	    pvac_exp_rate=vac_exp_rate
	    vac_exp_rate<<-0 # don't double expire vaccines!
	    if (expon_imports ==1)
		inf_intros <<-new_strain_intros*(1+(time-vac_mutate_time)/7) * the_age_prop
	    else
		inf_intros <<-new_strain_intros*the_age_prop
	    
	    vac_eff_pi <<- vac_eff_pi2
	    vac_eff_susc <<- vac_eff_susc2
	    vac_eff_inf <<- vac_eff_inf2

	    Reff2 = model_eq_set(Vcum_hosp_idx+4,time, state, parameters, T)
	    vac_exp_rate<<-pvac_exp_rate

	    return(max(Reff1,Reff2))
	}
	else
	{
	    return(Reff1)
	}

    }
  })
}

model_eq_set = function(state_index, time, state, parameters, Reff = F) {
par <- as.list(c(state, parameters))
  with(par, { 
    
    # non-vaccinated populations (s, vs and cumulatives shared between strains)
    S_i = matrix( state[S_idx], nrow = 4, ncol = 1)         # current and cumulative number of susceptible individuals
    VS_i = matrix( state[VS_idx], nrow = 4, ncol = 1)         # current and cumulative number of susceptible vacinees
    Vtot_i = matrix( state[Vtot_idx], nrow = 4, ncol = 1)       # cumulative number of vaccinated vacinees
    tot_tests_i = matrix(state[tot_tests_idx], nrow = 4, ncol = 1)	# total tests
    tot_diag_i = matrix(state[tot_diag_idx], nrow = 4, ncol = 1)	# total cases including vaccinated ones
    tot_deaths_i = matrix(state[tot_deaths_idx], nrow = 4, ncol = 1)	# total deaths including vaccinated ones
    tot_Inf_i = matrix(state[tot_inf_idx], nrow = 4, ncol = 1)		# total infections including vaccinated ones
    tot_hosp_i = matrix(state[tot_hosp_idx], nrow = 4, ncol = 1)	# total hospitalization including vaccinated ones
    VEX_i = matrix(state[VEX_idx], nrow = 4, ncol = 1)	# total vaccine expirees
    N_i = matrix(state[age_pop_idx], nrow = 4, ncol = 1)	# total population by age
    sd_adj_i = matrix( state[sd_adj_idx], nrow = 4, ncol = 1)       # adjusted sd values (1st strain only!)


    E_i = matrix( state[state_index], nrow = 4, ncol = 1)         # current number of exposed individuals
    state_index = state_index+4
    A_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current number of Asymptomatic individuals
    state_index = state_index+4
    P_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current number of Pre-symptomatic individuals
    state_index = state_index+4
    I_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current number of Infected individuals
    state_index = state_index+4
    D_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current number of Diagnosed individuals
    state_index = state_index+4
    DA_i = matrix( state[state_index], nrow = 4, ncol = 1)       # Current number of Diagnosed Asymptomatic individuals
    state_index = state_index+4
    H_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current number of Hospitalized individuals
    state_index = state_index+4
    R_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current and cumulative number of Recovered individuals
    state_index = state_index+4
    F_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current and cumulative number of Deceased individuals
    state_index = state_index+4

    cum_exp_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Cumulative number of Exposed individuals
    state_index = state_index+4
    cum_asym_i = matrix( state[state_index], nrow = 4, ncol = 1)       # Cumulative number of Asymptomatic individuals
    state_index = state_index+4
    cum_sym_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Cumulative number of Symptomatic individuals
    state_index = state_index+4
    cum_diag_i = matrix( state[state_index], nrow = 4, ncol = 1)       # Cumulative number of Diagnosed individuals
    state_index = state_index+4
    cum_hosp_i = matrix( state[state_index], nrow = 4, ncol = 1)       # Cumulative number of Hospitalized individuals
    state_index = state_index+4

    # Vaccinated populations
    VE_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current number of Exposed vacinees
    state_index = state_index+4
    VA_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current number of Asymptomatic vacinees
    state_index = state_index+4
    VP_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current number of Pre-symptomatic vacinees
    state_index = state_index+4
    VI_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current number of Infected vacinees
    state_index = state_index+4
    VD_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current number of Diagnosed vacinees
    state_index = state_index+4
    VDA_i = matrix( state[state_index], nrow = 4, ncol = 1)       # Current number of Diagnosed Asymptomatic vacinees
    state_index = state_index+4
    VH_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current number of Hospitalized vacinees
    state_index = state_index+4
    VR_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current and cumulative number of Recovered vacinees
    state_index = state_index+4
    VF_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Current and cumulative number of Deceased vacinees
    state_index = state_index+4

    Vcum_exp_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Cumulative number of Exposed vacinees
    state_index = state_index+4
    Vcum_asym_i = matrix( state[state_index], nrow = 4, ncol = 1)       # Cumulative number of Asymptomatic vacinees
    state_index = state_index+4
    Vcum_sym_i = matrix( state[state_index], nrow = 4, ncol = 1)         # Cumulative number of Symptomatic vacinees
    state_index = state_index+4
    Vcum_diag_i = matrix( state[state_index], nrow = 4, ncol = 1)       # Cumulative number of Diagnosed vacinees
    state_index = state_index+4
    Vcum_hosp_i = matrix( state[state_index], nrow = 4, ncol = 1)       # Cumulative number of Hospitalized vacinees


    # first calculate deltas and mid_month dates in model time
    day0_doy = first_case_doy - delta0offset
    delta1 = delta1_doy - day0_doy
    delta2 = delta2_doy - day0_doy

    mid_month_1 = DiagDate1 - day0_doy # mid-March
    mid_month_2 = DiagDate2 - day0_doy # mid-April
    mid_month_3 = DiagDate3 - day0_doy # mid-May

    local_calib_doy = parameters$calib_doy - day0_doy

    hd1 = 11.2
    hd2 = 20

    # set the monthly rates (note: hosp_rates and death_rates currently unused)
    #
    hosp_fract = monthly_interpolation(time,parameters,hosp_fract1,hosp_fract2,hosp_fract3,hosp_fract4,hosp_fract5,hosp_fract6,hosp_fract7,hosp_fract8,hosp_fract9,hosp_fract10)

    # hosp fract is used in place of 1 - m_i to indicate fraction of infections that are serious
    # if a strain makes infections more serious then it will modify this fraction!
    #hosp_fract = hosp_fract * severity
    cfr = cfr * severity

    nhd = 24

    if (time < mid_month_2) { 
	hd = hd1
    } else if (time < mid_month_3) { 
	hd = hd1 + (time - mid_month_2) / (mid_month_3 - mid_month_2) * (hd2 - hd1) 
    } else {
	hd = hd2
    }

    dtot_tests_i = d_i * (rho_A * (A_i + P_i + VA_i + VP_i) + rho_S * (S_i + E_i + VS_i + VE_i) +  I_i + VI_i)

    d_a = rho_A * d_i
    d_p = rho_A * d_i


    # previously, fmin_i = cfr / (1 - m_i) / hd and hd = time from hosp to death (= 1/death_rate)...
    # assuming hosp_fract ~= 1 - m_i ...
    fmin_i = c(0,0,0,0)
    die_i = c(0,0,0,0)
    for (i in 1:4) {
	if (hosp_fract[i]>0) {
	    fmin_i[i] = cfr[i] / hosp_fract[i] / hd
    	    die_i[i] = cfr[i] / hosp_fract[i] / nhd
	}
    }

    fmax_i = 2 * fmin_i
    
    hstar_i = h_i * hosp_fract * severity      # Hospitalization rate among symptomatic (both diagnosed and not) by age (calculated)
    pvac = p - vac_eff_pi *p	     # Symptomatic fraction
    rstar_i = r_2 * (1 - hosp_fract) # Recovery rate of the  symptomatic (both diagnosed and not) by age (calculated)
    
#
# Uncomment below if you want to remove min SD values some time after vaccionation starts.
#
#    sd_unlock_date = 366 + yday(ymd("2021-2-01"))
#    if (time > sd_unlock_date - day0_doy && vac_rate > 0) 
#    { 
#        if (time > sd_unlock_date + 14 - day0_doy)	
#	{
#		dynamic_sd_min=max(dynamic_sd_min-0.3,0)
#		dynamic_sd_min_snrs=max(dynamic_sd_min_snrs-0.3,0)
#	} else if (time > sd_unlock_date + 7 - day0_doy)	{
#		dynamic_sd_min=max(dynamic_sd_min-0.2,0)
#		dynamic_sd_min_snrs=max(dynamic_sd_min_snrs-0.2,0)
#	} else {
#		dynamic_sd_min=max(dynamic_sd_min-0.1,0)
#		dynamic_sd_min_snrs=max(dynamic_sd_min_snrs-0.1,0)
#	}
#    }


    vac_start_day = vac_schedule[1,1] - day0_doy
    vac_stop_day = vac_stop_doy - day0_doy
    
    # to avoid stopping vaccines abruptly and thwarting the LSODA solver, I gradually taper the
    # vaccination rate for the last 10% of the total vaccines...
    if (vac_on && vac_rate > 0 && time >= vac_start_day && time <= vac_stop_day) 
    { 
	if (vac_mutate==1 && time > vac_mutate_time)
	{
	    if (expon_imports ==1)
		new_intros = new_strain_intros*(1+(time-vac_mutate_time)/7) * the_age_prop
	    else
		new_intros = new_strain_intros*the_age_prop
	}
	else
	    new_intros = inf_intros
	# vac_dist_i uses current susc ratios: vac_dist_i = c(S_i[1]/sum(S_i),S_i[2]/sum(S_i),S_i[3]/sum(S_i),S_i[4]/sum(S_i))
	# or uses a custom mix using the vac_first age group (1=0-19, 2=20-49, 3=50-69, 4=70+)
	# CHANGE: 1/12/21 - do NOT vaccinate age group 0-19
	vac_dist_i = c(0,0,0,0)

	if (vac_first != 0)
	{
	    if (vac_first == 4)
	    {
		if (S_i[4] > new_intros + 1 && Vtot_i[4] < N_i[4]*vac_coverage) {
		    vac_dist_i = c(0,0.1,0.1,0.8)
		}
		else if (S_i[3] > new_intros + 1 && Vtot_i[3] < N_i[3]*vac_coverage) {
		    vac_dist_i = c(0,0.2,0.8,0)
		    #print(paste("Last batch for 70+ age group at t=",time,"S_i[4]=",S_i[4]))
		}
		else if (Vtot_i[2] < N_i[2]*vac_coverage) {
		    vac_dist_i = c(0,1.0,0,0)
		    #print(paste("Last batch for 50-69 age group at t=",time,"S_i[3]=",S_i[3]))
		}
	    }
	    if (vac_first == 3)
	    {
		if (S_i[3] > new_intros + 1 && Vtot_i[3] < N_i[3]*vac_coverage)
		    vac_dist_i = c(0,0.1,0.8,0.1)
		else if (S_i[4] > new_intros + 1 && Vtot_i[4] < N_i[4]*vac_coverage)
		    vac_dist_i = c(0,0.2,0,0.8)
		else if (S_i[2] > new_intros + 1 && Vtot_i[2] < N_i[2]*vac_coverage) {
		    vac_dist_i = c(0,1.0,0,0)
		}
	    }
	    if (vac_first == 2)
	    {
		if (S_i[2] > new_intros + 1 && Vtot_i[2] < N_i[2]*vac_coverage)
		    vac_dist_i = c(0,0.8,0.1,0.1)
		else if (S_i[3] > new_intros + 1 && Vtot_i[3] < N_i[3]*vac_coverage)
		    vac_dist_i = c(0,0,0.8,0.2)
		else if (S_i[4] > new_intros + 1 && Vtot_i[4] < N_i[4]*vac_coverage) {
		    vac_dist_i = c(0,0,0,1)
		}
	    }
	}
	else
	    vac_dist_i = c(0,(S_i[1]/3 + S_i[2])/sum(S_i),(S_i[1]/3 + S_i[3])/sum(S_i),(S_i[1]/3 + S_i[4])/sum(S_i))
	    # old way: vac_dist_i = c(S_i[1]/sum(S_i),S_i[2]/sum(S_i),S_i[3]/sum(S_i),S_i[4]/sum(S_i))

	v_i = vac_rate * vac_dist_i
    }
    else
	v_i = c(0,0,0,0)
    
    # intervention scenarios
    beta_ad = beta_a * beta_d # need to reduce asyptomatic trasmission rate if diagnosed

    int_start_day = intervention_day - delta0offset

    # to avoid thwarting the LSODA solver, gradually implement the chosen "interventions"
    # here are the final values of the interventions...
    new_beta_d = beta_d * beta_d_fact
    new_beta_ad = beta_ad * beta_d_fact
    new_hstar_i = hstar_i * hstar_fact
    new_fmin_i = fmin_i * f_fact
    new_fmax_i = fmax_i * f_fact

    if (time > int_start_day && time <= int_start_day+int_rampup)
    {
      beta_d = beta_d + (new_beta_d - beta_d) * (time - int_start_day) / int_rampup
      beta_ad = beta_ad + (new_beta_ad - beta_ad) * (time - int_start_day) / int_rampup
      hstar_i = hstar_i + (new_hstar_i - hstar_i) * (time - int_start_day) / int_rampup
      fmin_i = fmin_i + (new_fmin_i - fmin_i) * (time - int_start_day) / int_rampup
      fmax_i = fmax_i + (new_fmax_i - fmax_i) * (time - int_start_day) / int_rampup
      if (!is.na(r_3_intervention)) { r_3 = r_3 + (r_3_intervention - r_3)* (time - int_start_day) / int_rampup }  
    }
    else if (time > int_start_day + int_rampup)
    {
      beta_d = beta_d * beta_d_fact
      beta_ad = beta_ad * beta_d_fact
      hstar_i = hstar_i * hstar_fact
      fmin_i = fmin_i * f_fact
      fmax_i = fmax_i * f_fact
      if (!is.na(r_3_intervention)) { r_3 = r_3_intervention }  
      #if (!is.na(d2_intervention)) { d_i = rep(d2_intervention, 4) }
    }

    hvac_i = (1 - vac_eff_hi) * hstar_i
    
    # calculate transmission decrease based on time period
    sd = c(sd1_1, sd1_2, sd1_3, sd1_4) # lockdown period (to 5/1)
    sd2 = c(sd2_1, sd2_2, sd2_3, sd2_4) # May SD values
    sd3 = c(sd3_1, sd3_2, sd3_3, sd3_4) # June SD values
    sd4 = c(sd4_1, sd4_2, sd4_3, sd4_4) # July SD values
    sd5 = c(sd5_1, sd5_2, sd5_3, sd5_4) # Aug SD values
    sd6 = c(sd6_1, sd6_2, sd6_3, sd6_4) # Sept SD values
    sd7 = c(sd7_1, sd7_2, sd7_3, sd7_4) # Oct SD values
    sd8 = c(sd8_1, sd8_2, sd8_3, sd8_4) # Nov SD values
    sd9 = c(sd9_1, sd9_2, sd9_3, sd9_4) # Dec SD values

    # set hospitalization rates based on time period (fitted values)
    sd_curr = sd_monthly(time,parameters,sd,sd2,sd3,sd4,sd5,sd6,sd7,sd8,sd9,sd_adj_i)

    if (time > local_calib_doy)
    {
	sd_adj_1 = max(min(sd_adj_i[1] + sd_inc[1],dynamic_sd_max),dynamic_sd_min)
	sd_adj_2 = max(min(sd_adj_i[2] + sd_inc[2],dynamic_sd_max),dynamic_sd_min)
	sd_adj_3 = max(min(sd_adj_i[3] + sd_inc[3],dynamic_sd_max),dynamic_sd_min)
	sd_adj_4 = max(min(sd_adj_i[4] + sd_inc[4],dynamic_sd_max_snrs),dynamic_sd_min_snrs)
	dsd_adj_i=c(sd_adj_1-sd_adj_i[1],sd_adj_2-sd_adj_i[2],sd_adj_3-sd_adj_i[3],sd_adj_4-sd_adj_i[4])
    }
    else
    {
	dsd_adj_i=c(sd_curr[1]-sd_adj_i[1],sd_curr[2]-sd_adj_i[2],sd_curr[3]-sd_adj_i[3],sd_curr[4]-sd_adj_i[4])
    }
    
    if (print_debug)
    {
	print(paste(time,rho_S[1],rho_S[2],rho_S[3],rho_S[4],d_i[1],d_i[2],d_i[3],d_i[4],sd_curr[1],sd_curr[2],sd_curr[3],sd_curr[4],hstar_i[1],hstar_i[2],hstar_i[3],hstar_i[4],cfr[1],cfr[2],cfr[3],cfr[4],tot_Inf_i[1],tot_Inf_i[2],tot_Inf_i[3],tot_Inf_i[4],tot_Inf_i[1]+tot_Inf_i[2]+tot_Inf_i[3]+tot_Inf_i[4],tot_diag_i[1],tot_diag_i[2],tot_diag_i[3],tot_diag_i[4],tot_diag_i[1]+tot_diag_i[2]+tot_diag_i[3]+tot_diag_i[4],tot_hosp_i[1],tot_hosp_i[2],tot_hosp_i[3],tot_hosp_i[4],tot_hosp_i[1]+tot_hosp_i[2]+tot_hosp_i[3]+tot_hosp_i[4],tot_deaths_i[1],tot_deaths_i[2],tot_deaths_i[3],tot_deaths_i[4],tot_deaths_i[1]+tot_deaths_i[2]+tot_deaths_i[3]+tot_deaths_i[4],sep=","))
    }

    # calculate transmission decrease based on time period
    bfact = 1 - sd_curr

    # calculate transmission for diagnosed, they don't relax social distancing
    bfact_d = if (time < delta1) { 1 }
              else if (time >= delta1 & time < delta2) { 1 - (time - delta1) / (delta2 - delta1) * sd }
              else if (time >= delta2) { 1 - sd }
    
    # Calculated, state dependent parameters: lambda_i
    # Risk of the susceptible individuals in age group i to acquire infection through contacts with infected individuals from different age group and infection status
    lambda_i=c(0,0,0,0)

    if (vac_irresp == 0)
    {
	lambda_i <- c_ij %*% ( bstar * ( bfact * (beta_a * A_i + beta_p * P_i + beta_s * I_i) +
			bfact_d * (beta_d * D_i + beta_ad * DA_i) + beta_h * H_i + 
		((1 - vac_eff_inf) * (bfact * (beta_a * VA_i + beta_p * VP_i + beta_s * VI_i) + 
			bfact_d * (beta_d * VD_i + beta_ad * VDA_i)+ beta_h * VH_i))) / N_i )

	lambda_vi = (1 - vac_eff_susc) * lambda_i #if sd applied enverywhere
    }
    else 
    {
	lambda_i <- c_ij %*% ( bstar * ( bfact * (beta_a * A_i + beta_p * P_i + beta_s * I_i) +
			bfact_d * (beta_d * D_i + beta_ad * DA_i) + beta_h * H_i + 
		((1 - vac_eff_inf) * ((beta_a * VA_i + beta_p * VP_i + beta_s * VI_i) + 
			bfact_d * (beta_d * VD_i + beta_ad * VDA_i)+ beta_h * VH_i))) / N_i )

        lambda_vi <- (1 - vac_eff_susc) *c_ij %*% ( bstar * ( bfact * (beta_a * A_i + beta_p * P_i + beta_s * I_i) +
			bfact_d * (beta_d * D_i + beta_ad * DA_i) + beta_h * H_i + 
		((1 - vac_eff_inf) * ((beta_a * VA_i + beta_p * VP_i + beta_s * VI_i) + 
			bfact_d * (beta_d * VD_i + beta_ad * VDA_i)+ beta_h * VH_i))) / N_i )
    }

    # calculate hospital mortality
    mort_i <- if (f_simple) { fmin_i } # simple mortality without sigmoid
    else { fmin_i + (fmax_i - fmin_i) / (1 + exp( -fs * (sum(q_i * H_i) + sum(q_i * VH_i) - icu) )) }
    
    ####Start Reff Calculation####
    if(Reff){
      #R effective calculation
      ###Warning###
      # Will have to change this if the model is changed
      
      #Calculate time in each state (should match exponential decay rates below)
      Time_Ei = 1/gamma_1
      Time_Ai = 1/(d_a + r_1)
      Time_Pi = 1/(d_p + gamma_2)
      Time_Ii = 1/(d_i + hstar_i + rstar_i)
      Time_Di = 1/(die_i + hstar_i + rstar_i)
      Time_DAi = 1/r_1
      Time_Hi = 1/(r_3 + mort_i)
      
      #Fraction that enter each state
      Frac_Ei = 1
      Frac_Ai = 1-p
      Frac_Pi = p
      Frac_Ii = Frac_Pi*(gamma_2/(gamma_2 + d_i))
      Frac_Di = Frac_Ii*(d_i/(d_i + hstar_i + rstar_i)) + Frac_Pi*(d_p/(gamma_2 + d_p))
      Frac_DAi = Frac_Ai*d_a/(d_a + r_1)
      Frac_Hi = Frac_Ii*(hstar_i/(d_i + hstar_i + rstar_i)) + Frac_Di*(hstar_i/(die_i + hstar_i + rstar_i))
      
      #Transmission by state
      Trans_Ei = 0 * Frac_Ei * Time_Ei
      Trans_Ai = beta_a * Frac_Ai * Time_Ai
      Trans_Pi = beta_p * Frac_Pi * Time_Pi
      Trans_Ii = beta_s * Frac_Ii * Time_Ii
      Trans_Di = beta_d * Frac_Di * Time_Di
      Trans_DAi = beta_ad * Frac_DAi * Time_DAi
      Trans_Hi = beta_h * Frac_Hi * Time_Hi
      
      #Calculate time in each vaccinated state
      Time_VEi = 1/gamma_1
      Time_VAi = 1/(d_a + r_1)
      Time_VPi = 1/(d_p + gamma_2)
      Time_VIi = 1/(d_i + hvac_i + rstar_i)
      Time_VDi = 1/(die_i + hvac_i + rstar_i)
      Time_VDAi = 1/r_1
      Time_VHi = 1/(r_3 + mort_i)
      
      #Fraction that enter each state
      Frac_VEi = 1
      Frac_VAi = 1-pvac
      Frac_VPi = pvac
      Frac_VIi = Frac_VPi*(gamma_2/(gamma_2 + d_p))
      Frac_VDi = Frac_VIi*(d_i/(d_i + hvac_i + rstar_i)) + Frac_VPi*(d_p/(gamma_2 + d_p))
      Frac_VDAi = Frac_VAi*d_a/(d_a + r_1)
      Frac_VHi = Frac_VIi*(hvac_i/(d_i + hvac_i + rstar_i)) + Frac_VDi*(hvac_i/(die_i + hvac_i + rstar_i))
      
      #Transmission by state
      Trans_VEi = 0 * Frac_VEi * Time_VEi
      Trans_VAi = beta_a * Frac_VAi * Time_VAi
      Trans_VPi = beta_p * Frac_VPi * Time_VPi
      Trans_VIi = beta_s * Frac_VIi * Time_VIi
      Trans_VDi = beta_d * Frac_VDi * Time_VDi
      Trans_VDAi = beta_ad * Frac_VDAi * Time_VDAi
      Trans_VHi = beta_h * Frac_VHi * Time_VHi
      
      #Total Transmission
      Trans_i = as.numeric(((Trans_Ei + Trans_Ai + Trans_Pi + Trans_Ii) * bfact + (Trans_Di + Trans_DAi) * bfact_d + Trans_Hi) * bstar / N_i)
      Trans_Vi = as.numeric((1 - vac_eff_inf) * ((Trans_VEi + Trans_VAi + Trans_VPi + Trans_VIi) * bfact + (Trans_VDi + Trans_VDAi) * bfact_d + Trans_VHi) * bstar / N_i)
      
      #Transition matrix satisfying y = Ax where x is the number of infected people (by age group and vaccination status) and  y is the number of people they infection.
      #We wish to solve y = Reff x = Ax SO Reff is the dominant eigen value of A.
      
      chat_ij = rbind(cbind(c_ij, c_ij), cbind(c_ij, c_ij))

      Trans.matrix = diag(c(S_i + VEX_i, (1 - vac_eff_susc) * VS_i))%*%chat_ij%*%diag(c(Trans_i, Trans_Vi))

      return(as.numeric(eigen(Trans.matrix)$values[1]))
    }
    #End Reff Calculation
    vac_exp_daily = vac_exp_rate / 30
    
    # Formulas for change in each state 
    # All terms should be either vectors with 4 elements (subscript j) or single values
    dS_i = -1 * lambda_i * S_i - v_i * (S_i) / ((S_i) + (R_i)) -inf_intros # Change in Susceptibles (infection OR vaccination)

    dVEX_i = -1 * lambda_i * VEX_i + vac_exp_daily * VS_i # Change in Vaccine exempt Susceptibles 
    dE_i = inf_intros + lambda_i * S_i + lambda_i * VEX_i - gamma_1 * E_i            # Change in Exposed

    dA_i = (1 - p) * gamma_1 * E_i - d_a * A_i - r_1 * A_i              # Change in Asymptomatic
    dP_i = p * gamma_1 * E_i - d_p * P_i - gamma_2 * P_i                # Change in Pre-symptomatic
    dI_i = gamma_2 * P_i - d_i * I_i - hstar_i * I_i - rstar_i * I_i     # Change in Infected 
    dD_i = d_p * P_i + d_i * I_i - hstar_i * D_i - rstar_i * D_i - die_i * D_i          # Change in Diagnosed
    dDA_i = d_a * A_i - r_1 * DA_i                                       # Change in Asym Diagnosed
    dH_i = hstar_i * I_i + hstar_i * D_i - r_3 * H_i - mort_i * H_i      # Change in Hospitalized
    dR_i = r_1 * A_i + r_1 * DA_i + rstar_i * I_i + rstar_i * D_i + r_3 * H_i - 
	v_i * (R_i) / ((S_i) + (R_i)) 				# Change in Recovered
    dF_i = mort_i * H_i + die_i * D_i                                                 # Change in Deaths
    
    # summary statistics 
    dcum_exp_i = lambda_i * S_i + lambda_i * VEX_i                      # new E infections (exposed)
    dcum_asym_i = (1 - p) * gamma_1 * E_i                               # new A asymptomatic
    dcum_sym_i = gamma_2 * P_i                                            # new I symptomatic
    dcum_diag_i = d_i * I_i + hstar_i * I_i + d_a * A_i + d_p * P_i         # new cases (diagnosed + directly hosptalized)
    dcum_hosp_i = hstar_i * I_i + hstar_i * D_i                           # new hospitalizations
    
    dVS_i = v_i * (S_i) / ((S_i) + (R_i)) + -1 * lambda_vi * VS_i - vac_exp_daily*VS_i  # Change in Vaccinated Susceptibles
    dVE_i = lambda_vi * VS_i - gamma_1 * VE_i                             # Change in Vaccinated Exposed
    dVA_i = (1 - pvac) * gamma_1 * VE_i - d_a * VA_i - r_1 * VA_i          # Change in Vaccinated Asymptomatic
    dVP_i = pvac * gamma_1 * VE_i - d_p * VP_i - gamma_2 * VP_i            # Change in Vaccinated Pre-symptomatic
    dVI_i = gamma_2 * VP_i - d_i * VI_i - hvac_i * VI_i - rstar_i * VI_i  # Change in Vaccinated Infected 
    dVD_i = d_p * VP_i + d_i * VI_i - hvac_i * VD_i - rstar_i * VD_i - die_i * VD_i       # Change in Vaccinated Diagnosed
    dVDA_i = d_a * VA_i - r_1 * VDA_i                                      # Change in Vaccinated Asym Diagnosed
    dVH_i = hvac_i * VI_i + hvac_i * VD_i - r_3 * VH_i - mort_i * VH_i    # Change in Vaccinated Hospitalized
    dVR_i = r_1 * VA_i + r_1 * VDA_i + rstar_i * VI_i + 
	rstar_i * VD_i + r_3 * VH_i + v_i * (R_i) / ((S_i) + (R_i))		# Change in Vaccinated Recovered

    dVF_i = mort_i * VH_i + die_i * VD_i                                                  # Change in Vaccinated Deaths
    

    # summary statistics 
    dVcum_exp_i = lambda_vi * VS_i                                           # new E infections (exposed)
    dVcum_asym_i = (1 - pvac) * gamma_1 * VE_i                               # new A asymptomatic
    dVcum_sym_i = gamma_2 * VP_i                                            # new I symptomatic
    dVcum_diag_i = d_i * VI_i + hvac_i * VI_i + d_a * VA_i + d_p * VP_i         # new cases (diagnosed + directly hosptalized)
    dVcum_hosp_i = hvac_i * VI_i + hvac_i * VD_i                           # new hospitalizations
    dVtot_i = v_i                           # new vaccinations

    dtot_diag_i = dcum_diag_i + dVcum_diag_i				# new cases including vaccinated ones
    dtot_deaths_i = dF_i + dVF_i						# new deaths including vaccinated ones
    dtot_Inf_i = dcum_asym_i + dcum_sym_i + dVcum_asym_i + dVcum_sym_i	# new infections including vaccinated ones
    dtot_hosp_i = dcum_hosp_i + dVcum_hosp_i				# new hospitalization including vaccinated ones
    
    dN_i = - dF_i - dVF_i

    list(c(dS_i, # Change in Susceptibles
	   dVS_i, # Change in Susceptibles
	   dtot_tests_i,	# new tests
	   dVtot_i,         # new vaccinations
	   dtot_diag_i,	# new cases including vaccinated ones
	   dtot_deaths_i,	# new deaths including vaccinated ones
	   dtot_Inf_i,	# new infections including vaccinated ones
	   dtot_hosp_i,	# new hospitalization including vaccinated ones
	   dVEX_i,	# new vaccine expired pop
	   dN_i,	# change to total pop by age group
	   dsd_adj_i,         # new sd adjustments
           dE_i, # Change in Exposed
           dA_i, # Change in Asymptomatic
           dP_i, # Change in Pre-symptomatic
           dI_i, # Change in Infected
           dD_i, # Change in Diagnosed
           dDA_i, # Change in Diagnosed Asymptomatic
           dH_i, # Change in Hospitalized
           dR_i, # Change in Recovered
           dF_i, # Change in Deaths
           dcum_exp_i,    # new infections (exposed)
           dcum_asym_i,   # new asymptomatic
           dcum_sym_i,    # new symptomatic
           dcum_diag_i,   # new cases (diagnosed + directly hosptalized)
           dcum_hosp_i,   # new hospitalizations
           dVE_i, # Change in Exposed
           dVA_i, # Change in Asymptomatic
           dVP_i, # Change in Pre-symptomatic
           dVI_i, # Change in Infected
           dVD_i, # Change in Diagnosed
           dVDA_i, # Change in Diagnosed Asymptomatic
           dVH_i, # Change in Hospitalized
           dVR_i, # Change in Recovered
           dVF_i, # Change in Deaths
           dVcum_exp_i,    # new infections (exposed)
           dVcum_asym_i,   # new asymptomatic
           dVcum_sym_i,    # new symptomatic
           dVcum_diag_i,   # new cases (diagnosed + directly hosptalized)
           dVcum_hosp_i   # new hospitalizations
         )) 
  })
}
