shape_data = function(ode_out)
{
  data_out = melt(as.data.frame(as.matrix(ode_out)), id='time') %>%
    separate(variable, c("state", "age"), "_i")
  data_out$age = as.numeric(data_out$age) # otherwise it's a character from the separate()
  return(data_out)
}

get_model_data <- function(pars, pars_names, pars_fixed, daycount, state)
{
  parameters = get_params(pars, pars_names, pars_fixed)
  print200=0
  print350=0
  print500=0
  print650=0

  day0_doy = parameters$first_case_doy - parameters$delta0offset
  daycount = daycount - day0_doy
 
  delta1 = parameters$delta1_doy - day0_doy
  delta2 = parameters$delta2_doy - day0_doy
  delta3 = parameters$delta3_doy - day0_doy # May 1st
  delta4 = parameters$delta4_doy - day0_doy # June 1st
  delta5 = parameters$delta5_doy - day0_doy # July 1st
  delta6 = parameters$delta6_doy - day0_doy # Aug 1st
  delta7 = parameters$delta7_doy - day0_doy # Sept 1st
  delta8 = parameters$delta8_doy - day0_doy # Oct 1st
  delta9 = parameters$delta9_doy - day0_doy # Nov 1st
  delta10 = parameters$delta10_doy - day0_doy # Dec 1st
  delta11 = parameters$delta11_doy - day0_doy # Jan 1st

  mid_month1 = parameters$DiagDate1 - day0_doy # mid-March
  mid_month2 = parameters$DiagDate2 - day0_doy # mid-April
  mid_month3 = parameters$DiagDate3 - day0_doy # mid-May
  mid_month4 = parameters$DiagDate4 - day0_doy # mid-June
  mid_month5 = parameters$DiagDate5 - day0_doy # mid-July
  mid_month6 = parameters$DiagDate6 - day0_doy # mid-Aug
  mid_month7 = parameters$DiagDate7 - day0_doy # mid-Sept
  mid_month8 = parameters$DiagDate8 - day0_doy # mid-Oct
  mid_month9 = parameters$DiagDate9 - day0_doy # mid-Nov
  mid_month10 = parameters$DiagDate10 - day0_doy # mid-Dec
  mid_month11 = parameters$DiagDate11 - day0_doy # mid-Jan
  
  sd = c(parameters$sd1_1,parameters$sd1_2,parameters$sd1_3,parameters$sd1_4)
  sd2 = c(parameters$sd2_1,parameters$sd2_2,parameters$sd2_3,parameters$sd2_4)
  sd3 = c(parameters$sd3_1,parameters$sd3_2,parameters$sd3_3,parameters$sd3_4)
  sd4 = c(parameters$sd4_1,parameters$sd4_2,parameters$sd4_3,parameters$sd4_4)
  sd5 = c(parameters$sd5_1,parameters$sd5_2,parameters$sd5_3,parameters$sd5_4)
  sd6 = c(parameters$sd6_1,parameters$sd6_2,parameters$sd6_3,parameters$sd6_4)
  sd7 = c(parameters$sd7_1,parameters$sd7_2,parameters$sd7_3,parameters$sd7_4)
  sd8 = c(parameters$sd8_1,parameters$sd8_2,parameters$sd8_3,parameters$sd8_4)
  sd9 = c(parameters$sd9_1,parameters$sd9_2,parameters$sd9_3,parameters$sd9_4)

  local_calib_doy = parameters$calib_doy - day0_doy

  all_time = seq(1, daycount, by = 1)

  if (parameters$dynamic_sd == TRUE)
  {
    # run model until local_calib_doy, then proceed bi-weekly to update sd targets
    out = ode(y = state, times = seq(0, local_calib_doy, by = 1), func = model_eq, parms = parameters, method = "euler", hini = ode_step_time, maxsteps=10000)

    increment = parameters$dynamic_sd_period
    
    saved_cum_diag = out[(nrow(out)-increment),"tot_diag_i1"]+out[(nrow(out)-increment),"tot_diag_i2"]+out[(nrow(out)-increment),"tot_diag_i3"]+out[(nrow(out)-increment),"tot_diag_i4"]
    saved_cum_hosp = out[(nrow(out)-increment),"tot_hosp_i1"]+out[(nrow(out)-increment),"tot_hosp_i2"]+out[(nrow(out)-increment),"tot_hosp_i3"]+out[(nrow(out)-increment),"tot_hosp_i4"]

    curr_cum_diag = out[nrow(out),"tot_diag_i1"]+out[nrow(out),"tot_diag_i2"]+out[nrow(out),"tot_diag_i3"]+out[nrow(out),"tot_diag_i4"]
    curr_cum_hosp = out[nrow(out),"tot_hosp_i1"]+out[nrow(out),"tot_hosp_i2"]+out[nrow(out),"tot_hosp_i3"]+out[nrow(out),"tot_hosp_i4"]
    last_case_delta = curr_cum_diag - saved_cum_diag
    last_hosp_delta = curr_cum_hosp - saved_cum_hosp
    print(paste("At t=",i,",bi-weekly case delta=",last_case_delta,"bi-weekly hosp delta=",last_hosp_delta))

    saved_cum_diag = curr_cum_diag
    saved_cum_hosp = curr_cum_hosp
    
    parameters$sd_inc=c(0,0,0,0)

    sd_high=c(parameters$dynamic_sd_max,parameters$dynamic_sd_max,parameters$dynamic_sd_max,parameters$dynamic_sd_max_snrs)
    sd_low=c(parameters$dynamic_sd_min,parameters$dynamic_sd_min,parameters$dynamic_sd_min,parameters$dynamic_sd_min_snrs)

    # rescale high/low if eval period != 2 weeks!
    dynamic_sd_limit=parameters$dynamic_sd_limit*(increment/14)
    dynamic_sd_hyster=parameters$dynamic_sd_hyster*(increment/14)

    if (daycount > local_calib_doy)
    {
	for (i in seq(from = local_calib_doy, to = daycount, by = increment))
	{
	  last_state = tail(out[,-1], 1)
	  # at mutation time, shift a proportion of those exposed to main to be new strain
	  if (vac_mutate==1 && i <= vac_mutate_time && i + increment > vac_mutate_time && parameters$new_strain_preval > 0)
	  {
		#print(paste("Shifting infs at time=",i))

		# 1st do exposed
		last_state[E2_idx]= parameters$new_strain_preval * last_state[E_idx]
		last_state[E_idx]= (1 - parameters$new_strain_preval) * last_state[E_idx]
		#print("From exposed move...")
		#print(last_state[E2_idx])

		# next do asympts
		last_state[A2_idx]= parameters$new_strain_preval * last_state[A_idx]
		last_state[A_idx]= (1 - parameters$new_strain_preval) * last_state[A_idx]
		#print("From asympt move...")
		#print(last_state[A2_idx])

		# next do pre-sympt
		last_state[P2_idx]= parameters$new_strain_preval * last_state[P_idx]
		last_state[P_idx]= (1 - parameters$new_strain_preval) * last_state[P_idx]
		#print("From pre-sympt move...")
		#print(last_state[P2_idx])

		# next do sympts
		last_state[I2_idx]= parameters$new_strain_preval * last_state[I_idx]
		last_state[I_idx]= (1 - parameters$new_strain_preval) * last_state[I_idx]
		#print("From sympt move...")
		#print(last_state[I2_idx])

	  }
	  inc_time = seq(i, i + increment, by = 1)
	  out_inc = ode(y = last_state, times = inc_time, func = model_eq, parms = parameters, method = "euler", hini = ode_step_time, maxsteps=10000)
	  out = rbind(out, out_inc[-1,]) # don't include first row because it is the same as the last row of previous run
	  curr_sd = c(out[nrow(out),"sd_adj_i1"],out[nrow(out),"sd_adj_i2"],out[nrow(out),"sd_adj_i3"],out[nrow(out),"sd_adj_i4"])
	  curr_cum_diag = out[nrow(out),"tot_diag_i1"]+out[nrow(out),"tot_diag_i2"]+out[nrow(out),"tot_diag_i3"]+out[nrow(out),"tot_diag_i4"]
	  curr_cum_hosp = out[nrow(out),"tot_hosp_i1"]+out[nrow(out),"tot_hosp_i2"]+out[nrow(out),"tot_hosp_i3"]+out[nrow(out),"tot_hosp_i4"]
	  curr_case_delta = curr_cum_diag - saved_cum_diag
	  curr_hosp_delta = curr_cum_hosp - saved_cum_hosp
	  
	  # check threshold
	  tighten = 0
	  loosen = 0
	  if (i < new_check_date || parameters$sd_growth_trigger == 0)
	  {
	      sd_limit200 = 110 * the_pop / 100000
	      sd_hyster200 = 90 * the_pop / 100000
	      sd_limit350 = 185 * the_pop / 100000
	      sd_hyster350 = 165 * the_pop / 100000
	      sd_limit500 = 260 * the_pop / 100000
	      sd_hyster500 = 240 * the_pop / 100000
	      sd_limit650 = 335 * the_pop / 100000
	      sd_hyster650 = 315 * the_pop / 100000
	      if (curr_case_delta > sd_limit200 && print200==0) {
		 #print(paste("Saw bi-weekly exceeed 200 at t=",i))
		 print200=1
	      }
	      if (curr_case_delta > sd_limit350 && print350==0) {
		 #print(paste("Saw bi-weekly exceeed 350 at t=",i))
		 print350=1
	      }
	      if (curr_case_delta > sd_limit500 && print500==0) {
		 #print(paste("Saw bi-weekly exceeed 500 at t=",i))
		 print500=1
	      }
	      if (curr_case_delta > sd_limit650 && print650==0) {
		 #print(paste("Saw bi-weekly exceeed 650 at t=",i))
		 print650=1
	      }
	      if (curr_case_delta > (dynamic_sd_limit + dynamic_sd_hyster))
	      {
		    tighten = 1
	      }
	      if (curr_case_delta < (dynamic_sd_limit - dynamic_sd_hyster))
	      {
		    loosen = 1
	      }
	  }
	  else
	  {
	      if (i - increment < new_check_date)
		  print(paste("New policy at time=",i,"saved_cum_diag=",saved_cum_diag,"saved_cum_hosp=",saved_cum_hosp,"curr_cum_diag=",curr_cum_diag,"curr_cum_hosp=",curr_cum_hosp))

	      if (curr_case_delta > last_case_delta * (1 + parameters$sd_growth_trigger) &&
		  curr_hosp_delta > last_hosp_delta * (1 + parameters$sd_growth_trigger))
	      {
		    tighten = 1
		    #print(paste0("Tightening at t=",i,"..."))
		    if (last_case_delta > 0)
		    {
			perc_inc=100*(curr_case_delta-last_case_delta)/last_case_delta
			#print(paste0("Saw bi-weekly case increase=",perc_inc,"%"))
		    }
		    if (last_hosp_delta > 0)
		    {
			perc_inc=100*(curr_hosp_delta-last_hosp_delta)/last_hosp_delta
			#print(paste0("Saw bi-weekly hosp increase=",perc_inc,"%"))
		    }
	      }
	      if (curr_case_delta < last_case_delta * (1 - parameters$sd_decline_trigger) &&
		  curr_hosp_delta < last_hosp_delta * (1 - parameters$sd_decline_trigger))
	      {
		    loosen = 1
		    #print(paste0("Loosening at t=",i,"..."))
		    if (last_case_delta > 0)
		    {
			perc_dec=100*(last_case_delta-curr_case_delta)/last_case_delta
			#print(paste0("Saw bi-weekly case decrease=",perc_dec,"%"))
		    }
		    if (last_hosp_delta > 0)
		    {
			perc_dec=100*(last_hosp_delta-curr_hosp_delta)/last_hosp_delta
			#print(paste0("Saw bi-weekly hosp decrease=",perc_dec,"%"))
		    }
	      }
	  }
	  if (tighten == 1)
	  {
	      #parameters$sd_inc=rep((parameters$dynamic_sd_delta/parameters$dynamic_sd_period),4)
	      parameters$sd_inc=(sd_high - curr_sd)/parameters$dynamic_sd_period
	      #print(paste("rate=",curr_case_delta*100000/the_pop,"SD=",curr_sd[2],"toward high at t=",i,"sd_inc=",parameters$sd_inc[2]))
	  }
	  if (loosen==1)
	  {
	      parameters$sd_inc=rep((-parameters$dynamic_sd_delta/parameters$dynamic_sd_period),4)
	      #parameters$sd_inc=(sd_low - curr_sd)/parameters$dynamic_sd_period
	      #print(paste("rate=",curr_case_delta*100000/the_pop,"SD=",curr_sd[2],"toward low at t=",i,"sd_inc=",parameters$sd_inc[2]))
	  }
	  last_case_delta = curr_cum_diag - saved_cum_diag
	  last_hosp_delta = curr_cum_hosp - saved_cum_hosp
	  #print(paste("At t=",i,",bi-weekly case delta=",last_case_delta,"bi-weekly hosp delta=",last_hosp_delta))
	  saved_cum_diag = curr_cum_diag
	  saved_cum_hosp = curr_cum_hosp
	}
    }
  }

  else
  {
	  out = ode(y = state, times = seq(0, daycount, by = 1), func = model_eq, parms = parameters, method = "euler", hini = ode_step_time, maxsteps=10000)
  }
  if (nrow(out) > daycount)
  {
	out = out[1:daycount,]
  }

  #Added by James to Calculate Reff
  out = append.Reff(out, parameters)
  
  return(out)
}

#This function calls the ODE function with a different flag to tell it to output R effective instead.
append.Reff = function(ode.output, parameters) {
  
  N.times = nrow(ode.output)
  Reff = numeric(N.times)
  
  time = ode.output[,1]
  state = ode.output[,-1]
  
  for(i in seq(N.times)){
    Reff[i] = model_eq(time[i], state[i,], parameters, Reff = T)
  }
  cbind(ode.output, Reff)
}

transform_data_calib_plot = function(data_out, cum_cases)
{
  Deaths <- data_out %>%
    filter(state == "tot_deaths") %>%
    select(-state)
  names(Deaths) <- c("time", "age", "deaths_base")

  Cases <- data_out %>%
    filter(state == "tot_diag") %>%
    select(-state)
  names(Cases) <- c("time", "age", "cases_base")

  preds <- left_join(Cases, Deaths)
  
  calibdata <- right_join(cum_cases, preds)
  
  # need to fill in doy past data to be able to plot true date
  fact_to_add = calibdata$doy[calibdata$time == 0]
  calibdata$doy[is.na(calibdata$doy)] = calibdata$time[is.na(calibdata$doy)] + fact_to_add
  calibdata <- calibdata %>% mutate(date = as.Date(doy + 1, origin = "2020-01-01")) # as.Date is 0-based!!
  
  return(calibdata)
}

transform_vac_data_calib_plot = function(data_out, cum_cases)
{
  Deaths <- data_out %>%
    filter(state == "F") %>%
    select(-state)
  names(Deaths) <- c("time", "age", "deaths_base")

  Cases <- data_out %>%
    filter(state == "cum_diag") %>%
    select(-state)
  names(Cases) <- c("time", "age", "cases_base")

  preds1 <- left_join(Cases, Deaths)
  
  Deaths <- data_out %>%
    filter(state == "VF") %>%
    select(-state)
  names(Deaths) <- c("time", "age", "vac_deaths_base")

  Cases <- data_out %>%
    filter(state == "Vcum_diag") %>%
    select(-state)
  names(Cases) <- c("time", "age", "vac_cases_base")

  preds2 <- left_join(Cases, Deaths)
  
  calibdata <- left_join(preds1, preds2)

  calibdata <- right_join(cum_cases, calibdata)
  
  # need to fill in doy past data to be able to plot true date
  fact_to_add = calibdata$doy[calibdata$time == 0]
  calibdata$doy[is.na(calibdata$doy)] = calibdata$time[is.na(calibdata$doy)] + fact_to_add
  calibdata <- calibdata %>% mutate(date = as.Date(doy + 1, origin = "2020-01-01")) # as.Date is 0-based!!
  
  return(calibdata)
}

transform_data_calib_hosp_plot = function(data_out, daily_hosp)
{
  Hosp <- data_out %>%
    filter(state == "tot_hosp") %>%
    select(-state)
  names(Hosp) <- c("time", "age", "hosp_base")
  
  calibdata <- right_join(daily_hosp, Hosp)
  
  # need to fill in doy past data to be able to plot true date
  fact_to_add = calibdata$doy[calibdata$time == 0]
  calibdata$doy[is.na(calibdata$doy)] = calibdata$time[is.na(calibdata$doy)] + fact_to_add
  calibdata <- calibdata %>% mutate(date = as.Date(doy + 1, origin = "2020-01-01")) # as.Date is 0-based!!
  
  return(calibdata)
}

transform_data_calib_test_plot = function(data_out, daily_tests)
{
  Tests <- data_out %>%
    filter(state == "tot_tests") %>%
    select(-state)
  names(Tests) <- c("time", "age", "test_base")
  
  calibdata <- right_join(daily_tests, Tests)
  
  # need to fill in doy past data to be able to plot true date
  fact_to_add = calibdata$doy[calibdata$time == 0]
  calibdata$doy[is.na(calibdata$doy)] = calibdata$time[is.na(calibdata$doy)] + fact_to_add
  calibdata <- calibdata %>% mutate(date = as.Date(doy + 1, origin = "2020-01-01")) # as.Date is 0-based!!
  
  return(calibdata)
}


transform_data_calib_inf_plot = function(data_out, cohort_data)
{
  Infs <- data_out %>%
    filter(state == "tot_Inf") %>%
    select(-state)
  names(Infs) <- c("time", "age", "infs_base")
  
  calibdata <- right_join(cohort_data, Infs)
  
  # need to fill in doy past data to be able to plot true date
  fact_to_add = calibdata$doy[calibdata$time == 0]
  calibdata$doy[is.na(calibdata$doy)] = calibdata$time[is.na(calibdata$doy)] + fact_to_add
  calibdata <- calibdata %>% mutate(date = as.Date(doy + 1, origin = "2020-01-01")) # as.Date is 0-based!!
  
  return(calibdata)
}

transform_data_calib_reff_plot = function(data_out, daily_hosp)
{
  Reff <- data_out %>%
    filter(state == "Reff") %>%
    select(-state)
  names(Reff) <- c("time", "age", "reff_base")
  
  calibdata <- right_join(daily_hosp, Reff)
  
  # need to fill in doy past data to be able to plot true date
  fact_to_add = calibdata$doy[calibdata$time == 0]
  calibdata$doy[is.na(calibdata$doy)] = calibdata$time[is.na(calibdata$doy)] + fact_to_add
  calibdata <- calibdata %>% mutate(date = as.Date(doy + 1, origin = "2020-01-01")) # as.Date is 0-based!!
  
  return(calibdata)
}

# as called from scenarios.R
# calibdata <- plotdata
# age_data <- kc_age_t
transform_data_calib_age_plot = function(calibdata, age_data)
{
  act_cases = age_data$cases[nrow(age_data$cases),] %>%           # takes just the last calibration date
    gather("age1", "age2", "age3", "age4", key = "age", value = "cases_age") %>%
    mutate(age = as.numeric(substr(age, 4, 4))) 
  
  act_deaths = age_data$deaths[nrow(age_data$deaths),] %>%        # takes just the last calibration date
    gather("age1", "age2", "age3", "age4", key = "age", value = "deaths_age") %>%
    mutate(age = as.numeric(substr(age, 4, 4))) 
  
  calibage = left_join(act_cases, calibdata, by = c("doy", "age"))
  calibage = left_join(calibage, act_deaths, by = c("doy", "age"))
  calibage = mutate(calibage, model_cases = cases_base / sum(cases_base),
                    model_deaths = deaths_base / sum(deaths_base),
                    data_cases = cases_age / cases,
                    data_deaths = deaths_age / deaths)
  
  return(calibage)
}

get_model_data_param_sets = function(pars_matrix, pars_names, pars_fixed, daycount, state, senior_scenario = FALSE)
{
  if (daycount > 385)
      calendar_time = 1:(daycount+20)
  else
      calendar_time = 1:385

  pop = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  susc = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  tests = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  cases = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  deaths = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  hosp = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  cum_hosp = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  symp = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  asymp = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  symp1 = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  asymp1 = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  symp2 = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  asymp2 = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  inf = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  inf1 = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  inf2 = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  recov = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  Reff = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  vax = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  
  susc_1  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  susc_2  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  susc_3  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  susc_4  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  
  tests_1  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  tests_2  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  tests_3  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  tests_4  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  
  cases_1  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  cases_2  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  cases_3  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  cases_4  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  
  deaths_1  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  deaths_2  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  deaths_3  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  deaths_4  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  
  exp_1  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  exp_2  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  exp_3  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  exp_4  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  
  hosp_1  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  hosp_2  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  hosp_3  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  hosp_4  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))

  sd_1  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  sd_2  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  sd_3  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  sd_4  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  
  vac_1  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  vac_2  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  vac_3  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  vac_4  = matrix(NA, nrow = length(calendar_time), ncol = nrow(pars_matrix))
  
  good_ones = 0
  for (i in 1:nrow(pars_matrix))
  {
    print(paste("Filling array for parameter set",i,"...."))
    out = get_model_data(pars_matrix[i,], pars_names, pars_fixed, daycount, state)
    #print(out)
    good_ones= good_ones+1
    parameters = get_params(pars_matrix[i,], pars_names, pars_fixed)
    day0_doy = parameters$first_case_doy - parameters$delta0offset
    doy = day0_doy + out[,1]

    # first column is time
    pop[doy, good_ones] = sum_across_ages(out, "N")$all 
    susc[doy, good_ones] = sum_across_ages(out, "S")$all + sum_across_ages(out, "VEX")$all
    tests[doy, good_ones] = sum_across_ages(out, "tot_test")$all
    cases[doy, good_ones] = sum_across_ages(out, "tot_diag")$all
    deaths[doy, good_ones] = sum_across_ages(out, "tot_deaths")$all
    hosp[doy, good_ones] = sum_across_ages(out, "H")$all + sum_across_ages(out, "VH")$all
    cum_hosp[doy, good_ones] = sum_across_ages(out, "tot_hosp")$all 

    symp[doy, good_ones] = sum_across_ages(out, "cum_sym")$all + sum_across_ages(out, "Vcum_sym")$all
    asymp[doy, good_ones] = sum_across_ages(out, "cum_asym")$all + sum_across_ages(out, "Vcum_asym")$all
    symp1[doy, good_ones] = sum_across_ages(out, "cum_sym_")$all + sum_across_ages(out, "Vcum_sym_")$all
    asymp1[doy, good_ones] = sum_across_ages(out, "cum_asym_")$all + sum_across_ages(out, "Vcum_asym_")$all
    symp2[doy, good_ones] = sum_across_ages(out, "cum_sym2")$all + sum_across_ages(out, "Vcum_sym2")$all
    asymp2[doy, good_ones] = sum_across_ages(out, "cum_asym2")$all + sum_across_ages(out, "Vcum_asym2")$all
    
    recov[doy, good_ones] = sum_across_ages(out, "R")$all + sum_across_ages(out, "VR")$all
    Reff[doy, good_ones] = out[,"Reff"]
    vax[doy, good_ones] = sum_across_ages(out,"Vtot")$all
    
    tests_1[doy, good_ones] = out[,"tot_tests_i1"]
    tests_2[doy, good_ones] = out[,"tot_tests_i2"]
    tests_3[doy, good_ones] = out[,"tot_tests_i3"]
    tests_4[doy, good_ones] = out[,"tot_tests_i4"]
    
    cases_1[doy, good_ones] = out[,"tot_diag_i1"]
    cases_2[doy, good_ones] = out[,"tot_diag_i2"]
    cases_3[doy, good_ones] = out[,"tot_diag_i3"]
    cases_4[doy, good_ones] = out[,"tot_diag_i4"]
    
    deaths_1[doy, good_ones] = out[,"tot_deaths_i1"]
    deaths_2[doy, good_ones] = out[,"tot_deaths_i2"]
    deaths_3[doy, good_ones] = out[,"tot_deaths_i3"]
    deaths_4[doy, good_ones] = out[,"tot_deaths_i4"]

    susc_1[doy, good_ones] = out[,"S_i1"] + out[,"VEX_i1"]
    susc_2[doy, good_ones] = out[,"S_i2"] + out[,"VEX_i2"]
    susc_3[doy, good_ones] = out[,"S_i3"] + out[,"VEX_i3"]
    susc_4[doy, good_ones] = out[,"S_i4"] + out[,"VEX_i4"]
    
    exp_1[doy, good_ones] = out[,"cum_exp_i1"] + out[,"Vcum_exp_i1"] + out[,"cum_exp2_i1"] + out[,"Vcum_exp2_i1"]
    exp_2[doy, good_ones] = out[,"cum_exp_i2"] + out[,"Vcum_exp_i2"] + out[,"cum_exp2_i2"] + out[,"Vcum_exp2_i2"]
    exp_3[doy, good_ones] = out[,"cum_exp_i3"] + out[,"Vcum_exp_i3"] + out[,"cum_exp2_i3"] + out[,"Vcum_exp2_i3"]
    exp_4[doy, good_ones] = out[,"cum_exp_i4"] + out[,"Vcum_exp_i4"] + out[,"cum_exp2_i4"] + out[,"Vcum_exp2_i4"]
    
    hosp_1[doy, good_ones] = out[,"H_i1"] + out[,"VH_i1"] + out[,"H2_i1"] + out[,"VH2_i1"]
    hosp_2[doy, good_ones] = out[,"H_i2"] + out[,"VH_i2"] + out[,"H2_i2"] + out[,"VH2_i2"]
    hosp_3[doy, good_ones] = out[,"H_i3"] + out[,"VH_i3"] + out[,"H2_i3"] + out[,"VH2_i3"]
    hosp_4[doy, good_ones] = out[,"H_i4"] + out[,"VH_i4"] + out[,"H2_i4"] + out[,"VH2_i4"]

    sd_1[doy, good_ones] = out[,"sd_adj_i1"]
    sd_2[doy, good_ones] = out[,"sd_adj_i2"]
    sd_3[doy, good_ones] = out[,"sd_adj_i3"]
    sd_4[doy, good_ones] = out[,"sd_adj_i4"]

    vac_1[doy, good_ones] = out[,"Vtot_i1"]
    vac_2[doy, good_ones] = out[,"Vtot_i2"]
    vac_3[doy, good_ones] = out[,"Vtot_i3"]
    vac_4[doy, good_ones] = out[,"Vtot_i4"]
  }
  
  inf = symp + asymp
  inf1 = symp1 + asymp1
  inf2 = symp2 + asymp2
  
  return(list(tests = tests[,1:good_ones],cases = cases[,1:good_ones], deaths = deaths[,1:good_ones],
		hosp = hosp[,1:good_ones], cum_hosp = cum_hosp[,1:good_ones],
		symp = symp[,1:good_ones], asymp = asymp[,1:good_ones],
		inf = inf[,1:good_ones], recov = recov[,1:good_ones], Reff=Reff[,1:good_ones],
		inf1 = inf1[,1:good_ones], inf2 = inf2[,1:good_ones],
		pop=pop[,1:good_ones], susc=susc[,1:good_ones], vax=vax[,1:good_ones],
		tests_1 = tests_1[,1:good_ones], tests_2 = tests_2[,1:good_ones],
		tests_3 = tests_3[,1:good_ones], tests_4 = tests_4[,1:good_ones],
		susc_1 = susc_1[,1:good_ones], susc_2 = susc_2[,1:good_ones],
		susc_3 = susc_3[,1:good_ones], susc_4 = susc_4[,1:good_ones],
		cases_1 = cases_1[,1:good_ones], cases_2 = cases_2[,1:good_ones],
		cases_3 = cases_3[,1:good_ones], cases_4 = cases_4[,1:good_ones],
		deaths_1 = deaths_1[,1:good_ones], deaths_2 = deaths_2[,1:good_ones],
		deaths_3 = deaths_3[,1:good_ones], deaths_4 = deaths_4[,1:good_ones],
		exp_1 = exp_1[,1:good_ones], exp_2 = exp_2[,1:good_ones],
		exp_3 = exp_3[,1:good_ones], exp_4 = exp_4[,1:good_ones],
		hosp_1 = hosp_1[,1:good_ones], hosp_2 = hosp_2[,1:good_ones],
		hosp_3 = hosp_3[,1:good_ones], hosp_4 = hosp_4[,1:good_ones],
		sd_1 = sd_1[,1:good_ones], sd_2 = sd_2[,1:good_ones],
		sd_3 = sd_3[,1:good_ones], sd_4 = sd_4[,1:good_ones],
		vac_1 = vac_1[,1:good_ones], vac_2 = vac_2[,1:good_ones],
		vac_3 = vac_3[,1:good_ones], vac_4 = vac_4[,1:good_ones],
		doy = calendar_time))
}

get_model_data_param_sets_region = function(pars_matrix, pars_names, pars_fixed, region, daycount, state)
{
  # first do the fixed parameters
  region_par_idx = startsWith(names(pars_fixed), region)
  pars_to_set = gsub(".*\\.", "", names(pars_fixed)[region_par_idx]) # get part after .
  parameters = get_params(unlist(pars_fixed[region_par_idx]), pars_to_set, pars_fixed)
  
  # now update the names for the parameter matrix
  pars_names = gsub(".*\\.", "", pars_names)

  get_model_data_param_sets(pars_matrix, pars_names, parameters, daycount, state)
}
