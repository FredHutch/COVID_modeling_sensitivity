#
require(lubridate)

#' Gets parameters
#' 
#' This is a helper function since the optimizer just uses an unnamed vector of parameters.
#' We pass an additional vector of the names of the parameters, that can be used to determine
#' what parameters are in the pars vector and this overwrite the correct parameter in the 
#' parameter list pars_base.
#'
#' @param pars Vector of parmater, e.g. from the optimizer
#' @param pars_names Names of the paramters in pars
#' @param pars_base Base parameter values, to be updated with those in pars
#'
#' @return List of parameters suitable for calling model_eq
get_params = function(pars, pars_names, pars_base)
{
  # pars is a vector of parameters, need to convert these to list 
  # make the parameter setting more flexible by over-writing the base parameters,
  # so that it's easier to vary which ones are fit
  for (i in 1:length(pars))
  {
    param_name = pars_names[i]
    param_value = pars[i]
    
    # this needs to be an integer number of days
    if (param_name == "delta0offset")
    {
      param_value = floor(param_value)
    }
    # handle setting a common value for age-spacific parameters
    if (param_name %in% c("d2", "h","sd2"))
    {
      param_name = paste(param_name, 1:4, sep = "_")
    }
    pars_base[ param_name ] = param_value
  }

  return(pars_base)
}

get_region_params = function(pars, pars_names, parameters, region_name)
{
  # because parameters have to be passed as a single vector for optimization, we find region parameters
  # according to the specification regionname-parametername
  # note that this needs to be done for both calibrated and fixed parameters

  # first get the fixed parameters
  region_par_idx = startsWith(names(parameters), region_name)
  pars_to_set = gsub(".*\\.", "", names(parameters)[region_par_idx]) # get part after .
  parameters = get_params(unlist(parameters[region_par_idx]), pars_to_set, parameters)
  
  # now the calibrated ones in pars
  region_par_idx = startsWith(pars_names, region_name)
  pars_to_set = gsub(".*\\.", "", pars_names[region_par_idx]) # get part after .
  parameters = get_params(pars[region_par_idx], pars_to_set, parameters)
  
  return(parameters)
}
avg_weekly = function(out_data, col_name)
{
  # use 3 days before & after this day for 7-day avg calc
  avg_data = vector (length=nrow(out_data))
  for (i in 1:nrow(out_data)) {
        avg_count=0;
        n = 0;
        for (j in 0:6 ) {
            if (i+3-j > 0 && (i+3-j) <= nrow(out_data)) {
                avg_count = avg_count + out_data[(i+3-j),col_name]
                n = n+1
            }
        }
        if (n > 0) {
            avg_data[i] = avg_count / n
        } else {
            avg_data[i] = 0
        }
   }
  return(avg_data)
}
avg_weekly_w_delta = function(out_data, col_name)
{
  avg_data = vector (length=nrow(out_data))
  for (i in 1:nrow(out_data)) {
        avg_count=0;
        n = 0;
        for (j in 0:6 ) {
            if ((i+3-j-1) > 0 && (i+3-j) <= nrow(out_data)) {
                avg_count = avg_count + (out_data[(i+3-j),col_name] - out_data[(i+3-j-1),col_name])
                n = n+1
            }
        }
        if (n > 0) {
            avg_data[i] = avg_count / n
        } else {
            avg_data[i] = 0
        }
   }
  return(avg_data)
}
create_data_start_day0 = function(composite_data, first_case_doy, delta0offset)
{
  # Offset by delta0offset days to align timeline to model, that is how many days are there
  # between day0 of the model and the first detected case
  # doy give the calendar day of year, i.e. feb 28 is doy 59

  # check for problem between parameter and data
  # we now have deaths before cases in new data
  # stopifnot(yday(composite_data$date[1]) == first_case_doy) 
  delta0offset = floor(delta0offset)
  
  day0_doy = first_case_doy - delta0offset
  data_from_day0 = data.frame(time = 0:(delta0offset + nrow(composite_data) - 1), 
                              doy = day0_doy:(day0_doy + delta0offset + nrow(composite_data) - 1),
                              tests1 = 0, pos1 = 0, deaths1 = 0, hosps1 = 0,hosp_deaths1 = 0,
                              tests2 = 0, pos2 = 0, deaths2 = 0, hosps2 = 0,hosp_deaths2 = 0,
                              tests3 = 0, pos3 = 0, deaths3 = 0, hosps3 = 0,hosp_deaths3 = 0,
                              tests4 = 0, pos4 = 0, deaths4 = 0, hosps4 = 0,hosp_deaths4 = 0,
                              tests = 0, cases = 0, deaths = 0,Hospitalizations=0)

  data_from_day0$tests[(delta0offset + 1):nrow(data_from_day0)] = composite_data$tests
  data_from_day0$cases[(delta0offset + 1):nrow(data_from_day0)] = composite_data$cases
  data_from_day0$deaths[(delta0offset + 1):nrow(data_from_day0)] = composite_data$deaths
  data_from_day0$hosp_deaths[(delta0offset + 1):nrow(data_from_day0)] = composite_data$hosp_deaths
  data_from_day0$Hospitalizations[(delta0offset + 1):nrow(data_from_day0)] = composite_data$Hospitalizations

  data_from_day0$tests1[(delta0offset + 1):nrow(data_from_day0)] = composite_data$tests1
  data_from_day0$tests2[(delta0offset + 1):nrow(data_from_day0)] = composite_data$tests2
  data_from_day0$tests3[(delta0offset + 1):nrow(data_from_day0)] = composite_data$tests3
  data_from_day0$tests4[(delta0offset + 1):nrow(data_from_day0)] = composite_data$tests4
  data_from_day0$pos1[(delta0offset + 1):nrow(data_from_day0)] = composite_data$pos1
  data_from_day0$pos2[(delta0offset + 1):nrow(data_from_day0)] = composite_data$pos2
  data_from_day0$pos3[(delta0offset + 1):nrow(data_from_day0)] = composite_data$pos3
  data_from_day0$pos4[(delta0offset + 1):nrow(data_from_day0)] = composite_data$pos4
  data_from_day0$deaths1[(delta0offset + 1):nrow(data_from_day0)] = composite_data$deaths1
  data_from_day0$deaths2[(delta0offset + 1):nrow(data_from_day0)] = composite_data$deaths2
  data_from_day0$deaths3[(delta0offset + 1):nrow(data_from_day0)] = composite_data$deaths3
  data_from_day0$deaths4[(delta0offset + 1):nrow(data_from_day0)] = composite_data$deaths4
  data_from_day0$hosps1[(delta0offset + 1):nrow(data_from_day0)] = composite_data$hosps1
  data_from_day0$hosps2[(delta0offset + 1):nrow(data_from_day0)] = composite_data$hosps2
  data_from_day0$hosps3[(delta0offset + 1):nrow(data_from_day0)] = composite_data$hosps3
  data_from_day0$hosps4[(delta0offset + 1):nrow(data_from_day0)] = composite_data$hosps4
  data_from_day0$hosp_deaths1[(delta0offset + 1):nrow(data_from_day0)] = composite_data$hosp_deaths1
  data_from_day0$hosp_deaths2[(delta0offset + 1):nrow(data_from_day0)] = composite_data$hosp_deaths2
  data_from_day0$hosp_deaths3[(delta0offset + 1):nrow(data_from_day0)] = composite_data$hosp_deaths3
  data_from_day0$hosp_deaths4[(delta0offset + 1):nrow(data_from_day0)] = composite_data$hosp_deaths4
  
  return(data_from_day0)
}


calc_sse_multi = function(pars, pars_names, pars_fixed, 
                          cum_cases, case_death_by_age_at_time)
{
  parameters = get_params(pars, pars_names, pars_fixed)

  sd2 = c(parameters$sd2_1,parameters$sd2_2,parameters$sd2_3,parameters$sd2_4)

  # get actual data in model time (so can fit how many days before first detected case)
  model_t_from_doy_factor = parameters$delta0offset - parameters$first_case_doy # add this to doy to get model time
  cum_cases = create_data_start_day0(cum_cases, parameters$first_case_doy, parameters$delta0offset)
 
  out = ode(y = state, times = cum_cases$time, func = model_eq, parms = parameters, method = "euler", hini = ode_step_time, maxsteps=10000)
  
  error = rep(0, 16) # errors for daily teste, cases, deaths and hospital admissions by age group of each and smoothed hospitalizations
  scores = rep(0, 4) # scores for daily teste, cases, deaths and hospital admissions (aggregated)

  varnce = rep(0, 16) # cases, deaths, and age distributions of each, hospitalizations
  varnce[1] = var(cum_cases$pos1)
  varnce[2] = var(cum_cases$pos2)
  varnce[3] = var(cum_cases$pos3)
  varnce[4] = var(cum_cases$pos4)
  varnce[5] = var(cum_cases$deaths1)
  varnce[6] = var(cum_cases$deaths2)
  varnce[7] = var(cum_cases$deaths3)
  varnce[8] = var(cum_cases$deaths4)
  varnce[9] = var(cum_cases$hosps1)
  varnce[10] = var(cum_cases$hosps2)
  varnce[11] = var(cum_cases$hosps3)
  varnce[12] = var(cum_cases$hosps4)
  varnce[13] = var(cum_cases$tests1)
  varnce[14] = var(cum_cases$tests2)
  varnce[15] = var(cum_cases$tests3)
  varnce[16] = var(cum_cases$tests4)

  #prevent division by zero below
  for (cat in 1:16)
    if (varnce[cat]==0)
	varnce[cat]=1

  out_cases = as.data.frame(out)

  #add 7-day running avg to positive tests as "smoothed_cases<n>" where n is agegroup
  scores[1] = 0
  for (age in 1:4)
  {
    smoothed = avg_weekly(cum_cases,paste0("pos",age))
    old_names = names(cum_cases)
    cum_cases = cbind(cum_cases,smoothed)
    names(cum_cases) = c(old_names,paste0("smoothed_cases",age))

    smoothed = avg_weekly_w_delta(out_cases,paste0("tot_diag_i",age))
    old_names = names(out_cases)
    out_cases = cbind(out_cases,as.numeric(smoothed))
    names(out_cases) = c(old_names,paste0("smoothed_cases",age))

    if (age == 1)
    {
	error[1] = sum((cum_cases$smoothed_cases1 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_cases1))^2)
	scores[1] = sqrt((error[1]/length(cum_cases$time))/varnce[1])
    } 
    else if (age == 2)
    {
	error[2] = sum((cum_cases$smoothed_cases2 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_cases2))^2)
	scores[1] = scores[1] + sqrt((error[2]/length(cum_cases$time))/varnce[2])
    } 
    else if (age == 3)
    {
	error[3] = sum((cum_cases$smoothed_cases3 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_cases3))^2)
	scores[1] = scores[1] + sqrt((error[3]/length(cum_cases$time))/varnce[3])
    } 
    else 
    {
	error[4] = sum((cum_cases$smoothed_cases4 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_cases4))^2)
	scores[1] = scores[1] + sqrt((error[4]/length(cum_cases$time))/varnce[4])
    } 
  }

  #add 7-day running avg to hosp_deaths as "smoothed_deaths<n>" where n is agegroup
  scores[2]=0
  for (age in 1:4)
  {
    #smoothed = avg_weekly(cum_cases,paste0("hosp_deaths",age))
    smoothed = avg_weekly(cum_cases,paste0("deaths",age))
    old_names = names(cum_cases)
    cum_cases = cbind(cum_cases,smoothed)
    names(cum_cases) = c(old_names,paste0("smoothed_deaths",age))

    smoothed = avg_weekly_w_delta(out_cases,paste0("tot_deaths_i",age))
    old_names = names(out_cases)
    out_cases = cbind(out_cases,as.numeric(smoothed))
    names(out_cases) = c(old_names,paste0("smoothed_deaths",age))

    if (age == 1)
    {
	error[5] = sum((cum_cases$smoothed_deaths1 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_deaths1))^2)
	scores[2] = sqrt(error[5]/length(cum_cases$time)) # no variance as deaths=0
    } 
    else if (age == 2)
    {
	error[6] = sum((cum_cases$smoothed_deaths2 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_deaths2))^2)
	scores[2] = scores[2] + sqrt((error[6]/length(cum_cases$time))/varnce[6])
    }
    else if (age == 3)
    {
	error[7] = sum((cum_cases$smoothed_deaths3 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_deaths3))^2)
	scores[2] = scores[2] + sqrt((error[7]/length(cum_cases$time))/varnce[7])
    }
    else 
    {
	error[8] = sum((cum_cases$smoothed_deaths4 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_deaths4))^2)
	scores[2] = scores[2] + sqrt((error[8]/length(cum_cases$time))/varnce[8])
    }
  }

  #add 7-day running avg to hosp admits as "smoothed_hosps<n>" where n is agegroup
  scores[3]=0
  for (age in 1:4)
  {
    smoothed = avg_weekly(cum_cases,paste0("hosps",age))
    old_names = names(cum_cases)
    cum_cases = cbind(cum_cases,smoothed)
    names(cum_cases) = c(old_names,paste0("smoothed_hosps",age))

    smoothed = avg_weekly_w_delta(out_cases,paste0("tot_hosp_i",age))
    old_names = names(out_cases)
    out_cases = cbind(out_cases,as.numeric(smoothed))
    names(out_cases) = c(old_names,paste0("smoothed_hosps",age))

    if (age == 1)
    {
	error[9] = sum((cum_cases$smoothed_hosps1 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_hosps1))^2)
	scores[3] = sqrt((error[9]/length(cum_cases$time))/varnce[9])
    } 
    else if (age == 2)
    {
	error[10] = sum((cum_cases$smoothed_hosps2 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_hosps2))^2)
	scores[3] = scores[3] + sqrt((error[10]/length(cum_cases$time))/varnce[10])
    } 
    else if (age == 3)
    {
	error[11] = sum((cum_cases$smoothed_hosps3 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_hosps3))^2)
	scores[3] = scores[3] + sqrt((error[11]/length(cum_cases$time))/varnce[11])
    } 
    else 
    {
	error[12] = sum((cum_cases$smoothed_hosps4 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_hosps4))^2)
	scores[3] = scores[3] + sqrt((error[12]/length(cum_cases$time))/varnce[12])
    } 
  }

  #add 7-day running avg to all tests as "smoothed_tests<n>" where n is agegroup
  scores[4] = 0
  for (age in 1:4)
  {
    smoothed = avg_weekly(cum_cases,paste0("tests",age))
    old_names = names(cum_cases)
    cum_cases = cbind(cum_cases,smoothed)
    names(cum_cases) = c(old_names,paste0("smoothed_tests",age))

    smoothed = avg_weekly_w_delta(out_cases,paste0("tot_tests_i",age))
    old_names = names(out_cases)
    out_cases = cbind(out_cases,as.numeric(smoothed))
    names(out_cases) = c(old_names,paste0("smoothed_tests",age))

    if (age == 1)
    {
	error[13] = sum((cum_cases$smoothed_tests1 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_tests1))^2)
	scores[4] = sqrt((error[13]/length(cum_cases$time))/varnce[13])
    } 
    else if (age == 2)
    {
	error[14] = sum((cum_cases$smoothed_tests2 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_tests2))^2)
	scores[4] = scores[4] + sqrt((error[14]/length(cum_cases$time))/varnce[14])
    } 
    else if (age == 3)
    {
	error[15] = sum((cum_cases$smoothed_tests3 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_tests3))^2)
	scores[4] = scores[4] + sqrt((error[15]/length(cum_cases$time))/varnce[15])
    } 
    else 
    {
	error[16] = sum((cum_cases$smoothed_tests4 - out_cases %>% filter(time %in% cum_cases$time) %>% select(smoothed_tests4))^2)
	scores[4] = scores[4] + sqrt((error[16]/length(cum_cases$time))/varnce[16])
    } 
  }

  #print(paste("daily case scores:",scores[1]))
  #print(paste("daily death scores:",scores[2]))
  #print(paste("daily hosp scores:",scores[3]))
  #print(paste("daily test scores:",scores[4]))

  names(scores) = c("daily cases", "daily deaths", "daily hosp", "daily tests")
  return(scores)
}

calc_sse_multiregions = function(pars, pars_names, pars_base, region_names,
                             cum_cases)
{
  # for now error is cases, deaths, peak * num regions
  error = NULL
  parameters = get_params(pars, pars_names, pars_base)
  
  for (region in region_names)
  {
    # first apply any region specific parameters
    param_region = get_region_params(pars, pars_names, parameters, region)
    error_region = rep(0, 2) # cases, deaths, peak
    
    # get actual data in model time (so can fit how many days before first detected case)
    model_t_from_doy_factor = param_region$delta0offset - param_region$first_case_doy # add this to doy to get model time
    cum_cases_region = create_data_start_day0(cum_cases[[region]], param_region$first_case_doy, param_region$delta0offset)
    
    out = ode(y = state[[region]], times = cum_cases_region$time, func = model_eq, parms = param_region, method = "euler", hini = ode_step_time, maxsteps=10000)
    
    # cases
    out_cases = sum_across_ages(out, "tot_diag")
    error_region[1] = sum((cum_cases_region$cases - out_cases %>% filter(time %in% cum_cases_region$time) %>% select(all))^2)
    
    # deaths, when we have them
    out_deaths = sum_across_ages(out, "tot_deaths")
    error_region[2] = sum((cum_cases_region$deaths - out_deaths %>% filter(time %in% cum_cases_region$time) %>% select(all))^2)
    
    error = c(error, error_region)
  }
  
  names(error) = paste(rep(region_names, each = 3), c("cases", "deaths", "peak"))
  return(error)
}

calc_sse_cases = function(pars, pars_names, pars_fixed, 
                           cum_cases, case_death_by_age_at_time)
{
  sse = calc_sse_multi(pars, pars_names, pars_fixed, cum_cases, case_death_by_age_at_time)

  score =sse[1] #+sse[4]

  return(score)
}

calc_sse_simple = function(pars, pars_names, pars_fixed, 
                           cum_cases, case_death_by_age_at_time)
{
  sse = calc_sse_multi(pars, pars_names, pars_fixed, cum_cases, case_death_by_age_at_time)

  score =sse[1]
  score =score + sse[2]
  score =score + sse[3]
  #score =score + sse[4]

  return(score)
}

calc_sse_cases_peak = function(pars, pars_names, pars_fixed, 
                               cum_cases, case_death_by_age_at_time)
{
  sse = calc_sse_multi(pars, pars_names, pars_fixed, cum_cases, case_death_by_age_at_time)
  return(sse[c("cases", "peak")])
}

calc_sse_cases_deaths_peak = function(pars, pars_names, pars_fixed, 
                               cum_cases, case_death_by_age_at_time)
{
  sse = calc_sse_multi(pars, pars_names, pars_fixed, cum_cases, case_death_by_age_at_time)
  return(sse[c("cases", "deaths", "peak")])
}

calc_sse_age_dist = function(pars, pars_names, pars_fixed, 
                               cum_cases, case_death_by_age_at_time)
{
  sse = calc_sse_multi(pars, pars_names, pars_fixed, cum_cases, case_death_by_age_at_time)
  return(sse[c("age (c)", "age (d)")])
}

get_dates_from_model_time = function(delta0offset, delta1, delta2offset, date_first_case)
{
  rows = c("day0", "first case", "start sd", "strict sd")
  day0 = date_first_case - delta0offset
  model_time = c(0, delta0offset, delta1, delta1 + delta2offset)
  date = day0 + model_time
  doy = yday(date)
  
  dates = data.frame(model_time, date, doy, row.names = rows)
  return(dates)
}

get_model_day = function(doy, delta0offset, first_case_doy)
{
  day0_doy = first_case_doy - delta0offset
  model_day = doy - day0_doy
  return(model_day)
}

sum_across_ages = function(out_data, col_start)
{
  out_data = as.data.frame(out_data)
  out_sums = data.frame(time = out_data$time, all = out_data %>% 
                           select(starts_with(col_start)) %>% rowSums() )
  return(out_sums)
}


meet_criteria = function(pars, pars_names, pars_fixed, 
                         cum_cases, case_death_by_age_at_time)
{
  scores = calc_sse_multi(pars, pars_names, pars_fixed, 
                          cum_cases, case_death_by_age_at_time)
  
  score1=scores[1]
  score2=scores[2]
  score3=scores[3]
  score4=scores[4]
  score = score1+score2+score3+score4

  result = score < 5

  return(result)
}

sample_param_space_inc = function(samples, lower_bounds, upper_bounds, pars_names, pars_fixed, 
                              cum_cases, case_death_by_age_at_time, samples_to_find = 100,outfile)
{
  good_sets = rep(FALSE, samples)
  param_sets = sapply(1:length(upper_bounds), function(x) runif(samples, min = lower_bounds[x], max = upper_bounds[x]))

  for (i in 1:samples)
  {
    scores = calc_sse_multi(param_sets[i,], pars_names, pars_fixed, cum_cases, case_death_by_age_at_time)

    score1=scores[1]
    score2=scores[2]
    score3=scores[3]
    score4=scores[4]
    score = score1+score2+score3+score4

    result = score < 5

    if (result)
    {
      good_sets[i] = TRUE
      line = i
      for (j in 1:length(param_sets[i,]))
      {
	line = paste(line,param_sets[i,j],sep=",")
      }
      line = paste(line,score1,sep=",")

      line = paste(line,score2,sep=",")

      line = paste(line,score3,sep=",")

      line = paste(line,score,sep=",")

      write(line,file=outfile,append=TRUE)
    }
    if (sum(good_sets) >= samples_to_find )
    {
      print(paste("Returning",sum(good_sets),"good parameter sets in",i,"trials"))
      break
    }
  }
  return(param_sets[which(good_sets),])
}

sample_param_space = function(samples, lower_bounds, upper_bounds, pars_names, pars_fixed, 
                              cum_cases, case_death_by_age_at_time,samples_to_find = 100)
{
  good_sets = rep(FALSE, samples)
  param_sets = sapply(1:length(upper_bounds), function(x) runif(samples, min = lower_bounds[x], max = upper_bounds[x]))

  for (i in 1:samples)
  {
    if (meet_criteria(param_sets[i,], pars_names, pars_fixed, cum_cases, case_death_by_age_at_time))
    {
      good_sets[i] = TRUE
    }
    if (sum(good_sets) >= samples_to_find )
    {
      print(paste("Returning",sum(good_sets),"good parameter sets in",i,"trials"))
      break
    }
  }
  return(param_sets[which(good_sets),])
}
