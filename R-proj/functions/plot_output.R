require(ggplot2)
require(gridExtra)

plot_agegrouptotals <- function(data_out, popscale = 1)
{
  ggplot(data_out, aes(time, value/popscale, col = factor(age))) + 
    geom_line() + 
    facet_wrap(~state, scales = "free") + 
    theme_classic()+ 
    scale_color_discrete(name="Age group",
                         breaks=c("1", "2", "3", "4"),
                         labels=c("0-19", "20-49", "50-69", "70+")) +
    ylab("population in each state")
}

plot_statestack <- function(data_out, droplist="")
{
  ggplot(filter(data_out, state != droplist),
         aes(time, value, col = state)) + 
    geom_bar(position = "fill", stat = "identity") + 
    facet_wrap(~age, 
               scales = "free", 
               labeller=labeller(age = c("1" = "0-19", "2" = "20-49", "3" = "50-69", "4" = "70+"))) + 
    theme_minimal() + 
    ylab("% of population in each state") +
    scale_color_paletteer_d("awtools::a_palette")
}

plot_case_calibration = function(plotdata, plot_together = FALSE){
  
  plotdata = plotdata %>% group_by(time) %>%
    summarise(
      cases_mod = sum(cases_base),
      cases = first(cases),
      date = first(date)
    )
  
  c = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = cases_mod)) +
    geom_point(aes(y = cases), col = "red", pch = 1) +
    theme_classic() +
    ylab("Diagnosed Cases")

  return(c)
}
plot_inf_calibration = function(plotdata, plot_together = FALSE){
  
  plotdata = plotdata %>% group_by(time) %>%
    summarise(
      infs_mod = sum(infs_base),
      date = first(date)
    )
  
  c = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = infs_mod)) +
    theme_classic() +
    ylab("Cumulative Infections")

  return(c)
}
plot_death_calibration = function(plotdata, plot_together = FALSE){
  
  plotdata = plotdata %>% group_by(time) %>%
    summarise(
      deaths_mod = sum(deaths_base),
      #hosp_deaths = first(hosp_deaths),
      deaths = first(deaths),
      date = first(date)
    )
  
  d = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = deaths_mod)) +
    #geom_point(aes(y = hosp_deaths), col = "red", pch = 1) +
    geom_point(aes(y = deaths), col = "red", pch = 1) +
    theme_classic() +
    #ylab("Hospital Deaths")
    ylab("Diagnosed Deaths")
  return(d)
}
plot_calibration = function(plotdata, plot_together = FALSE){
  
  plotdata = plotdata %>% group_by(time) %>%
    summarise(
      cases_mod = sum(cases_base),
      deaths_mod = sum(deaths_base),
      cases = first(cases),
      #hosp_deaths = first(hosp_deaths),
      deaths = first(deaths),
      date = first(date)
    )
  
  c = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = cases_mod)) +
    geom_point(aes(y = cases), col = "red", pch = 1) +
    theme_classic() +
    ylab("Diagnosed Cases")

  d = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = deaths_mod)) +
    #geom_point(aes(y = hosp_deaths), col = "red", pch = 1) +
    geom_point(aes(y = deaths), col = "red", pch = 1) +
    theme_classic() +
    #ylab("Hospital Deaths")
    ylab("Diagnosed Deaths")
  
  p = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = cases_mod)) +
    geom_point(aes(y = cases), col = "red", pch = 1) +
    geom_line(aes(y = deaths_mod)) +
    geom_point(aes(y = deaths), col = "red", pch = 3) +
    theme_classic() +
    ylab("Diagnosed Cases / Deaths")
  
  if (plot_together)
  { return(p) }
  else { return(grid.arrange(c, d, ncol=2)) }
}

plot_vac_calibration = function(plotdata, plot_together = FALSE){
  
  plotdata = plotdata %>% group_by(time) %>%
    summarise(
      cases_vac = sum(vac_cases_base),
      deaths_vac = sum(vac_deaths_base),
      cases = sum(cases_base),
      deaths = sum(deaths_base),
      date = first(date)
    )
  
  e = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = cases)) +
    geom_line(aes(y = cases_vac),col = "red") +
    theme_classic() +
    ylab("unvac/vac cases")

  f = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = deaths)) +
    geom_line(aes(y = deaths_vac),col = "red") +
    theme_classic() +
    ylab("unvac/vac deaths")
  
  p = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = cases_vac)) +
    geom_point(aes(y = cases), col = "red", pch = 1) +
    geom_line(aes(y = deaths_vac)) +
    geom_point(aes(y = deaths), col = "red", pch = 3) +
    theme_classic() +
    ylab("diagnosed vac cases / deaths")
  
  if (plot_together)
  { return(p) }
  else { return(grid.arrange(e, f, ncol=2)) }
}

plot_calibration_daily_case = function(plotdata, plot_together = FALSE){
  
  plotdata = plotdata %>% group_by(time) %>%
    summarise(
      cases_mod = sum(cases_base),
      cases = first(cases),
      date = first(date)
    )
  
  plotdata = plotdata %>% 
    mutate(newcases = cases - lag(cases),
           newcases_mod = cases_mod - lag(cases_mod))
           
  c = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = newcases_mod)) +
    geom_point(aes(y = newcases), col = "red", pch = 1) +
#    ylim(0, min(c(max(plotdata$newcases), max(plotdata$newcases_mod)))) +
    theme_classic() + 
    ylab("Daily Cases")

  return(c)
}

plot_calibration_daily_death = function(plotdata, plot_together = FALSE){
  
  plotdata = plotdata %>% group_by(time) %>%
    summarise(
      deaths_mod = sum(deaths_base),
      #hosp_deaths = first(hosp_deaths),
      deaths = first(deaths),
      date = first(date)
    )
  
  plotdata = plotdata %>% 
    mutate(
           newdeaths = deaths - lag(deaths),
           newdeaths_mod = deaths_mod - lag(deaths_mod))
           
  d = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = newdeaths_mod)) +
    geom_point(aes(y = newdeaths), col = "red", pch = 1) +
 #   ylim(0, min(c(max(plotdata$newdeaths), max(plotdata$newdeaths_mod)))) +
    theme_classic() + 
    ylab("Daily Deaths")
  return(d)
}

plot_calibration_daily = function(plotdata, plot_together = FALSE){
  
  plotdata = plotdata %>% group_by(time) %>%
    summarise(
      cases_mod = sum(cases_base),
      deaths_mod = sum(deaths_base),
      cases = first(cases),
      #hosp_deaths = first(hosp_deaths),
      deaths = first(deaths),
      date = first(date)
    )
  
  plotdata = plotdata %>% 
    mutate(newcases = cases - lag(cases),
           #newdeaths = hosp_deaths - lag(hosp_deaths),
           newdeaths = deaths - lag(deaths),
           newcases_mod = cases_mod - lag(cases_mod),
           newdeaths_mod = deaths_mod - lag(deaths_mod))
           
  c = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = newcases_mod)) +
    geom_point(aes(y = newcases), col = "red", pch = 1) +
#    ylim(0, min(c(max(plotdata$newcases), max(plotdata$newcases_mod)))) +
    theme_classic() + 
    ylab("Daily Cases")

  d = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = newdeaths_mod)) +
    geom_point(aes(y = newdeaths), col = "red", pch = 1) +
 #   ylim(0, min(c(max(plotdata$newdeaths), max(plotdata$newdeaths_mod)))) +
    theme_classic() + 
    ylab("Daily Deaths")
  
  p = ggplot(plotdata, aes(x = time)) +
    geom_line(aes(y = newcases_mod)) +
    geom_point(aes(y = newcases), col = "red", pch = 1) +
    geom_line(aes(y = newdeaths_mod)) +
    geom_point(aes(y = newdeaths), col = "red", pch = 3) +
    theme_classic() + 
    ylab("Daily Diagnosed Cases / Death")
  
  if (plot_together)
  { return(p) }
  else { return(grid.arrange(c, d, ncol=2)) }
}

plot_calibration_hosp = function(plotdata){
  
  plotdata = plotdata %>% group_by(time) %>%
    summarise(
      hosps_mod = sum(hosp_base),
      hosps = first(Hospitalizations),
      date = first(date)
    )
  
  plotdata = plotdata %>% 
    mutate(newhosps = hosps,
           newhosps_mod = hosps_mod - lag(hosps_mod))
           
  c = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = newhosps_mod)) +
    geom_point(aes(y = newhosps), col = "red", pch = 1) +
    theme_classic() + 
    ylab("Daily Hospital Admissions")

  return(c)
}

plot_calibration_tests = function(plotdata){
  
  plotdata = plotdata %>% group_by(time) %>%
    summarise(
      tests_mod = sum(test_base),
      tests = first(tests),
      date = first(date)
    )
  
  plotdata = plotdata %>% 
    mutate(newtests = tests,
           newtests_mod = tests_mod - lag(tests_mod))
           
  c = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = newtests_mod)) +
    geom_point(aes(y = newtests), col = "red", pch = 1) +
    theme_classic() + 
    ylab("Daily Tests")

  return(c)
}

plot_calibration_reff = function(calibage){
  
  plotdata = calibage %>% group_by(time) %>%
    summarise(
      reff_mod = sum(reff_base),
      date = first(date)
    )
  
  c = ggplot(plotdata, aes(x = date)) +
    geom_line(aes(y = reff_mod)) +
    theme_classic() + 
    ylab("R effective")

  return(c)
}

plot_calibration_byage = function(calibage){
  cases_age = calibage %>% select(age, model_cases, data_cases) %>% melt(id.vars = "age")
  deaths_age = calibage %>% select(age, model_deaths, data_deaths) %>% melt(id.vars = "age")
  
  c <- ggplot(cases_age, aes(x = age, y = value, fill = variable)) +
      geom_bar(stat='identity', position='dodge') +
      theme_classic() + 
      scale_fill_paletteer_d("awtools::gpalette") +
      ylab("cases")
  
  d <- ggplot(deaths_age, aes(x = age, y = value, fill = variable)) +
      geom_bar(stat='identity', position='dodge') +
      theme_classic() + 
      scale_fill_paletteer_d("awtools::gpalette") +
      ylab("deaths")

grid.arrange(c, d, ncol=2)

}

plot_case_calibration_params = function(pars, pars_names, pars_fixed, kc_data, kc_age, daycount, state)
{
  out <- get_model_data(pars, pars_names, pars_fixed, daycount, state)
  
  parameters = get_params(pars, pars_names, pars_fixed)
  cum_cases = create_data_start_day0(kc_data, parameters$first_case_doy, parameters$delta0offset)

  data_out <- shape_data(out)
  plotdata <- transform_data_calib_plot(data_out, cum_cases)
  plot_case_calibration(plotdata)
}
plot_inf_calibration_params = function(pars, pars_names, pars_fixed, kc_data, kc_age, daycount, state)
{
  out <- get_model_data(pars, pars_names, pars_fixed, daycount, state)
  
  parameters = get_params(pars, pars_names, pars_fixed)
  cum_cases = create_data_start_day0(kc_data, parameters$first_case_doy, parameters$delta0offset)

  data_out <- shape_data(out)
  plotdata <- transform_data_calib_inf_plot(data_out, cum_cases)

  plot_inf_calibration(plotdata)
}
plot_death_calibration_params = function(pars, pars_names, pars_fixed, kc_data, kc_age, daycount, state)
{
  out <- get_model_data(pars, pars_names, pars_fixed, daycount, state)
  
  parameters = get_params(pars, pars_names, pars_fixed)
  cum_cases = create_data_start_day0(kc_data, parameters$first_case_doy, parameters$delta0offset)

  data_out <- shape_data(out)
  plotdata <- transform_data_calib_plot(data_out, cum_cases)
  plot_death_calibration(plotdata)
}
plot_hosp_calibration_params = function(pars, pars_names, pars_fixed, kc_data, kc_age, daycount, state)
{
  out <- get_model_data(pars, pars_names, pars_fixed, daycount, state)
  
  parameters = get_params(pars, pars_names, pars_fixed)
  cum_cases = create_data_start_day0(kc_data, parameters$first_case_doy, parameters$delta0offset)

  data_out <- shape_data(out)
  calibage2 = transform_data_calib_hosp_plot(data_out, cum_cases)
  plot_calibration_hosp(calibage2)
}
plot_daily_case_calibration_params = function(pars, pars_names, pars_fixed, kc_data, kc_age, daycount, state)
{
  out <- get_model_data(pars, pars_names, pars_fixed, daycount, state)
  
  parameters = get_params(pars, pars_names, pars_fixed)
  cum_cases = create_data_start_day0(kc_data, parameters$first_case_doy, parameters$delta0offset)

  data_out <- shape_data(out)
  plotdata <- transform_data_calib_plot(data_out, cum_cases)
  plot_calibration_daily_case(plotdata)
}
plot_daily_death_calibration_params = function(pars, pars_names, pars_fixed, kc_data, kc_age, daycount, state)
{
  out <- get_model_data(pars, pars_names, pars_fixed, daycount, state)
  
  parameters = get_params(pars, pars_names, pars_fixed)
  cum_cases = create_data_start_day0(kc_data, parameters$first_case_doy, parameters$delta0offset)

  data_out <- shape_data(out)
  plotdata <- transform_data_calib_plot(data_out, cum_cases)
  plot_calibration_daily_death(plotdata)
}
plot_calibration_params = function(pars, pars_names, pars_fixed, kc_data, kc_age, daycount, state)
{
  out <- get_model_data(pars, pars_names, pars_fixed, daycount, state)
  
  parameters = get_params(pars, pars_names, pars_fixed)
  cum_cases = create_data_start_day0(kc_data, parameters$first_case_doy, parameters$delta0offset)

  data_out <- shape_data(out)
  
  plotdata <- transform_data_calib_plot(data_out, cum_cases)
  plot_calibration(plotdata)
  plot_calibration_daily(plotdata)

  #calibage = transform_data_calib_age_plot(plotdata, kc_age)
  #plot_calibration_byage(calibage)

  #plotdata <- transform_vac_data_calib_plot(data_out, cum_cases)
  #plot_vac_calibration(plotdata)
  
  calibage2 = transform_data_calib_hosp_plot(data_out, cum_cases)
  plot_calibration_hosp(calibage2)
  
  calibage3 = transform_data_calib_test_plot(data_out, cum_cases)
  plot_calibration_tests(calibage3)
  
  #calibage3 = transform_data_calib_inf_plot(data_out, cum_cases)
  #plot_calibration_infs(calibage3)
  
  #age_cols = paletteer_d("RColorBrewer::YlOrRd", 6)[-(1:2)]
  #plot_sd3(out, age_cols)
}

plot_age_test_fit = function(pars, pars_names, pars_fixed, kc_data, kc_age, daycount, state)
{
  #out <- get_model_data(pars, pars_names, pars_fixed, daycount, state)

  parameters = get_params(pars, pars_names, pars_fixed)
  cum_cases = create_data_start_day0(kc_data, parameters$first_case_doy, parameters$delta0offset)

  out = ode(y = state, times = cum_cases$time, func = model_eq, parms = parameters)
  out_cases = as.data.frame(out)

  print(paste("records=",length(cum_cases$doy),"days=",daycount))

  #len_doy = min(length(cum_cases$doy),daycount)
  len_doy = daycount

  for (age in 1:4)
  {
    smoothed = avg_weekly(cum_cases,paste0("tests",age))
    old_names = names(cum_cases)
    cum_cases = cbind(cum_cases,smoothed)
    names(cum_cases) = c(old_names,paste0("smoothed_tests",age))
  }
  calibdata <- right_join(cum_cases, out_cases)
  # need to fill in doy past data to be able to plot true date
  fact_to_add = calibdata$doy[calibdata$time == 0]
  calibdata$doy[is.na(calibdata$doy)] = calibdata$time[is.na(calibdata$doy)] + fact_to_add
  calibdata <- calibdata %>% mutate(date = as.Date(doy + 1, origin = "2020-01-01")) # as.Date is 0-based!!

  calibdata = calibdata %>% group_by(time)

  daily_tests1 = calibdata$tot_tests_i1 - lag(calibdata$tot_tests_i1)
  daily_tests2 = calibdata$tot_tests_i2 - lag(calibdata$tot_tests_i2)
  daily_tests3 = calibdata$tot_tests_i3 - lag(calibdata$tot_tests_i3)
  daily_tests4 = calibdata$tot_tests_i4 - lag(calibdata$tot_tests_i4)

  #c = ggplot(calibdata, aes(x = date)) +
  #  geom_line(aes(y = daily_cases1)) +
  #  geom_line(aes(y = smoothed_cases1), col = "gray30", lty = 2) +
  #  geom_line(aes(y = daily_cases2),col="darkblue") +
  #  geom_line(aes(y = smoothed_cases2), col = "lightblue", lty = 3) +
  #  geom_line(aes(y = daily_cases3),col="darkgreen") +
  #  geom_line(aes(y = smoothed_cases3), col = "lightgreen", lty = 4) +
  #  geom_line(aes(y = daily_cases4),col="darkred") +
  #  geom_line(aes(y = smoothed_cases4), col = "pink", lty = 5) +
  #  #theme_classic() +
  #  theme(legend.position="top") +
  #  ylab("cases")


  #return (c)

  x_lim = c(1, len_doy)
  y_lim = c(0, max(daily_tests1,daily_tests2,daily_tests3,daily_tests4,
	cum_cases$smoothed_tests1,cum_cases$smoothed_tests2,cum_cases$smoothed_tests3,cum_cases$smoothed_tests4,
	out_cases$tests1,out_cases$tests2,out_cases$tests3,out_cases$tests4,na.rm = TRUE))
  
  plot(x = 0, y = 0, type = "n", xlab = "date", ylab = "Daily Tests by Age Group",
       xlim = x_lim, ylim = y_lim, xaxt = "n")

  draw_all_month_axis() 

  doy = cum_cases$doy

  lines(x = doy[2:len_doy], y = daily_tests1[2:len_doy], lty = 1, lwd = 0.75, col = "black")
  lines(x = doy[2:len_doy], y = cum_cases$smoothed_tests1[2:len_doy], lty = 2, lwd = 1, col = "gray30")
  points(x = doy[2:len_doy], y = calibdata$tests1[2:len_doy], col = "gray30", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = daily_tests2[2:len_doy], lty = 1, lwd = 0.75, col = "darkblue")
  lines(x = doy[2:len_doy], y = cum_cases$smoothed_tests2[2:len_doy], lty = 2, lwd = 1, col = "lightblue")
  points(x = doy[2:len_doy], y = calibdata$tests2[2:len_doy], col = "lightblue", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = daily_tests3[2:len_doy], lty = 1, lwd = 0.75, col = "darkgreen")
  lines(x = doy[2:len_doy], y = cum_cases$smoothed_tests3[2:len_doy], lty = 2, lwd = 1, col = "lightgreen")
  points(x = doy[2:len_doy], y = calibdata$tests3[2:len_doy], col = "lightgreen", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = daily_tests4[2:len_doy], lty = 1, lwd = 0.75, col = "darkred")
  lines(x = doy[2:len_doy], y = cum_cases$smoothed_tests4[2:len_doy], lty = 2, lwd = 1, col = "pink")
  points(x = doy[2:len_doy], y = calibdata$tests4[2:len_doy], col = "pink", pch = 1, cex=0.5)
  legend("topleft", legend = c("0-19","20-49","50-69","70+"), col = c("black","darkblue","darkgreen","darkred"), lty = 1, bty = "n")
}

plot_age_case_fit = function(pars, pars_names, pars_fixed, kc_data, kc_age, daycount, state)
{
  #out <- get_model_data(pars, pars_names, pars_fixed, daycount, state)

  parameters = get_params(pars, pars_names, pars_fixed)
  cum_cases = create_data_start_day0(kc_data, parameters$first_case_doy, parameters$delta0offset)

  out = ode(y = state, times = cum_cases$time, func = model_eq, parms = parameters)
  out_cases = as.data.frame(out)

  #len_doy = min(length(cum_cases$doy),daycount)
  len_doy = daycount

  for (age in 1:4)
  {
    smoothed = avg_weekly(cum_cases,paste0("pos",age))
    old_names = names(cum_cases)
    cum_cases = cbind(cum_cases,smoothed)
    names(cum_cases) = c(old_names,paste0("smoothed_cases",age))
  }
  calibdata <- right_join(cum_cases, out_cases)
  # need to fill in doy past data to be able to plot true date
  fact_to_add = calibdata$doy[calibdata$time == 0]
  calibdata$doy[is.na(calibdata$doy)] = calibdata$time[is.na(calibdata$doy)] + fact_to_add
  calibdata <- calibdata %>% mutate(date = as.Date(doy + 1, origin = "2020-01-01")) # as.Date is 0-based!!

  calibdata = calibdata %>% group_by(time)

  daily_cases1 = calibdata$tot_diag_i1 - lag(calibdata$tot_diag_i1)
  daily_cases2 = calibdata$tot_diag_i2 - lag(calibdata$tot_diag_i2)
  daily_cases3 = calibdata$tot_diag_i3 - lag(calibdata$tot_diag_i3)
  daily_cases4 = calibdata$tot_diag_i4 - lag(calibdata$tot_diag_i4)

  #c = ggplot(calibdata, aes(x = date)) +
  #  geom_line(aes(y = daily_cases1)) +
  #  geom_line(aes(y = smoothed_cases1), col = "gray30", lty = 2) +
  #  geom_line(aes(y = daily_cases2),col="darkblue") +
  #  geom_line(aes(y = smoothed_cases2), col = "lightblue", lty = 3) +
  #  geom_line(aes(y = daily_cases3),col="darkgreen") +
  #  geom_line(aes(y = smoothed_cases3), col = "lightgreen", lty = 4) +
  #  geom_line(aes(y = daily_cases4),col="darkred") +
  #  geom_line(aes(y = smoothed_cases4), col = "pink", lty = 5) +
  #  #theme_classic() +
  #  theme(legend.position="top") +
  #  ylab("cases")


  #return (c)

  x_lim = c(1, len_doy)
  y_lim = c(0, max(daily_cases1,daily_cases2,daily_cases3,daily_cases4,
	cum_cases$smoothed_cases1,cum_cases$smoothed_cases2,cum_cases$smoothed_cases3,cum_cases$smoothed_cases4,
	out_cases$pos1,out_cases$pos2,out_cases$pos3,out_cases$pos4,na.rm = TRUE))
  
  plot(x = 0, y = 0, type = "n", las=2, xlab = "",ylab = "Daily Cases by Age Group",
       xlim = x_lim, ylim = y_lim, xaxt = "n")

  draw_all_month_axis() 

  doy = cum_cases$doy

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_cases1[2:len_doy], lty = 2, lwd = 2, col = "gray30")
  lines(x = doy[2:len_doy], y = daily_cases1[2:len_doy], lty = 1, lwd = 1.5, col = "black")
  points(x = doy[2:len_doy], y = calibdata$pos1[2:len_doy], col = "gray30", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_cases2[2:len_doy], lty = 2, lwd = 2, col = "lightblue")
  lines(x = doy[2:len_doy], y = daily_cases2[2:len_doy], lty = 1, lwd = 1.5, col = "darkblue")
  points(x = doy[2:len_doy], y = calibdata$pos2[2:len_doy], col = "lightblue", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_cases3[2:len_doy], lty = 2, lwd = 2, col = "lightgreen")
  lines(x = doy[2:len_doy], y = daily_cases3[2:len_doy], lty = 1, lwd = 1.5, col = "darkgreen")
  points(x = doy[2:len_doy], y = calibdata$pos3[2:len_doy], col = "lightgreen", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_cases4[2:len_doy], lty = 2, lwd = 2, col = "pink")
  lines(x = doy[2:len_doy], y = daily_cases4[2:len_doy], lty = 1, lwd = 1.5, col = "darkred")
  points(x = doy[2:len_doy], y = calibdata$pos4[2:len_doy], col = "pink", pch = 1, cex=0.5)
  legend("topleft", legend = c("0-19","20-49","50-69","70+"), col = c("black","darkblue","darkgreen","darkred"), lty = 1, bty = "n")
}

plot_age_death_fit = function(pars, pars_names, pars_fixed, kc_data, kc_age, daycount, state)
{
  parameters = get_params(pars, pars_names, pars_fixed)
  cum_cases = create_data_start_day0(kc_data, parameters$first_case_doy, parameters$delta0offset)

  out = ode(y = state, times = cum_cases$time, func = model_eq, parms = parameters)
  out_cases = as.data.frame(out)

  #len_doy = min(length(cum_cases$doy),daycount)
  len_doy = daycount

  for (age in 1:4)
  {
    #smoothed = avg_weekly(cum_cases,paste0("hosp_deaths",age))
    smoothed = avg_weekly(cum_cases,paste0("deaths",age))
    old_names = names(cum_cases)
    cum_cases = cbind(cum_cases,smoothed)
    names(cum_cases) = c(old_names,paste0("smoothed_deaths",age))
  }
  calibdata <- right_join(cum_cases, out_cases)
  # need to fill in doy past data to be able to plot true date
  fact_to_add = calibdata$doy[calibdata$time == 0]
  calibdata$doy[is.na(calibdata$doy)] = calibdata$time[is.na(calibdata$doy)] + fact_to_add
  calibdata <- calibdata %>% mutate(date = as.Date(doy + 1, origin = "2020-01-01")) # as.Date is 0-based!!

  calibdata = calibdata %>% group_by(time)

  daily_deaths1 = calibdata$tot_deaths_i1 - lag(calibdata$tot_deaths_i1)
  daily_deaths2 = calibdata$tot_deaths_i2 - lag(calibdata$tot_deaths_i2)
  daily_deaths3 = calibdata$tot_deaths_i3 - lag(calibdata$tot_deaths_i3)
  daily_deaths4 = calibdata$tot_deaths_i4 - lag(calibdata$tot_deaths_i4)

  #c = ggplot(calibdata, aes(x = date)) +
  #  geom_line(aes(y = daily_cases1)) +
  #  geom_line(aes(y = smoothed_cases1), col = "gray30", lty = 2) +
  #  geom_line(aes(y = daily_cases2),col="darkblue") +
  #  geom_line(aes(y = smoothed_cases2), col = "lightblue", lty = 3) +
  #  geom_line(aes(y = daily_cases3),col="darkgreen") +
  #  geom_line(aes(y = smoothed_cases3), col = "lightgreen", lty = 4) +
  #  geom_line(aes(y = daily_cases4),col="darkred") +
  #  geom_line(aes(y = smoothed_cases4), col = "pink", lty = 5) +
  #  #theme_classic() +
  #  theme(legend.position="top") +
  #  ylab("cases")


  #return (c)

  x_lim = c(1, len_doy)
  y_lim = c(0, max(daily_deaths1,daily_deaths2,daily_deaths3,daily_deaths4,
	cum_cases$smoothed_deaths1,cum_cases$smoothed_deaths2,cum_cases$smoothed_deaths3,cum_cases$smoothed_deaths4,
	out_cases$deaths1,out_cases$deaths2,out_cases$deaths3,out_cases$deaths4,na.rm = TRUE))
  
#  plot(x = 0, y = 0, type = "n", las=2, ylab = "Daily Hosp Deaths by Age Group",
  plot(x = 0, y = 0, type = "n", las=2, xlab = "",ylab = "Daily Deaths by Age Group",
       xlim = x_lim, ylim = y_lim, xaxt = "n")

  draw_all_month_axis() 

  doy = cum_cases$doy

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_deaths1[2:len_doy], lty = 2, lwd = 2, col = "gray30")
  lines(x = doy[2:len_doy], y = daily_deaths1[2:len_doy], lty = 1, lwd = 1.5, col = "black")
  #points(x = doy[2:len_doy], y = calibdata$hosp_deaths1[2:len_doy], col = "gray30", pch = 1, cex=0.5)
  points(x = doy[2:len_doy], y = calibdata$deaths1[2:len_doy], col = "gray30", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_deaths2[2:len_doy], lty = 2, lwd = 2, col = "lightblue")
  lines(x = doy[2:len_doy], y = daily_deaths2[2:len_doy], lty = 1, lwd = 1.5, col = "darkblue")
  #points(x = doy[2:len_doy], y = calibdata$hosp_deaths2[2:len_doy], col = "lightblue", pch = 1, cex=0.5)
  points(x = doy[2:len_doy], y = calibdata$deaths2[2:len_doy], col = "lightblue", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_deaths3[2:len_doy], lty = 2, lwd = 2, col = "lightgreen")
  lines(x = doy[2:len_doy], y = daily_deaths3[2:len_doy], lty = 1, lwd = 1.5, col = "darkgreen")
  #points(x = doy[2:len_doy], y = calibdata$hosp_deaths3[2:len_doy], col = "lightgreen", pch = 1, cex=0.5)
  points(x = doy[2:len_doy], y = calibdata$deaths3[2:len_doy], col = "lightgreen", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_deaths4[2:len_doy], lty = 2, lwd = 2, col = "pink")
  lines(x = doy[2:len_doy], y = daily_deaths4[2:len_doy], lty = 1, lwd = 1.5, col = "darkred")
  #points(x = doy[2:len_doy], y = calibdata$hosp_deaths4[2:len_doy], col = "pink", pch = 1, cex=0.5)
  points(x = doy[2:len_doy], y = calibdata$deaths4[2:len_doy], col = "pink", pch = 1, cex=0.5)
  legend("topleft", legend = c("0-19","20-49","50-69","70+"), col = c("black","darkblue","darkgreen","darkred"), lty = 1, bty = "n")
}

plot_age_hosp_fit = function(pars, pars_names, pars_fixed, kc_data, kc_age, daycount, state)
{
  parameters = get_params(pars, pars_names, pars_fixed)
  cum_cases = create_data_start_day0(kc_data, parameters$first_case_doy, parameters$delta0offset)

  out = ode(y = state, times = cum_cases$time, func = model_eq, parms = parameters)
  out_cases = as.data.frame(out)

  #len_doy = min(length(cum_cases$doy),daycount)
  len_doy = daycount

  for (age in 1:4)
  {
    smoothed = avg_weekly(cum_cases,paste0("hosps",age))
    old_names = names(cum_cases)
    cum_cases = cbind(cum_cases,smoothed)
    names(cum_cases) = c(old_names,paste0("smoothed_hosp",age))
  }
  calibdata <- right_join(cum_cases, out_cases)
  # need to fill in doy past data to be able to plot true date
  fact_to_add = calibdata$doy[calibdata$time == 0]
  calibdata$doy[is.na(calibdata$doy)] = calibdata$time[is.na(calibdata$doy)] + fact_to_add
  calibdata <- calibdata %>% mutate(date = as.Date(doy + 1, origin = "2020-01-01")) # as.Date is 0-based!!

  calibdata = calibdata %>% group_by(time)

  daily_hosps1 = calibdata$tot_hosp_i1 - lag(calibdata$tot_hosp_i1)
  daily_hosps2 = calibdata$tot_hosp_i2 - lag(calibdata$tot_hosp_i2)
  daily_hosps3 = calibdata$tot_hosp_i3 - lag(calibdata$tot_hosp_i3)
  daily_hosps4 = calibdata$tot_hosp_i4 - lag(calibdata$tot_hosp_i4)

  #c = ggplot(calibdata, aes(x = date)) +
  #  geom_line(aes(y = daily_cases1)) +
  #  geom_line(aes(y = smoothed_cases1), col = "gray30", lty = 2) +
  #  geom_line(aes(y = daily_cases2),col="darkblue") +
  #  geom_line(aes(y = smoothed_cases2), col = "lightblue", lty = 3) +
  #  geom_line(aes(y = daily_cases3),col="darkgreen") +
  #  geom_line(aes(y = smoothed_cases3), col = "lightgreen", lty = 4) +
  #  geom_line(aes(y = daily_cases4),col="darkred") +
  #  geom_line(aes(y = smoothed_cases4), col = "pink", lty = 5) +
  #  #theme_classic() +
  #  theme(legend.position="top") +
  #  ylab("cases")


  #return (c)

  x_lim = c(1, len_doy)
  y_lim = c(0, max(daily_hosps1,daily_hosps2,daily_hosps3,daily_hosps4,
	cum_cases$smoothed_hosp1,cum_cases$smoothed_hosp2,cum_cases$smoothed_hosp3,cum_cases$smoothed_hosp4,
	out_cases$hosps1,out_cases$hosps2,out_cases$hosps3,out_cases$hosps4,na.rm = TRUE))
  
  plot(x = 0, y = 0, type = "n", las=2, xlab = "",ylab = "Daily Hospital Admits by Age Group",
       xlim = x_lim, ylim = y_lim, xaxt = "n")

  draw_all_month_axis() 

  doy = cum_cases$doy

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_hosp1[2:len_doy], lty = 2, lwd = 2, col = "gray30")
  lines(x = doy[2:len_doy], y = daily_hosps1[2:len_doy], lty = 1, lwd = 1.5, col = "black")
  points(x = doy[2:len_doy], y = calibdata$hosps1[2:len_doy], col = "gray30", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_hosp2[2:len_doy], lty = 2, lwd = 2, col = "lightblue")
  lines(x = doy[2:len_doy], y = daily_hosps2[2:len_doy], lty = 1, lwd = 1.5, col = "darkblue")
  points(x = doy[2:len_doy], y = calibdata$hosps2[2:len_doy], col = "lightblue", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_hosp3[2:len_doy], lty = 2, lwd = 2, col = "lightgreen")
  lines(x = doy[2:len_doy], y = daily_hosps3[2:len_doy], lty = 1, lwd = 1.5, col = "darkgreen")
  points(x = doy[2:len_doy], y = calibdata$hosps3[2:len_doy], col = "lightgreen", pch = 1, cex=0.5)

  lines(x = doy[2:len_doy], y = cum_cases$smoothed_hosp4[2:len_doy], lty = 2, lwd = 2, col = "pink")
  lines(x = doy[2:len_doy], y = daily_hosps4[2:len_doy], lty = 1, lwd = 1.5, col = "darkred")
  points(x = doy[2:len_doy], y = calibdata$hosps4[2:len_doy], col = "pink", pch = 1, cex=0.5)
  legend("topleft", legend = c("0-19","20-49","50-69","70+"), col = c("black","darkblue","darkgreen","darkred"), lty = 1, bty = "n")
}

plot_param_sets = function(doy, result_matrix, doy_actual, result_actual, y_lab = "result",
                           col_pal = "black", col_idx = 1, x_lim = NULL, y_lim = NULL, ...)
{
  data_end = min(which(apply(result_matrix, 1, function(x) all(is.na(x))) & doy > 50))
  if (is.infinite(data_end)) { data_end = nrow(result_matrix) }
  if (is.null(x_lim)) { x_lim = c(1, data_end) }
  if (is.null(y_lim)) { y_lim = c(0, max(result_matrix, na.rm = TRUE)) }

  plot(x = 0, y = 0, type = "n", las=2, xlab = "",ylab = y_lab,
       xlim = x_lim, ylim = y_lim, xaxt = "n", ...)
  draw_all_month_axis() 
  
  for (i in 1:ncol(result_matrix))
    lines(x = doy, y = result_matrix[,i], lty = 1, lwd = 0.75, col = col_pal[col_idx[i]])
  lines(x = doy_actual, y = result_actual, col = "red", lwd = 3)
  
}

plot_param_sets_age = function(doy, result_matrix_1, result_matrix_2, result_matrix_3, result_matrix_4,
                               doy_actual, result_actual_1, result_actual_2, result_actual_3, result_actual_4,
                               y_lab = "result", col_pal = "black", col_idx = 1, ...)
{
  plot(x = 0, y = 0, type = "n", las=2, xlab = "age", ylab = y_lab,
       xlim = c(1, 4), ylim = c(0,1), ...)
  
  # just take last row of actual data and plot 
  actual_idx = length(doy_actual)
  model_idx = which(doy == doy_actual[actual_idx])
  doy_plot = doy_actual[actual_idx]
  
  for (i in 1:ncol(result_matrix_1))
    lines(x = 1:4, y = c(result_matrix_1[model_idx,i], result_matrix_2[model_idx,i], result_matrix_3[model_idx,i], result_matrix_4[model_idx,i]), 
          lty = 1, lwd = 0.75, col = col_pal[col_idx[i]])
  lines(x = 1:4, y = c(result_actual_1[actual_idx], result_actual_2[actual_idx], result_actual_3[actual_idx], result_actual_4[actual_idx]), 
        col = "red", lwd = 3)
  
}

plot_scenarios = function(doy, result_matrix, doy_actual, result_actual, y_lab = "result",
                          col_pal = "black", col_idx = 1, delta = NULL, 
                          vaccination_date = NULL, calib_date = NULL, 
                          sd_vals, x_lim = NULL, y_lim = NULL,
                          totalpop = NULL, target=0, all_months=0, limit=0,hyster=0,...)
{
  if (is.null(x_lim)) 
  { 
    data_end = min(which(apply(result_matrix, 1, function(x) all(is.na(x))) & doy > 50))
    if (is.infinite(data_end)) { data_end = nrow(result_matrix) }

    x_lim = c(1, data_end) 
  }
  if (is.null(y_lim)) 
  { 
    y_lim = c(0, max(result_matrix[doy >= x_lim[1] & doy <= x_lim[2],], na.rm = TRUE)) 
  }

  if (y_lim[2] >= 10000)
  {
      plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "", ylab = y_lab, # xlab = "date"
	   xlim = x_lim, ylim = y_lim, xaxt = "n", yaxt="n",...)
      labels = pretty(y_lim /1000)
      axis(side = 2, las=2, at = labels * 1000, labels = paste0(labels, 'K') )
  }
  else
  {
      plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "", ylab = y_lab, # xlab = "date"
	   xlim = x_lim, ylim = y_lim, xaxt = "n", ...)
  }
  if (all_months == 1)
      draw_all_month_axis()
  else
      draw_month_axis()

  if (!is.null(totalpop) && totalpop > 0)
  {
    labels = pretty(y_lim / totalpop * 100)
    axis(side = 4, las=2, at = labels * totalpop / 100, labels = paste0(labels, '%') )
    mtext("percent population", side = 4, line = 2, cex = 0.95)
  }
  
  if (!is.null(delta))
  {
    # https://stackoverflow.com/questions/27250542/how-to-make-gradient-color-filled-timeseries-plot-in-r
    sd_cols = colorRampPalette(c("white", "gray30"))(500)

    n = 500
    e = par('usr')
    xmax = e[2]; ymin = e[3]; ymax = e[4]
    # track sd changes through who1e period after start of lockdown
    width = (x_lim[2] - delta[1]) / n
    x_amt = seq(delta[1], x_lim[2], width)
    
    save_sd = sd_vals[as.integer(x_amt[1])]
    save_i = 1
    for (i in 1:n) {
	if (x_amt[i] > data_end) x_amt[i] = data_end
        this_sd = sd_vals[as.integer(x_amt[i])]
	if (!is.na(this_sd) && this_sd != save_sd) {
	    rect(x_amt[save_i], ymin, x_amt[save_i] + (width*(i - save_i + 1)), ymax, col=sd_cols[save_sd *500], border=NA)
	    save_sd = this_sd
	    save_i = i
	}
    }
    if (save_i != n) {
	    rect(x_amt[save_i], ymin, x_amt[save_i] + (width*(n - save_i + 1)), ymax, col=sd_cols[save_sd *500], border=NA)
    }
  }
  
  if (!is.null(calib_date))
      thresh = which(doy == calib_date)
  else
      thresh = which(doy == vaccination_date)

  for (i in ncol(result_matrix):1) # reverse order so base scenario plotted last
  {
    # change second line lty = 6 to go back to dashed line projections
    #lines(x = doy[1:thresh], y = result_matrix[1:thresh,i], lty = 1, lwd = 2, col = col_pal[col_idx[i]])
    lines(x = doy[1:thresh], y = result_matrix[1:thresh,i], lty = 1, lwd = 2, col = "black")
    lines(x = doy[thresh:length(doy)], y = result_matrix[thresh:length(doy),i], lty = 1, lwd = 2, col = col_pal[col_idx[i]])
  }
  if (target > 0) 
  {
      lines(x = doy[1:data_end], y = rep(target,data_end), lty = 2, lwd = 2, col = "black")
  }
#  points(x = doy_actual, y = result_actual, col = "gray30", pch = ".")
  
  if (!is.null(calib_date))
  {
    #end calibration
    #points(x = calib_date, y = 0, col = "black", pch = 8)
   lines(x = rep(calib_date,2), y = y_lim, col = "black", lty = 2, lwd = 2)
  }
  if (!is.null(vaccination_date))
  {
    #points(x = vaccination_date, y = 0, col = "orange", pch = 12)
   lines(x = rep(vaccination_date,2), y = y_lim, col = "orange", lty = 2, lwd = 2)
  }
  if (limit > 0)
  {
      lines(x = doy[1:data_end], y = rep((limit+hyster),data_end), lty = 2, lwd = 1, col = "red")
      lines(x = doy[1:data_end], y = rep((limit-hyster),data_end), lty = 2, lwd = 1, col = "green")
  }
  
  
  box(bty = "l")
}

plot_scenario_rates = function(doy, result_matrix, doy_actual, result_actual, y_lab = "result",
                          col_pal = "black", col_idx = 1, delta = NULL, 
                          vaccination_date = NULL, calib_date = NULL, 
                          sd_vals, x_lim = NULL, y_lim = NULL,
                          totalpop = NULL, limit=50,hyster=25,period=14,...)
{
  # change result matrix into <period> day rate per 100k of population
  #delta_matrix = result_matrix
  if (is.null(x_lim)) 
  { 
    data_end = min(which(apply(result_matrix, 1, function(x) all(is.na(x))) & doy > 50))
    
    if (is.infinite(data_end)) { data_end = nrow(result_matrix) }

    data_end = data_end-1

    x_lim = c(1, data_end) 
  }
  delta_matrix = result_matrix[1:data_end,]

  # for the 1st n days, just use the number of recorded days so far to get a n day average
  for (d in 1:period)
  {
	delta_matrix[d,] = (result_matrix[d,] - result_matrix[1,])*period/d
	delta_matrix[d,] = delta_matrix[d,]*100000/totalpop
  }
  for (d in ((period+1):data_end))
  {
	delta_matrix[d,] = result_matrix[d,] - result_matrix[(d-period),]
	delta_matrix[d,] = delta_matrix[d,]*100000/totalpop
  }
  doy = doy[1:data_end]
  if (is.null(y_lim)) 
  { 
    y_lim = c(0, max(delta_matrix[x_lim[1]:x_lim[2],], na.rm = TRUE)) 
  }
  
  plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "", ylab = y_lab, # xlab = "date"
       xlim = x_lim, ylim = y_lim, xaxt = "n", ...)

  draw_all_month_axis()

  if (!is.null(delta))
  {
    # https://stackoverflow.com/questions/27250542/how-to-make-gradient-color-filled-timeseries-plot-in-r
    sd_cols = colorRampPalette(c("white", "gray30"))(500)

    n = 500
    e = par('usr')
    xmax = e[2]; ymin = e[3]; ymax = e[4]
    # track sd changes through who1e period after start of lockdown
    width = (x_lim[2] - delta[1]) / n
    x_amt = seq(delta[1], x_lim[2], width)
    
    save_sd = sd_vals[as.integer(x_amt[1])]
    save_i = 1
    for (i in 1:n) {
        this_sd = sd_vals[as.integer(x_amt[i])]
	if (!is.na(this_sd) && this_sd != save_sd) {
	    rect(x_amt[save_i], ymin, x_amt[save_i] + (width*(i - save_i + 1)), ymax, col=sd_cols[save_sd *500], border=NA)
	    save_sd = this_sd
	    save_i = i
	}
    }
    if (save_i != n) {
	    rect(x_amt[save_i], ymin, x_amt[save_i] + (width*(n - save_i + 1)), ymax, col=sd_cols[save_sd *500], border=NA)
    }
  }
  
  if (!is.null(calib_date))
      thresh = which(doy == calib_date)
  else
      thresh = which(doy == vaccination_date)
  
  for (i in ncol(delta_matrix):1) # reverse order so base scenario plotted last
  {
    # change second line lty = 6 to go back to dashed line projections
    #lines(x = doy[1:thresh], y = delta_matrix[1:thresh,i], lty = 1, lwd = 2, col = col_pal[col_idx[i]])
    lines(x = doy[1:thresh], y = delta_matrix[1:thresh,i], lty = 1, lwd = 2, col = "black")
    lines(x = doy[thresh:data_end], y = delta_matrix[thresh:data_end,i], lty = 1, lwd = 2, col = col_pal[col_idx[i]])
  }
  lines(x = doy[1:data_end], y = rep((limit+hyster),data_end), lty = 2, lwd = 2, col = "black")
  lines(x = doy[1:data_end], y = rep((limit-hyster),data_end), lty = 2, lwd = 2, col = "black")
  if (!is.null(calib_date))
  {
    #end calibration
    #points(x = calib_date, y = 0, col = "black", pch = 8)
   lines(x = rep(calib_date,2), y = y_lim, col = "black", lty = 2, lwd = 2)
  }
  if (!is.null(vaccination_date))
  {
    #points(x = vaccination_date, y = 0, col = "orange", pch = 12)
   lines(x = rep(vaccination_date,2), y = y_lim, col = "orange", lty = 2, lwd = 2)
  }
  
  box(bty = "l")
}

plot_delta_scenarios = function(doy, result_matrix, doy_actual, result_actual, y_lab = "result",
                          col_pal = "black", col_idx = 1, delta = NULL, 
                          vaccination_date = NULL, calib_date = NULL,
                          sd_vals, x_lim = NULL, y_lim = NULL, totalpop = NULL,...)
{
  # change result matrix into results SINCE vaccine date (zero before)
  delta_matrix = result_matrix
  for (d in 1:(vaccination_date-1))
  {
	delta_matrix[d,] = 0
  }
  for (d in vaccination_date:nrow(result_matrix))
  {
	delta_matrix[d,] = delta_matrix[d,]-result_matrix[vaccination_date,]
  }
  result_matrix = delta_matrix
  if (is.null(x_lim)) 
  { 
    data_end = min(which(apply(result_matrix, 1, function(x) all(is.na(x))) & doy > 50))
    if (is.infinite(data_end)) { data_end = nrow(result_matrix) }
    x_lim = c(1, data_end-1) 
  }

  if (is.null(y_lim)) 
  { 
    y_lim = c(0, max(result_matrix[doy >= vaccination_date & doy <= x_lim[2],1]-result_matrix[doy >= vaccination_date & doy <= x_lim[2],(2:ncol(result_matrix))], na.rm = TRUE)) 
  }
  
  if (y_lim[2] >= 10000)
  {
      plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "", ylab = y_lab, # xlab = "date"
	   xlim = x_lim, ylim = y_lim, xaxt = "n", yaxt = "n", ...)
      labels = pretty(y_lim /1000)
      axis(side = 2, las=2, at = labels * 1000, labels = paste0(labels, 'K') )
  }
  else
  {
      plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "", ylab = y_lab, # xlab = "date"
	   xlim = x_lim, ylim = y_lim, xaxt = "n", ...)
  }

  draw_all_month_axis()

  #if (!is.null(totalpop))
  #{
    max_baseline = max(delta_matrix[doy >= vaccination_date & doy <= x_lim[2],1],na.rm = TRUE)
    labels = pretty((y_lim / max(delta_matrix[doy >= vaccination_date & doy <= x_lim[2],1],na.rm = TRUE)) * 100)

    axis(side = 4, las=2, at = labels * max(result_matrix[doy >= vaccination_date & doy <= x_lim[2],1],na.rm = TRUE) / 100, labels = paste0(labels, '%') )
    mtext("percent reduction", side = 4, line = 2, cex = 0.95)
  #}
  
  if (!is.null(delta))
  {
    # https://stackoverflow.com/questions/27250542/how-to-make-gradient-color-filled-timeseries-plot-in-r
    sd_cols = colorRampPalette(c("white", "gray30"))(500)

    n = 500
    e = par('usr')
    xmax = e[2]; ymin = e[3]; ymax = e[4]
    # track sd changes through who1e period after start of lockdown
    width = (x_lim[2] - delta[1]) / n
    x_amt = seq(delta[1], x_lim[2], width)
    
    save_sd = sd_vals[as.integer(x_amt[1])]
    save_i = 1
    for (i in 1:n) {
        this_sd = sd_vals[as.integer(x_amt[i])]
	if (!is.na(this_sd) && this_sd != save_sd) {
	    rect(x_amt[save_i], ymin, x_amt[save_i] + (width*(i - save_i + 1)), ymax, col=sd_cols[save_sd *500], border=NA)
	    save_sd = this_sd
	    save_i = i
	}
    }
    if (save_i != n) {
	    rect(x_amt[save_i], ymin, x_amt[save_i] + (width*(n - save_i + 1)), ymax, col=sd_cols[save_sd *500], border=NA)
    }
  }
  
  if (!is.null(calib_date))
      thresh = which(doy == calib_date)
  else
      thresh = which(doy == vaccination_date)
  
  for (i in ncol(result_matrix):1) # reverse order (No base scenario plotted)
  {
    # change second line lty = 6 to go back to dashed line projections
    #lines(x = doy[1:thresh], y = (result_matrix[1:thresh,1] -result_matrix[1:thresh,i]), lty = 1, lwd = 2, col = col_pal[col_idx[i]])
    lines(x = doy[1:thresh], y = (result_matrix[1:thresh,1] -result_matrix[1:thresh,i]), lty = 1, lwd = 2, col = "black")
    lines(x = doy[thresh:length(doy)], y = (result_matrix[thresh:length(doy),1]-result_matrix[thresh:length(doy),i]), lty = 1, lwd = 2, col = col_pal[col_idx[i]])
  }
#  points(x = doy_actual, y = result_actual, col = "gray30", pch = ".")
  
  if (!is.null(calib_date))
  {
    #end calibration
    #points(x = calib_date, y = 0, col = "black", pch = 8)
   lines(x = rep(calib_date,2), y = y_lim, col = "black", lty = 2, lwd = 2)
  }
  if (!is.null(vaccination_date))
  {
    #vaccination
    points(x = vaccination_date, y = 0, col = "orange", pch = 12)
   lines(x = rep(vaccination_date,2), y = y_lim, col = "orange", lty = 2, lwd = 2)
  }
  box(bty = "l")
}

plot_sd_restored = function(x_lim, delta, sd1, sd2, sd2_labels)
{
  plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "", ylab = "pre-COVID physical interactions (pC-pI) restored %", 
       xlim = x_lim, ylim = c(0, 100), xaxt = "n")
  draw_all_month_axis()
  
  for (i in 1:length(sd2))
  {
    
    lines(x = c(x_lim[1], delta[1:4], x_lim[2]), 
          y = c(100, 100, (1-sd1) * 100, (1-sd1) * 100, (1-sd2[i]) * 100, (1-sd2[i]) * 100),
          lty = i)
  }
  
  legend("top", legend = sd2_labels, lty = 1:length(sd2), bty = "n")
}

plot_scenarios_uncertainty = function(doy, result_matrix, nointerv_bounds, uncertainty_matrix_list,
                                      output, daily = FALSE,
                                      y_lab = "result", col_pal = "black", 
                                      x_lim = NULL, y_lim = NULL, delta5, sample_date,
                                      totalpop = NULL, ...)
{
  if (is.null(x_lim)) 
  { 
    data_end = min(which(apply(result_matrix, 1, function(x) all(is.na(x))) & doy > 50))
    if (is.infinite(data_end)) { data_end = nrow(result_matrix) }
    x_lim = c(1, data_end) 
  }
  if (is.null(y_lim)) 
  { 
    y_lim = c(0, max(result_matrix[doy >= x_lim[1] & doy <= x_lim[2],], na.rm = TRUE)) 
  }
  
  plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "date", ylab = y_lab,
       xlim = x_lim, ylim = y_lim, xaxt = "n", ...)
  draw_all_month_axis()
  if (!is.null(totalpop))
  {
    labels = pretty(y_lim / totalpop * 100)
    axis(side = 4, at = labels * totalpop / 100, labels = paste0(labels, '%') )
  }
  
  # first draw uncertainty polygon for no intervention
  if (!is.null(nointerv_bounds))
  {
    poly_idxs = apply(nointerv_bounds, 1, function(x) all(!is.na(x)))
    polygon(c(rev(doy[poly_idxs]), doy[poly_idxs]), 
            c(rev(nointerv_bounds[poly_idxs,2]), nointerv_bounds[poly_idxs,1]), 
            col = alpha('grey90', 0.5), border = NA)
  }
  
  # now draw sample lines for uncertainty
  # this is a list of matrices, each matrix is for an intervention with columns for the parameter sets
  for (l in 1:length(uncertainty_matrix_list))
  {
    uncertainty_matrix = uncertainty_matrix_list[[l]][[output]]
    doy = uncertainty_matrix_list[[l]]$doy
    if (daily)
    {
      uncertainty_matrix = apply(uncertainty_matrix, 2, diff)
      doy = doy[-1]
    }
    
    color = alpha(col_pal[l+1], 0.2)
    for (i in 1:ncol(uncertainty_matrix)) # reverse order so base scenario plotted last
    {
      # note i+1 in color palette since we are skipping first color of no intervention
      lines(x = doy, y = uncertainty_matrix[,i], lty = 1, lwd = 0.3, col = color)
    }
  }
  
  # now best fit
  for (i in ncol(result_matrix):1) # reverse order so base scenario plotted last
  {
    # draw black behind so stand out better
    lines(x = doy, y = result_matrix[,i], lty = 1, lwd = 2.5, col = "black")
    lines(x = doy, y = result_matrix[,i], lty = 1, lwd = 2, col = col_pal[i])
  }

  # need to add pts for intervestion and sample date
  points(x = delta5, y = result_matrix[doy == delta5,1], col = "black", pch = 8)
  points(x = sample_date, y = 0, col = "black", pch = 12)
  
  box(bty = "l")
}


plot_scenarios_multi = function(doy, result_matrix, doy_actual, result_actual, y_lab = "result",
                            col = "black", delta = NULL, sd1, sd2, x_lim = NULL, y_lim = NULL,
                            totalpop = NULL, ...)
{
    if (is.null(x_lim)) 
    { 
      data_end = min(which(apply(result_matrix, 1, function(x) all(is.na(x))) & doy > 50))
      if (is.infinite(data_end)) { data_end = nrow(result_matrix) }
      x_lim = c(1, data_end) 
    }
    if (is.null(y_lim)) 
    { 
      y_lim = c(0, max(result_matrix[doy >= x_lim[1] & doy <= x_lim[2],], na.rm = TRUE)) 
    }

    plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "date", ylab = y_lab,
         xlim = x_lim, ylim = y_lim, xaxt = "n", ...)
    draw_all_month_axis()
    if (!is.null(totalpop))
    {
      labels = pretty(y_lim / totalpop * 100)
      axis(side = 4, las=2, at = labels * totalpop / 100, labels = paste0(labels, '%') )
    }
    
    for (i in 1:ncol(result_matrix)) # reverse order so base scenario plotted last
    {
      lines(x = doy, y = result_matrix[,i], lty = 1, lwd = 0.5, col = col)
    }

    if (!is.null(delta))
    {
      #intervention
      points(x = delta[5], y = result_matrix[doy == delta[5],1], col = "black", pch = 8)
    }
    box(bty = "l")
}

plot_interven_scenarios_multi = function(result_list, output, daily = FALSE, doy_actual, result_actual, y_lab = "result",
                                         col_pal = "black", col_idx = 1, delta = NULL, sd1, sd2, x_lim = NULL, y_lim = NULL,
                                         totalpop = NULL, ...)
{
  # for now just take y_lim from the first to make all the y axes the same
  # not idea, doesn't take x_lim into account...
  y_lim = if (daily) { c(0, max(apply(result_list[[1]][[output]], 2, diff), na.rm = TRUE)) } 
          else { c(0, max(result_list[[1]][[output]], na.rm = TRUE)) }
  
  for (i in 1:length(result_list))
  {
    result_matrix = result_list[[i]][[output]]
    doy = result_list[[i]]$doy
    if (daily)
    {
      result_matrix = apply(result_matrix, 2, diff)
      doy = doy[-1]
    }
    plot_scenarios_multi(doy, result_matrix, doy_actual, result_actual, 
                         y_lab = y_lab, x_lim = NULL, y_lim = y_lim, col = col_pal[col_idx[i]], 
                         delta = delta, sd1 = sd1, sd2 = sd2, totalpop = totalpop)
    
  }
}
  

plot_asym = function(doy, result_matrix, doy_actual, result_actual, y_lab = "result",
                          col_pal = "black", col_idx = 1, lty_pal = 1, lty_idx = 1, y_lim = NULL)
{
  data_end = min(which(apply(result_matrix, 1, function(x) all(is.na(x))) & doy > 50))
  if (is.null(y_lim)) { y_lim = c(0, max(result_matrix, na.rm = TRUE)) }
  
  plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "date", ylab = y_lab,
       xlim = c(1,data_end), ylim = y_lim, xaxt = "n")
  draw_all_month_axis() 

  for (i in ncol(result_matrix):1) # reverse order so base scenario plotted last
  {
    lines(x = doy, y = result_matrix[,i], 
          lty = lty_pal[lty_idx[i]], lwd = 1.5, col = col_pal[col_idx[i]])
  }
  points(x = doy_actual, y = result_actual, col = "gray30", pch = 16, cex = 0.5)
  
}

plot_sds = function(sd_vals, result_matrix, y_lab = "result", col_pal = "black", col_idx = 1,
                    sd_calib, sd_range, totalpop = NULL, ...)
{
  y_lim = c(0, max(result_matrix))
  plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "distancing (percent reduction from normal)", ylab = y_lab,
       xlim = c(0, 1), ylim = y_lim, ...)

  if (!is.null(totalpop))
  {
    labels = pretty(y_lim / totalpop * 100)
    axis(side = 4, at = labels * totalpop / 100, labels = paste0(labels, '%') )
  }
  
  for (i in ncol(result_matrix):1) # reverse order so base scenario plotted last
  {
    lines(x = sd_vals, y = result_matrix[,i], 
          lwd = 2, col = col_pal[col_idx[i]])
  }
  points(x = sd_calib, y =  0, col = "black", pch = 8, cex = 1.5)
  
}

plot_sds_heatmap = function(result_matrix, title, legend = FALSE, col= colorRampPalette(brewer.pal(9, "Blues"))(1000),...)
{
  heatmap(result_matrix, Rowv = NA, Colv = NA, scale = "none", main = title,
          col = col, ...)
  if (legend)
  {
    legend(x="topleft", legend=c(min(result_matrix), median(result_matrix), max(result_matrix)), 
           fill = colorRampPalette(brewer.pal(9, "Blues"))(3))
  }
}

plot_percent_hosp = function(out_data, cols, ..., legend = TRUE)
{
  plot(x = 0, y = 0, type = "n", xlim = range(out_data[,"time"]), ylim = c(0, 0.5),
       las=2, xlab = "model time", ylab = "percent symp hospitalized", bty = "l", ...)

  for (age in 1:4)
  {
    lines(out_data[,"time"], out_data[,paste0("cum_hosp_i", age)] / out_data[,paste0("cum_sym_i", age)], 
          col = cols[age])
  }
  lines(out_data[,"time"], sum_across_ages(out_data, col_start = "cum_hosp_i")$all / sum_across_ages(out_data, col_start = "cum_sym_i")$all, 
      col = "black", lwd = 1.5)
  
  if (legend)
  {
    legend("topleft", legend = c(paste("age", 1:4), "all"), col = c(cols, "black"), lty = 1, bty = "n")
  }
}

plot_sd3 = function(out_data, cols, ..., legend = TRUE)
{
  r = plot(x = 0, y = 0, type = "n", xlim = range(out_data[,"time"]), ylim = c(0, 1.0),
       las=2, xlab = "model time", ylab = "dynamic social dist", bty = "l", ...)

  for (age in 1:4)
  {
    lines(out_data[,"time"], out_data[,paste0("sd3_adj_i", age)], 
          col = cols[age])
  }
  
  if (legend)
  {
    legend("topleft", legend = c(paste("age", 1:4)), col = c(cols), lty = 1:4, bty = "n")
  }
  return (r)
}

draw_month_axis = function()
{
  all_label1 = ymd(paste("2020", 1:12, "1", sep = "-"))
  all_label2 = ymd(paste("2021", 1:12, "1", sep = "-"))
  all_label3 = ymd(paste("2022", 1:12, "1", sep = "-"))
  all_ylabels = c(yday(all_label1), 366 + yday(all_label2), 731 + yday(all_label3))
  label1 = ymd(paste("2020", c(1,4,7,10), "1", sep = "-"))
  label2 = ymd(paste("2021", c(1,4,7,10), "1", sep = "-"))
  label3 = ymd(paste("2022", c(1,4,7,10), "1", sep = "-"))
  labels = c(all_label1[1],"","",all_label1[4],"","",all_label1[7],"","",all_label1[10],"","")
  labels = c(labels,all_label2[1],"","",all_label2[4],"","",all_label2[7],"","",all_label2[10],"","")
  labels = c(labels,all_label3[1],"","",all_label3[4],"","",all_label3[7],"","",all_label3[10],"","")
  ylabels = c(yday(label1), 366 + yday(label2), 731 + yday(label3))
  axis(side = 1, las=2, at = all_ylabels, labels = format(labels, "%b %Y")) 
}
draw_all_month_axis = function()
{
  label1 = ymd(paste("2020", 1:12, "1", sep = "-"))
  label2 = ymd(paste("2021", 1:12, "1", sep = "-"))
  label3 = ymd(paste("2022", 1:12, "1", sep = "-"))
  labels = c(label1,label2,label3)
  mo_labels=format(labels, "%b")
  #m_labels = apply(mo_labels,function(x) all(substr(x,1,1)))
  ylabels = c(yday(label1), 366 + yday(label2), 731 + yday(label3))
  axis(side = 1, las=2, at = ylabels, labels = mo_labels)
}
plot_vac_reff_correl = function(doy, vac_data, reff_data, doy_actual, result_actual, y_lab = "result",
                          col_pal = "black", col_idx = 1, delta = NULL, 
                          vaccination_date = NULL, calib_date = NULL, 
                          sd_vals, x_lim = NULL, y_lim = NULL,
                          totalpop = NULL, target=0, ...)
{
  if (is.null(x_lim)) 
  { 
    data_end = min(which(apply(vac_data, 1, function(x) all(is.na(x))) & doy > 50))
    if (is.infinite(data_end)) { data_end = nrow(vac_data) }

    data_end2 = min(which(apply(reff_data, 1, function(x) all(is.na(x))) & doy > 50))
    if (is.infinite(data_end2)) { data_end2 = nrow(reff_data) }

    x_lim = c(vaccination_date, min(data_end,data_end2)) 
    earliest=min(data_end,data_end2)
    latest=vaccination_date
    for (i in ncol(vac_data):2) # reverse order so base scenario plotted last
    {
        found = 0
	for (j in x_lim[1]:x_lim[2])
	{
	    if (!is.na(reff_data[doy==(j-1),i]) && reff_data[doy==(j-1),i] > 1 && 
		!is.na(reff_data[doy==j,i]) && reff_data[doy==j,i] <= 1) 
	    {
		cross_date=j
		found = 1
	    }
	}
	if (found == 1)
	{
	    earliest = min(cross_date,earliest)
	    latest = max(cross_date,latest)
	    print(paste(i,earliest,latest))
	}
    }
    x_lim[1]=earliest
    x_lim[2]=latest
  }
  if (is.null(y_lim)) 
  { 
    y_lim = c(0, max(vac_data[doy >= x_lim[1] & doy <= x_lim[2],], na.rm = TRUE)) 
  }

  if (y_lim[2] >= 10000)
  {
      plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "", ylab = y_lab, # xlab = "date"
	   xlim = x_lim, ylim = y_lim, xaxt = "n", yaxt="n",...)
      labels = pretty(y_lim /1000)
      axis(side = 2, las=2, at = labels * 1000, labels = paste0(labels, 'K') )
  }
  else
  {
      plot(x = 0, y = 0, type = "n", bty = 'l', las=2, xlab = "", ylab = y_lab, # xlab = "date"
	   xlim = x_lim, ylim = y_lim, xaxt = "n", ...)
  }
  draw_all_month_axis()

  if (!is.null(totalpop) && totalpop > 0)
  {
    labels = pretty(y_lim / totalpop * 100)
    axis(side = 4, las=2, at = labels * totalpop / 100, labels = paste0(labels, '%') )
    mtext("percent population", side = 4, line = 2, cex = 0.95)
  }
  
  if (!is.null(delta))
  {
    # https://stackoverflow.com/questions/27250542/how-to-make-gradient-color-filled-timeseries-plot-in-r
    sd_cols = colorRampPalette(c("white", "gray30"))(500)

    n = 500
    e = par('usr')
    xmax = e[2]; ymin = e[3]; ymax = e[4]
    # track sd changes through who1e period after start of lockdown
    width = (x_lim[2] - delta[1]) / n
    x_amt = seq(delta[1], x_lim[2], width)
    
    save_sd = sd_vals[as.integer(x_amt[1])]
    save_i = 1
    for (i in 1:n) {
	if (x_amt[i] > data_end) x_amt[i] = data_end
        this_sd = sd_vals[as.integer(x_amt[i])]
	if (!is.na(this_sd) && this_sd != save_sd) {
	    rect(x_amt[save_i], ymin, x_amt[save_i] + (width*(i - save_i + 1)), ymax, col=sd_cols[save_sd *500], border=NA)
	    save_sd = this_sd
	    save_i = i
	}
    }
    if (save_i != n) {
	    rect(x_amt[save_i], ymin, x_amt[save_i] + (width*(n - save_i + 1)), ymax, col=sd_cols[save_sd *500], border=NA)
    }
  }
  
  print(paste((x_lim[1]+1),x_lim[2],reff_data[doy==(x_lim[1]+1),1],reff_data[doy==(x_lim[2]),1]))
  col_min=2
  col_max=10
  for (i in col_min:col_max) 
  {
    found = 0
    for (j in x_lim[1]:x_lim[2])
    {
	if (!is.na(reff_data[doy==(j-1),i]) && reff_data[doy==(j-1),i] > 1 && 
	    !is.na(reff_data[doy==j,i]) && reff_data[doy==j,i] <= 1) 
	{
	    cross_date=j
	    found = 1
	    print(paste(i,cross_date))
	}
    }
    color_idx = i
    if (found == 1)
	points(x = cross_date, y = vac_data[doy==cross_date,i], col = col_pal[col_idx[color_idx]], pch = 16, cex=1.5)
	lines(x = c(cross_date,cross_date), y = c(0,vac_data[doy==cross_date,i]), col = col_pal[col_idx[color_idx]], lty = 1)
  }
  if (!is.null(vaccination_date)) {
    #points(x = vaccination_date, y = 0, col = "orange", pch = 12)
   lines(x = rep(vaccination_date,2), y = y_lim, col = "orange", lty = 2, lwd = 2)
  }
  box(bty = "l")
}
