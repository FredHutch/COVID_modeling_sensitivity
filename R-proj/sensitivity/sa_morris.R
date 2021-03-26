library(sensitivity)
library(ggplot2)
library(RColorBrewer)

# let's do a morris sensitivity analysis

n_traj = 200
morris_design = morris(model = NULL, factors = 11, r = c(n_traj), design = list(type = "oat", levels = 10, grid.jump = 5))
dim(morris_design$X)

write.csv(morris_design$X, "../data/morris_11p_200r.csv", quote = FALSE, row.names = FALSE)

# functions needed to interpret morris results

get_morris_output = function(design_matrix, simulation_results)
{
  param_names = colnames(design_matrix) # without simID
  levels = length(unique(c(design_matrix[,1]))) # just number of levels and assume same for all params
  lower_bound = apply(design_matrix, 2, min)
  upper_bound = apply(design_matrix, 2, max)
  
  morris_full = morris(model = NULL, factors = param_names, r = n_traj, 
                       design = list(type = "oat", levels = levels, grid.jump = levels / 2),
                       binf = lower_bound, bsup = upper_bound, scale = TRUE)
  
  # now update morris_full object with current data 
  morris_full$r = nrow(design_matrix) / (ncol(design_matrix) + 1)
  morris_full$X = design_matrix
  morris_out = tell(morris_full, as.matrix(simulation_results)) 
  
  return(morris_out)
}

# calculate Morris, mu, mu.star, and sigma code from help file
calculate_mu = function(elementaryEffects)
{
  return(calculate_morris_metric(elementaryEffects, mean))
}
calculate_mu.star = function(elementaryEffects)
{
  return(calculate_morris_metric(abs(elementaryEffects), mean))
}
calculate_sigma = function(elementaryEffects)
{
  return(calculate_morris_metric(elementaryEffects, sd))
}

calculate_percent_positive = function(elementaryEffects)
{
  percent_pos = function(x) {sum(x > 0) / length(x) }
  
  return(calculate_morris_metric(elementaryEffects, percent_pos))
}

calculate_morris_metric = function(elementaryEffects, FUN)
{
  if (is.matrix(elementaryEffects))
  {
    metric = apply(elementaryEffects, 2, FUN)
  }
  else if (is.array(elementaryEffects) & length(dim(elementaryEffects)) == 3)
  {
    metric =  apply(elementaryEffects, 3, function(M){
      apply(M, 2, FUN)
    })
  }   
  else if (is.array(elementaryEffects) & length(dim(elementaryEffects)) == 4)  # results (y) a 3-dim array
  {
    metric = sapply(1:dim(elementaryEffects)[4], function(i){
      apply(elementaryEffects[, , , i, drop = FALSE], 3, function(M){
        apply(M, 2, FUN)
      })
    }, simplify = "array")
  }
  else { stop("elementaryEffects must be matrix, or 3- or 4-dimensional array") }
  return(metric)
}

############ read in and analyze results ###########################################

# here are the various SA's over time
morris_file = "morris_sensitivity_v11"
morris_file = "morris_2_25"
morris_file = "morris_3_2"

morris_sims = read.csv(paste0("out/", morris_file, ".csv"))


metrics = c("cases.since.vax", "hosp.since.vax", "deaths.since.vax", "max.daily.cases", "max.current.hosp", "max.daily.deaths", 
            "tot.infs", "tot.cases", "tot.hosp", "tot.deaths", "avg.SD", "days.at.max.SD")
metric_names = c("Diagnosed cases since vaccination", "Hospitalizations since vaccination", "Deaths since vaccination",
                 "Maximum diagnosed cases", "Maximum hospitalizations", "Maximum deaths",
                 "Total infections", "Total diagnosed cases", "Total hospitalizations", "Total deaths",
                 "Average social distancing", "Days at maximum social distancing")
params = c("min_sd", "max_sd", "ve_s", "ve_p", "ve_i", "vac_rate", 
           "new_strain_fact", "loosen", "tighten", "coverage", "import.rate")
param_names = c("Minimum social distancing", "Maximum social distancing", "VEsusc", "VEsympt", "VEinf", "Vaccination rate",
                "New strain infectivity", "Lockdown release case threshold","Lockdown case threshold", "Vaccination coverage", "New strain import rate")


#morris_plan =read.csv("../data/morris_11p_200r.csv")
# read plan directly from what was run
morris_plan = morris_sims[, params]

colnames(morris_plan) = param_names
morris_out = get_morris_output(morris_plan, morris_sims[,metrics])

mu = calculate_mu(morris_out$ee)
mu.star = calculate_mu.star(morris_out$ee)
sigma = calculate_sigma(morris_out$ee)

write.csv(mu.star, paste0("out/morris_mu.star_", morris_file, ".csv"), quote = FALSE)
write.csv(sigma, paste0("out/morris_sigma_", morris_file, ".csv"), quote = FALSE)

draw_lines = function(both = FALSE)
{
  abline(a = 0, b = 0.1, lty = 2, col = alpha("black", 0.5))
  abline(a = 0, b = 0.5, lty = 3, col = alpha("black", 0.6))
  abline(a = 0, b = 1, lty = 4, col = alpha("black", 0.5))
  
  if (both)
  {
    abline(v = 0,lty = 1, col = alpha("black", 0.2))
    
    abline(a = 0, b = -0.1, lty = 2, col = alpha("black", 0.5))
    abline(a = 0, b = -0.5, lty = 3, col = alpha("black", 0.6))
    abline(a = 0, b = -1, lty = 4, col = alpha("black", 0.5))
  }
  
}

cols = brewer.pal(12, "Paired")[-11]
pchs = 1:11
pch_cex = rep(1, length(pchs))
pch_cex[which(params %in% c("max_sd", "vac_rate", "new_strain_fact", "tighten"))] = 2

draw_legend = function()
{
  legend("left", legend = param_names, col = cols, pch = pchs, pt.cex = pch_cex, bty = "n")
}

plot_mu_vs_sigma = function(metric = c("mu.star", "mu"), out_index, title = TRUE)
{
  out_metric = if (metric == "mu.star") {
    mu.star[,out_index]
  } else if (metric == "mu") {
    mu[,out_index]
  } else { stop("unknown metric") }
  
  x_lim = if (metric == "mu.star") {
    c(0, max(mu.star[,out_index], sigma[,out_index]) * 1.05)
  } else if (metric == "mu") {
    c(-1,1) * max(abs(mu.star[,out_index]) * 1.05)
  }
  
  y_lim = if (metric == "mu.star") {
    x_lim
  } else if (metric == "mu") {
    c(0, max(sigma[,out_index]) * 1.05)
  }  
  
  plot(out_metric, sigma[,out_index], #asp = 1,
       pch = pchs, cex = pch_cex, col = cols, 
       xlim = x_lim, ylim = y_lim, 
       xlab = "", ylab = "", bty = "l", cex.lab = 1)
  
  draw_lines(both = (metric == "mu"))
  
  #  text(mu.star[,out_index], sigma[,out_index], labels = row.names(mu), 
  #       pos = text_pos, offset = 0.25, cex = 0.8, xpd = TRUE)
  
  if (title)
  {
    mtext(text = metric_names[out_index], side = 3, line = 1)
  }
}

########## plots #########################################################

# now need to loop over each output column and plot mu vs sigma
pdf(paste0("out/morris_sa_mu.star_", morris_file, ".pdf"), width = 12, height = 9)
par(mfrow = c(3, 4), mgp=c(2.3,0.45,0), tcl=-0.4, mar=c(3.6,3.6,2.3,2.3), oma = c(2, 2, 1, 0), las = 1)

text_pos = list(4, 4, 4, c(4, 4, 2, 4, 4, 4, 4), 
                c(2, 4, 4, 4, 4, 3, 4), 4, c(4, 4, 2, 4, 4, 4, 4), c(4, 4, 4, 4, 2, 4, 4), 
                c(4, 4, 4, 2, 4, 4, 4), 4, 4, c(2, 3, 4, 4, 1, 4, 4))
text_pos = 4

for (i in 1:ncol(mu))
{
  plot_mu_vs_sigma("mu.star", i)
}

mtext(text="Linear effects (mu.star)", side = 1, line = 0, cex = 1.2, outer = TRUE)
mtext(text="Nonlinear/interaction effects (sigma)", side = 2, line = 0, las = 0, cex = 1.2, outer = TRUE)

dev.off()

####### mu ######

pdf(paste0("out/morris_sa_mu_", morris_file, ".pdf"), width = 12, height = 9)
par(mfrow = c(3, 4), mgp=c(2.3,0.45,0), tcl=-0.4, mar=c(3.6,3.6,2.3,2.3), oma = c(2, 2, 1, 0), las = 1)

text_pos = 4

for (i in 1:ncol(mu))
{
  plot_mu_vs_sigma("mu", i)
}

mtext(text="Linear effects (mu)", side = 1, line = 0, cex = 1.2, outer = TRUE)
mtext(text="Nonlinear/interaction effects (sigma)", side = 2, line = 0, las = 0, cex = 1.2, outer = TRUE)

dev.off()



pdf(paste0("out/morris_sa_summary_", morris_file, ".pdf"), width = 10, height = 5)
par(mfrow = c(2,4), mgp=c(2.3,0.45,0), tcl=-0.4, mar=c(3,3,1,0), oma = c(0.5, 3.5, 2, 0), las = 1)

for (i in which(colnames(mu) %in% c("tot.cases", "tot.deaths", "days.at.max.SD")))
{
  plot_mu_vs_sigma("mu.star", i, TRUE)
}

plot(0, 0, axes = FALSE, ann = FALSE, type = "n")
draw_legend()

for (i in which(colnames(mu) %in% c("tot.cases", "tot.deaths", "days.at.max.SD")))
{
  plot_mu_vs_sigma("mu", i, FALSE)
}

plot(0, 0, axes = FALSE, ann = FALSE, type = "n")

mtext(text="Linear effects (mu.star/mu)", side = 1, line = -1, cex = 1, outer = TRUE)
mtext(text="Nonlinear/interaction\neffects (sigma)", side = 2, line = 0.5, las = 0, cex = 1, outer = TRUE)

dev.off()


