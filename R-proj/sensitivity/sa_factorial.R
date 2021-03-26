library(dplyr)


sa_output = read.csv("out/full_sensitivity.csv")
sa_output = read.csv("out/fixed_trigger_sens.csv")
sa_output = read.csv("out/new_strain_sens_v3.csv") # with new strain at 55%
sa_output = read.csv("out/high_low_trig_sens.csv") # "loosen" field (20,60,100) for release from lockdown
sa_output = read.csv("out/high_low_trig_sens_v2.csv") # fix max_sd
sa_output = read.csv("out/new_sensitivity.csv") # recalibration for rho_S

# need to get just high-low for a 2-level design, also handle scenario separately
# update to just 6 factors
sa_out_2_lev = sa_output %>% 
  filter(min_sd == min(min_sd) | min_sd == max(min_sd)) %>%
  filter(loosen == min(loosen) | loosen == max(loosen)) %>%
  filter(ve_s == min(ve_s) | ve_s == max(ve_s)) %>%
  filter(ve_p == min(ve_p) | ve_p == max(ve_p)) %>%
  filter(ve_i == min(ve_i) | ve_i == max(ve_i)) %>%
  filter(vac_rate == min(vac_rate) | vac_rate == max(vac_rate)) %>%
  filter(tighten == min(tighten) | tighten == max(tighten)) # or set to 800 to match 3-level

# now let's make a 3-level design, NOTE for now these are not quite equally spaced, but close
# unequal for ve_i, 
sa_out_3_lev = sa_output %>% filter(tighten %in% c(200, 500, 800)) %>% 
          filter(vac_rate %in% c(2000, 5000, 8000))

map_to_1s = function(vector_2_levels) { ifelse(vector_2_levels == max(vector_2_levels), 1, -1) }
map_to_01s = function(vector_3_levels) { match(vector_3_levels, c(min(vector_3_levels), sort(unique(vector_3_levels))[2], max(vector_3_levels))) - 2 } # match with give indices 1, 2, 3, then subtract 2 to get -1, 0, 1

# translate max to 1 and min to -1, these are the 7 parameters, A=tighten, B=min_sd, etc
sa_design_2 = data.frame(A = map_to_1s(sa_out_2_lev$tighten),
                         B = map_to_1s(sa_out_2_lev$loosen),
                         C = map_to_1s(sa_out_2_lev$min_sd),
                         D = map_to_1s(sa_out_2_lev$ve_s),
                         E = map_to_1s(sa_out_2_lev$ve_p),
                         F = map_to_1s(sa_out_2_lev$ve_i),
                         G = map_to_1s(sa_out_2_lev$vac_rate))
sa_design_3 = data.frame(A = map_to_01s(sa_out_3_lev$tighten),
                         B = map_to_01s(sa_out_3_lev$loosen),
                         C = map_to_01s(sa_out_3_lev$min_sd),
                         D = map_to_01s(sa_out_3_lev$ve_s),
                         E = map_to_01s(sa_out_3_lev$ve_p),
                         F = map_to_01s(sa_out_3_lev$ve_i),
                         G = map_to_01s(sa_out_3_lev$vac_rate))
param_names = c("tighten", "loosen", names(sa_output)[2:7])
param_letters = names(sa_design_2)
n_param = length(param_names)

############ 2-level analysis ####################################################

# now add all possible combinations to design matrix
for (n in 2:ncol(sa_design_2))
{
  combos = combn(param_letters, n) # each col is a combination
  
  for (j in 1:ncol(combos))
  {
    # select columns in combo, then multiply tehm to get design value for interaction
    sa_design_2$X = apply(sa_design_2[, combos[,j]], 1, prod)
    names(sa_design_2)[names(sa_design_2) == "X"] = paste0(combos[,j], collapse = "")
  }
}

calc_effect = function(design_vec, result_vec) { sum(design_vec * result_vec) / length(design_vec) }

# which output columns to consider?
# use this to examine all outcomes
out_col = 8:48 # which columns in sa_output are outputs not vars
out_col = out_col[!out_col %in% which(names(sa_output) %in% c("tighten", "loosen"))]

# use this to get rid of year 2 also
out_col = out_col[!out_col %in% which(grepl("yr2", names(sa_output)))]

# and finally, here are just the outcomes in the paper
metrics = c("days.at.max.SD", "avg.SD", "cases.since.vax", "hosp.since.vax", "deaths.since.vax", "max.daily.cases", "max.current.hosp", "max.daily.deaths", "tot.cases", "tot.hosp", "tot.deaths")
out_col = which(names(sa_output) %in% metrics)

sa_results = data.frame(matrix(nrow = length(out_col), ncol = ncol(sa_design_2),
                               dimnames = list(names(sa_out_2_lev)[out_col], names(sa_design_2))))

for (r in 1:nrow(sa_results))
{
  for (c in 1:ncol(sa_results))
  {
    sa_results[r,c] = calc_effect(sa_design_2[,c], sa_out_2_lev[,out_col[r]])
  }
}

plot_output_norm = function(parameter_effects, title = NA, remove_largest_effect = FALSE)
{
  parameter_effects = unlist(parameter_effects)
  parameter_effects = sort(parameter_effects)
  
  if(remove_largest_effect)
  {
    # this removes vac_total which is always the biggest
    idx_to_remove = which.max(abs(parameter_effects))
    removed = names(parameter_effects)[idx_to_remove]
    parameter_effects = parameter_effects[-idx_to_remove]
  }
  labels = names(parameter_effects)
  theor_quan = qnorm(ppoints(length(parameter_effects)))
  
  plot(theor_quan, parameter_effects, bty = "l",
       xlab = "theoretical quantiles", ylab = "parameter effect", main = title)
  abline(v = 0, col = "gray90")
  abline(h = 0, col = "gray90")
  qqline(parameter_effects, col = "gray60")
  
  last5 = (length(parameter_effects) - 4):length(parameter_effects)
  text(theor_quan[1:5], parameter_effects[1:5], label=labels[1:5], pos=3, cex=0.8)
  text(theor_quan[last5], parameter_effects[last5], label=labels[last5], pos=1, cex=0.8)
  
  if(remove_largest_effect)
  {
    legend("bottomright", pch = NA, legend = paste("not shown:", removed), bty = "n")
  }
}

legend_param_name_and_val = function()
{
  plot(0, 0, type = "n", axes = FALSE, bty = "n", xlab ="", ylab = "")
  param_ranges = apply(apply(sa_out_2_lev[,param_names], 2, range), 2, paste0, collapse = ",")
  legend("left", pch = NA, legend = paste(paste(param_letters, param_names, sep = "="), " (", param_ranges, ")", sep = ""), 
         bty = "n", xpd = TRUE)
}

pdf("out/factorial_param_effects.pdf", width = 8, height = 6)
par(mfrow = c(3, 4), mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
for (out_name in row.names(sa_results))
{
  plot_output_norm(sa_results[out_name,], title = out_name)
}
legend_param_name_and_val()
dev.off()

pdf("out/factorial_param_effects_remove_biggest.pdf", width = 8, height = 6)
par(mfrow = c(3, 4), mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
for (out_name in row.names(sa_results))
{
  plot_output_norm(sa_results[out_name,], title = out_name, remove_largest_effect = TRUE)
}
legend_param_name_and_val()
dev.off()

write.csv(sa_results, "out/factorial_param_effects.csv", quote = FALSE)


# calculate total effect
sa_results_total = data.frame(matrix(nrow = length(out_col), ncol = length(param_letters),
                                     dimnames = list(names(sa_out_2_lev)[out_col], param_letters)))
for (p in names(sa_results_total))
{
  col_idxs = grepl(p, names(sa_results), fixed = TRUE)
  sa_results_total[,p] = rowSums(sa_results[,col_idxs])
}

pdf("out/factorial_tot_param_effects.pdf", width = 8, height = 6)
par(mfrow = c(3, 4), mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
for (out_name in row.names(sa_results))
{
  barplot(unlist(sa_results_total[out_name,]), main = out_name,
          names.arg=names(sa_results_total))
}
legend_param_name_and_val()
dev.off()

pdf("out/factorial_main_param_effects.pdf", width = 8, height = 6)
par(mfrow = c(3, 4), mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
for (out_name in row.names(sa_results))
{
  barplot(unlist(sa_results[out_name, 1:n_param]), main = out_name,
          names.arg=names(sa_results)[1:n_param])
}
legend_param_name_and_val()
dev.off()

pdf("out/factorial_second_param_effects.pdf", width = 12, height = 6)
par(mfrow = c(3, 4), mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))

eff_2order = names(sa_results)[nchar(names(sa_results)) == 2]

for (out_name in row.names(sa_results))
{
  barplot(unlist(sa_results[out_name, which(names(sa_results) %in% eff_2order)]), main = out_name,
          names.arg=eff_2order, las = 2)
}
legend_param_name_and_val()
dev.off()

############ 3-level analysis ####################################################

# try to just fit a model to cases
mod_3order = aov(sa_out_3_lev$cases.since.vax ~ (A + B + C + D + E + F + G)^3, data = sa_design_3)
summary(mod_3order)

# stepwise regression to see which are sig
mod_3order_red = step(mod_3order, direction = "backward")
summary(mod_3order_red)

# model with all possible interactions
mod_all = aov(sa_out_3_lev$cases.since.vax ~ (A + B + C + D + E + F + G)^7, data = sa_design_3)
summary(mod_all)

all_effects = mod_all$effects
all_effects = all_effects[-1] # remove intercept
all_effects = sort(all_effects)
all_effects_names = names(all_effects)

plot_output_norm(all_effects, title = "3-level cases.since.vax")

########## plots ################
library(lattice)

# remake without maopping to -1,0,1 so that the labels will be param levels
# this ended up just looking really messy so decided not to use it
#sa_design_2 = sa_out_2_lev %>% select(tighten, min_sd, ve_s, ve_p, ve_i, vac_rate)
#sa_design_3 = sa_out_3_lev %>% select(tighten, min_sd, ve_s, ve_p, ve_i, vac_rate)

metrics = c("cases.since.vax", "hosp.since.vax", "deaths.since.vax", "max.daily.cases", "max.current.hosp", "max.daily.deaths", 
            "tot_infs", "tot.cases", "tot.hosp", "tot.deaths", "avg.SD", "days.at.max.SD")
metric_names = c("Diagnosed cases since vaccination", "Hospitalizations since vaccination", "Deaths since vaccination",
                 "Maximum diagnosed cases", "Maximum hospitalizations", "Maximum deaths",
                 "Total infections", "Total diagnosed cases", "Total hospitalizations", "Total deaths",
                 "Average social distancing", "Days at maximum social distancing")

# X since vaccination 
out_var_name = "cases.since.vax"
title = "Diagnosed cases since vaccination"

out_var_name = "hosp.since.vax"
title = "Hospitalizations since vaccination"

out_var_name = "deaths.since.vax"
title = "Deaths since vaccination"

# totals
out_var_name = "tot_infs"
title = "Total infections"

out_var_name = "tot.cases"
title = "Total diagnosed cases"

out_var_name = "tot.hosp"
title = "Total hospitalizations"

out_var_name = "tot.deaths"
title = "Total deaths"

# max daily
out_var_name = "max.daily.cases"
title = "Maximum daily diagnosed cases"

out_var_name = "max.current.hosp"
title = "Maximum hospitalizations"

out_var_name = "max.daily.deaths"
title = "Maximum daily deaths"

# days at max SD
out_var_name = "days.at.max.SD"
title = "Days at maximum social distancing"

out_var_name = "avg.SD"
title = "Average social distancing"


# 1st order effects
# use actual values instead of -1, 0, 1 for plot
group = rep(param_names, each=nrow(sa_out_3_lev))
out_var = rep(sa_out_3_lev[,out_var_name], length(param_names))
#level = c(sa_design_3$A, sa_design_3$B, sa_design_3$C, sa_design_3$D, sa_design_3$E, sa_design_3$F, sa_design_3$G)
level = c(sa_out_3_lev$tighten, sa_out_3_lev$loosen, sa_out_3_lev$min_sd, sa_out_3_lev$ve_s, sa_out_3_lev$ve_p, sa_out_3_lev$ve_i, sa_out_3_lev$vac_rate)
dflong = data.frame(group,level,out_var)

dfp = aggregate(x=dflong$out_var,by=list(dflong$group,dflong$level),FUN="mean")
names(dfp)=c("group","level","tmean")
dfp$group = factor(dfp$group, levels = unique(dflong$group))
dfp = dfp[order(dfp$group, dfp$level),]

pdf(paste0("out/factorial_3-level_main_param_effects_", out_var_name, ".pdf"), width = 10, height = 6)
xyplot(tmean ~ level|group, data=dfp,layout=c(n_param,1), xlim=c(-2,2),
       ylab=title,xlab="Factor Levels", type="b",
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.abline(h = mean(out_var), lty = 2, col = 2)})
dev.off()

devtools::install_github("zeehio/facetscales")
library(g)
library(facetscales)

scales_x <- list(
  `tighten` = scale_x_continuous(limits = c(0.9, 1.1) * range(dfp$level[dfp$group == "tighten"]), 
                                 breaks = dfp$level[dfp$group == "tighten"]),
  `loosen` = scale_x_continuous(limits = c(0.9, 1.1) * range(dfp$level[dfp$group == "loosen"]), 
                                breaks = dfp$level[dfp$group == "loosen"]),
  `min_sd` = scale_x_continuous(limits = c(0.9, 1.1) * range(dfp$level[dfp$group == "min_sd"]), 
                                breaks = dfp$level[dfp$group == "min_sd"]),
  `ve_s` = scale_x_continuous(limits = c(0.9, 1.1) * range(dfp$level[dfp$group == "ve_s"]), 
                              breaks = dfp$level[dfp$group == "ve_s"]),
  `ve_p` = scale_x_continuous(limits = c(0.9, 1.1) * range(dfp$level[dfp$group == "ve_p"]), 
                              breaks = dfp$level[dfp$group == "ve_p"]),
  `ve_i` = scale_x_continuous(limits = c(0.9, 1.1) * range(dfp$level[dfp$group == "ve_i"]), 
                              breaks = dfp$level[dfp$group == "ve_i"]),
  `vac_rate` = scale_x_continuous(limits = c(0.9, 1.1) * range(dfp$level[dfp$group == "vac_rate"]), 
                                  breaks = dfp$level[dfp$group == "vac_rate"])
)
pdf(paste0("out/factorial_3-level_main_param_effects_", out_var_name, ".pdf"), width = 10, height = 6)
ggplot(dfp, aes(level, tmean)) +
  geom_hline(yintercept = mean(out_var), color = "gray80") +
  geom_line() +
  geom_point() +
  ylab(title) + xlab("Factor Levels") +
 # scale_x_continuous(breaks = bfun, expand=expand_scale(0,0)) +
 # facet_grid(~group, scales = "free")) +
  facet_grid_sc(cols = vars(group), scales = list(x = scales_x)) +
#  theme(panel.spacing.x = unit(5, "lines")) + # spacing between panels
  theme_classic()
dev.off()

# 2nd order effects (for now do it on the 2-level analysis)
int_2order = names(sa_design_2)[nchar(names(sa_design_2)) == 2]
group2 = rep(int_2order, each=nrow(sa_out_2_lev))
out_var2 = rep(sa_out_2_lev[,out_var_name], length(int_2order))
level2 = unlist(sa_design_2[,int_2order])
df2way = data.frame(group2,level2,out_var2)

dfp2 = aggregate(x=df2way$out_var2, by=list(df2way$group2,df2way$level2), FUN="mean")
names(dfp2)=c("group","level","tmean")

pdf(paste0("out/factorial_2-level_interaction_param_effects_", file_suffix, ".pdf"), width = 10, height = 10)
sp = c(F,F,F,F,F,F, T,F,F,F,F,F, T,T,F,F,F,F, T,T,T,F,F,F, T,T,T,T,F,F, T,T,T,T,T,F )
strip.bg_custom = trellis.par.get("strip.background")
strip.bg_custom$col =c("#cce6ff","#ffe5cc","#ccffcc","#ccffff","#ffccff",
                       "#ffcccc","#ffffcc")
strip.sh_custom = strip.bg_custom
trellis.par.set("strip.background", strip.bg_custom)
trellis.par.set("strip.shingle", strip.sh_custom)
xyplot(tmean~level | group, data=dfp2, type="b", xlim=c(-2,2),
       layout=c(n_param-1, n_param-1), skip=sp, col=c(4), #ylim=c(1900,1935),
       # strip = function(..., style,factor.levels,strip.levels,strip.names)
       #   strip.default(..., style = 1,factor.levels=dfp2$group,
       #                 strip.levels=c(F,T),strip.names=c(T,F)),
       xlab="Factor Level", ylab=title,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.abline(h = mean(out_var2), lty = 2, col = 2)})
dev.off()


# now 2nd order effects plotted with all 3-level combinations

dw2all = NULL
for (p1 in param_letters)
{
  for (p2 in param_letters)
  {
    if (p1 != p2)
    {
      interacs = aggregate(sa_out_3_lev[,out_var_name], 
                           by = list(sa_design_3[,p1], sa_design_3[,p2]), 
                           FUN = "mean")
      gname = paste(param_names[which(param_letters == p1)], param_names[which(param_letters == p2)], sep = ":")
      interacs = cbind(gi = paste0(p1, p2), gname = gname, interacs)
      dw2all = rbind(dw2all, interacs)
    }
  }
}


pdf(paste0("out/factorial_3-level_full_interaction_param_effects_", out_var_name, ".pdf"), width = 10, height = 10)
sp = c(T,F,F,F,F,F,F, F,T,F,F,F,F,F, F,F,T,F,F,F,F, F,F,F,T,F,F,F, F,F,F,F,T,F,F, F,F,F,F,F,T,F, F,F,F,F,F,F,T)
xyplot(x ~ Group.1 | gi, data=dw2all, group=Group.2,
       layout=c(n_param, n_param), skip=sp, xlim=c(-2,2),
       ylab = title, xlab = "Factor Level",
       main = "Rows: x-axis, Columns: color (darker = higher)",
       type=c("p","l"), pch=20, cex=1, col=c("#bdd7e7", "#6baed6", "#2171b5"),
       panel=function(x,y,...){panel.superpose(x,y,...)})
trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]

panel.text(0.15, 0.15, paste(param_letters[1], param_names[1], sep = "="), cex=1)
panel.text(0.275, 0.275, paste(param_letters[2], param_names[2], sep = "="),  cex=1)
panel.text(0.375, 0.375, paste(param_letters[3], param_names[3], sep = "="), cex=1)
panel.text(0.5, 0.5, paste(param_letters[4], param_names[4], sep = "="), cex=1)
panel.text(0.625, 0.625, paste(param_letters[5], param_names[5], sep = "="), cex=1)
panel.text(0.725, 0.725, paste(param_letters[6], param_names[6], sep = "="), cex=1)
panel.text(0.85, 0.85, paste(param_letters[7], param_names[7], sep = "="),  cex=1)
trellis.unfocus()
dev.off()

# alternative with raw data (but transformed to low, med, high) so some params have 4 values
convert_levels = function(numeric_levels) { if(length(numeric_levels) == 3) { c("low", "med", "high") } else { c("low", "med-low", "med-high", "high") } }

dw2all = NULL
for (p1 in param_names)
{
  for (p2 in param_names)
  {
    if (p1 != p2)
    {
      interacs = aggregate(sa_output[,out_var_name], 
                           by = list(sa_output[,p1], sa_output[,p2]), 
                           FUN = "mean")
      gname = paste(p1, p2, sep = ":")
      interacs = cbind(gi = paste0(p1, p2), gname = gname, interacs)
      level_1 = sort(unique(interacs$Group.1))
      level_name_1 = convert_levels(level_1)
      level_2 = sort(unique(interacs$Group.2))
      level_name_2 = convert_levels(level_2)
      interacs$Group.1 = plyr::mapvalues(interacs$Group.1, level_1, level_name_1)
      interacs$Group.2 = plyr::mapvalues(interacs$Group.2, level_2, level_name_2)
      dw2all = rbind(dw2all, interacs)
    }
    else
    {
      # interacs = cbind(gi = paste0(p1, p2), gname = p1, Group.1 = min(sa_output[,p1]), Group.2 = "low", x = NA)
      # dw2all = rbind(dw2all, interacs)
    }
  }
}
# add empty AA, BB etc as placeholders
for (p1 in param_names)
{
  for (p2 in param_names)
  {
    if (p1 == p2)
    {
      level_name = convert_levels(sort(unique(sa_output[,p1])))
      interacs = cbind(gi = paste0(p1, p2), gname = p1, Group.1 = level_name, Group.2 = level_name, x = NA)
      dw2all = rbind(dw2all, interacs)
    }
  }
}
dw2all$gi = factor(dw2all$gi, levels = sort(as.character(unique(dw2all$gi))))
dw2all$gname = factor(dw2all$gname, levels = unique(dw2all$gname[order(dw2all$gi)]))
dw2all$Group.1 = ordered(dw2all$Group.1, levels = c("low", "med-low", "med", "med-high", "high"))
dw2all$Group.2 = ordered(dw2all$Group.2, levels = c("low", "med-low", "med", "med-high", "high"))
dw2all$x = as.numeric(dw2all$x)

pdf(paste0("out/factorial_2nd_order_interaction_", out_var_name, ".pdf"), width = 10, height = 10)
ggplot(dw2all, aes(x = Group.1, y = x, color = Group.2, group = Group.2)) +
  geom_line() +
  #stat_summary(fun.y=sum, geom="line") +
  geom_point() +
  ggtitle(title) +
  ylab(title) + xlab("Factor Levels (1st parameter)") +
  scale_x_discrete(labels = c("low", "", "med", "", "high")) +
  facet_wrap(~gname) + #, scales = "free_x"
  theme_classic() +
  labs(color = "2nd parameter") 
  
dev.off()

#--------------------------
# correlation plot
metrics = c("days.at.max.SD", "avg.SD", "cases.since.vax", "hosp.since.vax", "deaths.since.vax", "max.daily.cases", "max.current.hosp", "max.daily.deaths", "tot.cases", "tot.hosp", "tot.deaths")

library("PerformanceAnalytics")

pdf("out/metrics_correlation.pdf", width = 10, height = 10)
chart.Correlation(sa_output[,metrics], histogram=TRUE, pch=19, method = "spearman")
dev.off()

# make another for the parameters vs certain metrics


