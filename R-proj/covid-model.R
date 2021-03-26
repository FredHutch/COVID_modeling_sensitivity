# COVID-19 infection model ####

# Load required libraries ####
library(deSolve)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(paletteer)

# Load model functions ####
filepath <- "functions/"
#filepath <- "R-proj/functions/"
# TODO if we're going to organize one function per file like this we could just make it an R package
source(paste0(filepath,"model_eq.R"))
source(paste0(filepath,"monthly_params.R"))
source(paste0(filepath,"shape_data.R"))
source(paste0(filepath,"plot_output.R"))
source(paste0(filepath,"fit.R"))


