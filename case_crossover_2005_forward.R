# Title     : Implementation of Case Crossover study design
# Objective : See title
# Created by: Matthew Chambers
# Created on: 6/22/2020

# Designate the path where the necessary package(s) are installed
.libPaths('~/r_3.6.0_lib')

# Load the necessary package for conditional logistic regression
library(survival)
library(magrittr)
library(tidyverse)

# Define the location of my data and output directories
data_dir <- '~/case_crossover/data'
data_file <- '2005_forward_all_states_for_case_crossover.csv'
log_dir <- '~/case_crossover/analysis/r_logs'
log_file <- '2005_forward_all_states_for_case_crossover.log'

# Log output for later
sink(file = file.path(log_dir, log_file), append = FALSE, type = 'output', split = FALSE)

# Read in the data file
mydata <- read_csv(file.path(data_dir, data_file), col_types = "iiiiiifddddddiDflidldcdc")

# Do a conditional logit regression to implement the case-crossover design
clogit_estimates <- mydata %>%
  clogit(accident_occurred ~ MA_PM25_conc + raw_prcp + raw_tmax + raw_tmin + wind_speed + strata(event_id), .)

confidence_intervals <- clogit_estimates %>%
  confint(parm = 'MA_PM25_conc')

print(clogit_estimates)

print(confidence_intervals)
