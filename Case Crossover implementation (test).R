# Title     : Case Crossover implementation
# Objective : My initial attempt to implement a case crossover design. Dr. Pope says it's either clever or stupid. Let's find out.
# Created by: Matthew Chambers
# Created on: 6/12/2020


library(magrittr)
library(lubridate)
library(grf)
library(sufrep)
library(tidyverse)
library(xts)
library(plm)
library(survival)

# Set the random number generator seed, for replicability.
set.seed(42)

# Set the directory and filename.
data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/State files'
data_file <- 'Deryugina-QCEW_naics2-OSHA_RI.csv'
log_dir <- 'C:/Users/Matthew Chambers/Desktop'
log_file <- 'case_crossover_2005_forward.log'

# Log output for later use
sink(file = file.path(log_dir, log_file), append = FALSE, type = 'output', split = FALSE)

# Read in the chosen data file, then begin manipulating it.
mydata <- read_csv(file.path(data_dir, data_file), col_types = "iiiiififdddddffdiddddi") %>%
  # Draw a sample for testing, since doing anything on millions of examples is tedious. Obviously take this out later.
  filter(year == 2010) %>%
  # Select the variables I actually want to use.
  select(fips, naics, year, month_emplvl, month, day_of_month, poll_cluster,
         PM25_conc, angle, raw_prcp, raw_tmax, raw_tmin, wind_speed, num_injured) %>%
  # Create date objects from 3 date fields. Also add weekdays.
  mutate(date = make_date(year, month, day_of_month)) %>%
  mutate(weekday = wday(date, label = TRUE)) %>%
  # Replace NAs in num_injured with 0s, since they correspond to no accident occurring.
  mutate(num_injured = replace_na(num_injured, 0)) %>%
  # Drop observations with missing PM 2.5 values. I'll really want to start using the Di, et al. data
  # so I don't have to do this.
  drop_na(PM25_conc) %>%
  # Drop observations with no reported employees in a given industry, since they aren't relevant.
  filter(month_emplvl > 0) %>%
  # Generate a dummy for the occurrence of an accident
  mutate(accident_occurred = num_injured > 0) %>%
  # Group by county-day, get total employment by county across all industries, generate percent employment
  # within each county by industry, then ungroup.
  group_by(fips, year, month, day_of_month) %>%
  mutate(tot_emplvl = sum(month_emplvl)) %>%
  mutate(pct_emplvl = month_emplvl / tot_emplvl) %>%
  ungroup() %>%
  # Select case and referent observations. Group by fips, naics, year, month, weekday, then flag
  # by any(accident_occurred). This follows Jones, et al. (2005).
  group_by(fips, naics, year, month, weekday) %>%
  mutate(case_or_control = any(accident_occurred)) %>%
  ungroup()

# Designate the PM 2.5 data as panel data for calculating moving averages.
mydata %<>%
  mutate(L_PM25_conc = PM25_conc) %>%
  mutate(panel_id = paste(mydata$fips, mydata$naics)) %>%
  pdata.frame(index = c('panel_id', 'date'))

# Lag PM2.5 for calculating moving averages.
mydata$L_PM25_conc <- lag(mydata$PM25_conc, k = 1)

# Calculate moving averages.
mydata %<>%
  mutate(MA_PM25_conc = rowMeans(select(., PM25_conc, L_PM25_conc), na.rm = FALSE))

# Get a data frame of only case and referent observations.
mydata_small <- mydata %>%
  filter(case_or_control) %>%
  mutate(event_id = paste(panel_id, year, month))

# Do a conditional logit regression to implement the case-crossover design
clogit_estimates <- mydata_small %>%
  clogit(accident_occurred ~ MA_PM25_conc + raw_prcp + raw_tmax + raw_tmin + wind_speed + strata(event_id), .)

confidence_intervals <- clogit_estimates %>%
  confint(parm = 'MA_PM25_conc')

clogit_estimates

confidence_intervals
