# Title     : TODO
# Objective : TODO
# Created by: Matthew Chambers
# Created on: 7/4/2020

library(magrittr)
library(lubridate)
library(sufrep)
library(tidyverse)
library(xts)
library(plm)
library(AER)
library(caret)
library(grf)

# Set the directories and filenames for input, logging, and output (if relevant).
input_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/State files'
input_file <- 'Deryugina-QCEW_naics2-OSHA_RI.csv'
instrument_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Inversion data'
instrument_file <- 'inversions_all_years.csv'
log_dir <- 'C:/Users/Matthew Chambers/Desktop'
log_file <- 'causal_forest_implementation.log'

# Log output for later
# sink(file = file.path(log_dir, log_file), append = FALSE, type = 'output', split = FALSE)

# Read in the chosen data file, then begin manipulating it.
mydata <- read_csv(file.path(input_dir, input_file), col_types = "iiiiififdddddffdiddddi", progress = FALSE) %>%
  ## Draw a sample for testing, since doing anything on millions of examples is tedious. Obviously take this out later.
  #filter(year == 2010, month < 6) %>%
  # Use only slightly more recent data, so that reporting standards, etc. are a little more consistent.
  filter(year >= 2005) %>%
  # Select the variables I actually want to use.
  select(fips, naics, year, month_emplvl, month, day_of_month,
         PM25_conc, raw_prcp, raw_tmax, raw_tmin, wind_speed, num_injured) %>%
  rename(PM25 = PM25_conc) %>%
  # Create date objects from 3 date fields. Also add weekdays.
  mutate(date = make_date(year, month, day_of_month)) %>%
  mutate(weekday = wday(date, label = TRUE)) %>%
  # Replace NAs in num_injured with 0s, since they correspond to no accident occurring.
  mutate(num_injured = replace_na(num_injured, 0)) %>%
  # Drop observations with missing PM 2.5 values. I'll really want to start using the Di, et al. data
  # so I don't have to do this.
  drop_na(PM25) %>%
  # Drop observations with no reported employees in a given industry, since they aren't relevant.
  filter(month_emplvl > 0) %>%
  # Generate a dummy for the occurrence of an accident
  mutate(accident_occurred = num_injured > 0) %>%
  # Group by county-day, get total employment by county across all industries, generate percent employment
  # within each county by industry, then ungroup.
  group_by(fips, year, month, day_of_month) %>%
  mutate(tot_emplvl = sum(month_emplvl)) %>%
  mutate(pct_emplvl = month_emplvl / tot_emplvl) %>%
  ungroup()

# Designate the PM 2.5 data as panel data for calculating moving averages.
mydata %<>%
  mutate(panel_id = paste(mydata$fips, mydata$naics)) %>%
  pdata.frame(index = c('panel_id', 'date'))

# Lag PM2.5 for calculating moving averages. We can't use mutate because that would return an object
# that is not a pdata.frame
mydata$L1_PM25 <- lag(mydata$PM25, k = 1)

# Calculate moving averages, with square and cube for fitting quadratic and cubic functional forms
mydata %<>%
  mutate(MA2_PM25 = rowMeans(select(., PM25, L1_PM25), na.rm = FALSE))

# Calculate PM2.5 shock sizes, as the difference from the previous day's measure and as the difference
# from the 7-day moving average.
mydata %<>%
  mutate(shock_per_yesterday = PM25 - L1_PM25)

# Add in the inversion data
inversion_data <- read_csv(file.path(instrument_dir, instrument_file), col_types = "iDdddd", progress = FALSE)

# Make sure mydata$date is a date object, then join with the inversion data
mydata$date %<>% as_date()

mydata %<>%
  left_join(inversion_data) %>%
  mutate(inversion_binary = inversion_coverage >= 0.5)

#mydata$inversion_binary %<>% as.numeric()

encoder_x <- mydata %>%
  select(tot_emplvl, pct_emplvl, raw_prcp, raw_tmax, raw_tmin, wind_speed) %>%
  as.matrix()

encoder_g <- mydata %>%
  select(fips) %>%
  as.matrix() %>%
  as.factor()

encoder <- make_encoder(X = encoder_x, G = encoder_g, method = 'means')
fips_encoded_data <- encoder(X = encoder_x, G = encoder_g) %>%
  as_tibble() %>%
  select(starts_with('ENC')) %>%
  rename_all(function(x){paste0("fips_", x)})

# Next I encode naics using 'sufrep' again, with employment level and distribution variables
encoder_x <- mydata %>%
  select(tot_emplvl, pct_emplvl) %>%
  as.matrix()

encoder_g <- mydata %>%
  select(naics) %>%
  as.matrix() %>%
  as.factor()

encoder <- make_encoder(X = encoder_x, G = encoder_g, method = 'means')
naics_encoded_data <- encoder(X = encoder_x, G = encoder_g) %>%
  as_tibble() %>%
  select(starts_with('ENC')) %>%
  rename_all(function(x){paste0("naics_", x)})

mydata %<>%
  bind_cols(fips_encoded_data) %>%
  bind_cols(naics_encoded_data)

mydata %<>%
  na.omit()

X <- mydata %>%
  select(year, month, day_of_month, tot_emplvl, pct_emplvl, raw_prcp, raw_tmax, raw_tmin, wind_speed) %>%
  as.matrix()

Y <- mydata %>%
  pull(accident_occurred)

W <- mydata %>%
  pull(PM25)

Z <- mydata %>%
  pull(inversion_binary)

tau.forest <- instrumental_forest(X, Y, W, Z, num.trees = 4000)
average_late(tau.forest)
