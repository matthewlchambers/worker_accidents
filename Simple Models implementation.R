# Title     : Implementing simple models to test for uncomplicated results
# Objective : As recommended by a committee member, look for results in simple models first.
# Created by: Matthew Chambers
# Created on: 6/22/2020

library(magrittr)
library(lubridate)
library(sufrep)
library(tidyverse)
library(xts)
library(plm)
library(AER)
library(multiwayvcov)
library(lmtest)
library(ivpack)

# Set the directories and filenames for input, logging, and output (if relevant).
input_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/State files'
input_file <- 'Deryugina-QCEW_naics2-OSHA_PA.csv'
log_dir <- 'C:/Users/Matthew Chambers/Desktop'
log_file <- 'simple_models_implementation.log'

# Log output for later
sink(file = file.path(log_dir, log_file), append = FALSE, type = 'output', split = FALSE)

# Read in the chosen data file, then begin manipulating it.
mydata <- read_csv(file.path(input_dir, input_file), col_types = "iiiiififdddddffdiddddi", progress = FALSE) %>%
  ## Draw a sample for testing, since doing anything on millions of examples is tedious. Obviously take this out later.
  #filter(year == 2010) %>%
  # Use only slightly more recent data, so that reporting standards, etc. are a little more consistent.
  filter(year >= 2005) %>%
  # Select the variables I actually want to use.
  select(fips, naics, year, month_emplvl, month, day_of_month, poll_cluster,
         PM25_conc, angle, raw_prcp, raw_tmax, raw_tmin, wind_speed, num_injured) %>%
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
  ungroup() %>%
  # Select case and referent observations. Group by fips, naics, year, month, weekday, then flag
  # by any(accident_occurred). This follows Jones, et al. (2005).
  group_by(fips, naics, year, month, weekday) %>%
  mutate(case_or_control = any(accident_occurred)) %>%
  ungroup()

# Designate the PM 2.5 data as panel data for calculating moving averages.
mydata %<>%
  mutate(L_PM25 = PM25) %>%
  mutate(panel_id = paste(mydata$fips, mydata$naics)) %>%
  pdata.frame(index = c('panel_id', 'date'))

# Lag PM2.5 for calculating moving averages.
mydata$L_PM25 <- lag(mydata$PM25, k = 1)

# Calculate moving averages, with square and cube for fitting quadratic and cubic functional forms
mydata %<>%
  mutate(MA_PM25 = rowMeans(select(., PM25, L_PM25), na.rm = FALSE)) %>%
  mutate(MA_PM25_sq = MA_PM25 ^ 2) %>%
  mutate(MA_PM25_cu = MA_PM25 ^ 3)

# Drop observations with NA values, since I think they might be screwing things up.
mydata %<>%
  na.omit()

# Do OLS regressions with some different model specifications, hopefully capturing any nonlinearity
linear_regression <- mydata %>%
  lm(accident_occurred ~ MA_PM25 + month_emplvl + raw_prcp + raw_tmax + raw_tmin + wind_speed, .)
quadratic_regression <- mydata %>%
  lm(accident_occurred ~ MA_PM25 + MA_PM25_sq + month_emplvl + raw_prcp + raw_tmax + raw_tmin + wind_speed, .)
cubic_regression <- mydata %>%
  lm(accident_occurred ~ MA_PM25 + MA_PM25_sq + MA_PM25_cu + month_emplvl + raw_prcp + raw_tmax + raw_tmin + wind_speed, .)

summary(linear_regression)
summary(quadratic_regression)
summary(cubic_regression)

cat('Now we get the cluster-robust standard errors, and calculate new t_tests, etc.')

linear_vcov <- cluster.vcov(linear_regression, mydata$fips)
quadratic_vcov <- cluster.vcov(quadratic_regression, mydata$fips)
cubic_vcov <- cluster.vcov(cubic_regression, mydata$fips)

coeftest(linear_regression, linear_vcov)
coeftest(quadratic_regression, quadratic_vcov)
coeftest(cubic_regression, cubic_vcov)

cat('Now for the instrumental variables models')

linear_iv_regression <- ivreg(accident_occurred ~ MA_PM25 + month_emplvl + raw_prcp + raw_tmax + raw_tmin + wind_speed | . -MA_PM25 + angle:poll_cluster, data = mydata)
quadratic_iv_regression <- ivreg(accident_occurred ~ MA_PM25 + MA_PM25_sq + month_emplvl + raw_prcp + raw_tmax + raw_tmin + wind_speed | . -MA_PM25 -MA_PM25_sq + angle:poll_cluster, data = mydata)
cubic_iv_regression <- ivreg(accident_occurred ~ MA_PM25 + MA_PM25_sq + MA_PM25_cu + month_emplvl + raw_prcp + raw_tmax + raw_tmin + wind_speed | . -MA_PM25 -MA_PM25_sq -MA_PM25_cu + angle:poll_cluster, data = mydata)

summary(linear_iv_regression)
summary(quadratic_regression)
summary(cubic_regression)

cat('And again, we see what happens when we cluster standard errors.')

cluster_id_vector <- mydata %>% pull(fips)

linear_iv_vcov  <- cluster.robust.se(linear_iv_regression, cluster_id_vector)
quadratic_iv_vcov <- cluster.robust.se(quadratic_iv_regression, cluster_id_vector)
cubic_iv_vcov <- cluster.robust.se(cubic_iv_regression, cluster_id_vector)

coeftest(linear_iv_regression, linear_iv_vcov)
coeftest(quadratic_iv_regression, quadratic_iv_vcov)
coeftest(cubic_iv_regression, cubic_iv_vcov)