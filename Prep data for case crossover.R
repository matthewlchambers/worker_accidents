# Title     : Prep data for case crossover
# Objective : This script is to prepare my large dataset for case crossover analysis
# Created by: Matthew Chambers
# Created on: 6/18/2020

library(magrittr)
library(lubridate)
library(grf)
library(sufrep)
library(tidyverse)
library(xts)
library(plm)

# Set the directory and filename.
input_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data'
input_file <- 'Deryugina-QCEW_naics2-OSHA.csv'
output_dir <- input_dir
output_file <- '2005_forward_all_states_for_case_crossover.csv'

mydata <- read_csv(file.path(input_dir, input_file), col_types = "iiiiififdddddffdiddddi", progress = FALSE) %>%
  ## Draw a sample for testing, since doing anything on millions of examples is tedious. Obviously take this out later.
  #filter(year == 2010, month < 6) %>%
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
  ungroup()

# Designate the PM 2.5 data as panel data for calculating moving averages.
mydata %<>%
  mutate(panel_id = paste(mydata$fips, mydata$naics)) %>%
  pdata.frame(index = c('panel_id', 'date'))

# Lag PM2.5 for calculating moving averages. We can't use mutate because that would return an object
# that is not a pdata.frame
mydata$L1_PM25 <- lag(mydata$PM25, k = 1)
mydata$L2_PM25 <- lag(mydata$PM25, k = 2)
mydata$L3_PM25 <- lag(mydata$PM25, k = 3)
mydata$L4_PM25 <- lag(mydata$PM25, k = 4)
mydata$L5_PM25 <- lag(mydata$PM25, k = 5)
mydata$L6_PM25 <- lag(mydata$PM25, k = 6)

# Calculate moving averages, with square and cube for fitting quadratic and cubic functional forms
mydata %<>%
  mutate(MA2_PM25 = rowMeans(select(., PM25, L1_PM25), na.rm = FALSE)) %>%
  mutate(MA2_PM25_sq = MA2_PM25 ^ 2) %>%
  mutate(MA2_PM25_cu = MA2_PM25 ^ 3) %>%
  mutate(MA3_PM25 = rowMeans(select(., PM25, L1_PM25, L2_PM25), na.rm = FALSE)) %>%
  mutate(MA3_PM25_sq = MA3_PM25 ^ 2) %>%
  mutate(MA3_PM25_cu = MA3_PM25 ^ 3) %>%
  mutate(MA4_PM25 = rowMeans(select(., PM25, L1_PM25, L2_PM25, L3_PM25), na.rm = FALSE)) %>%
  mutate(MA4_PM25_sq = MA4_PM25 ^ 2) %>%
  mutate(MA4_PM25_cu = MA4_PM25 ^ 3) %>%
  mutate(MA5_PM25 = rowMeans(select(., PM25, L1_PM25, L2_PM25, L3_PM25, L4_PM25), na.rm = FALSE)) %>%
  mutate(MA5_PM25_sq = MA5_PM25 ^ 2) %>%
  mutate(MA5_PM25_cu = MA5_PM25 ^ 3) %>%
  mutate(MA6_PM25 = rowMeans(select(., PM25, L1_PM25, L2_PM25, L3_PM25, L4_PM25, L5_PM25), na.rm = FALSE)) %>%
  mutate(MA6_PM25_sq = MA6_PM25 ^ 2) %>%
  mutate(MA6_PM25_cu = MA6_PM25 ^ 3) %>%
  mutate(MA7_PM25 = rowMeans(select(., PM25, L1_PM25, L2_PM25, L3_PM25, L4_PM25, L5_PM25, L6_PM25), na.rm = FALSE)) %>%
  mutate(MA7_PM25_sq = MA7_PM25 ^ 2) %>%
  mutate(MA7_PM25_cu = MA7_PM25 ^ 3)

# Calculate PM2.5 shock sizes, as the difference from the previous day's measure and as the difference
# from the 7-day moving average.
mydata %<>%
  mutate(shock_per_yesterday = PM25 - L1_PM25) %>%
  mutate(shock_per_week = PM25 - MA7_PM25)

  # Select case and referent observations. Group by fips, naics, year, month, weekday, then flag
  # by any(accident_occurred). This follows Jones, et al. (2005).
mydata %<>%
  group_by(fips, naics, year, month, weekday) %>%
  mutate(case_or_control = any(accident_occurred)) %>%
  ungroup()

# Get a data frame of only case and referent observations.
mydata_small <- mydata %>%
  filter(case_or_control) %>%
  mutate(event_id = paste(panel_id, year, month))

write.csv(mydata_small, file.path(output_dir, output_file), row.names = FALSE)
