# Title     : TODO
# Objective : TODO
# Created by: Matthew Chambers
# Created on: 6/9/2020

library(tidyverse)
library(magrittr)
library(grf)
library(sufrep)

#mydata <- read_csv("E:/Research Projects/Worker Accidents and Pollution/Data/Deryugina-QCEW_naics2-OSHA.csv",
#                   col_types = "iiiiififdddddffdiddddi")
mydata <- read_csv("E:/Research Projects/Worker Accidents and Pollution/Data/State files/Deryugina-QCEW_naics2-OSHA_PA.csv",
                   col_types = "iiiiififdddddffdiddddi") %>%
  # Draw a sample for testing, since training the forest on millions of examples is tedious. Obivously take this out later.
  sample_n(10000) %>%
  # Select the variables I actually want to use.
  select(fips, naics, year, month_emplvl, month, day_of_month, poll_cluster,
         PM25_conc, angle, raw_prcp, raw_tmax, raw_tmin, wind_speed, num_injured) %>%
  # Replace NAs in num_injured with 0s, since they correspond to no accident occurring
  mutate(num_injured = replace_na(num_injured, 0)) %>%
  # Drop observation with missing values in important variables
  drop_na(c('fips', 'naics', 'year', 'PM25_conc', 'poll_cluster', 'month', 'day_of_month')) %>%
  # Drop observations with no reported employees in a given industry, since aren't relevant.
  filter(month_emplvl > 0) %>%
  # Generate the probability of an accident occurring by dividing number of people injured by industry-county employment level
  mutate(accident_prob = num_injured / month_emplvl) %>%
  # Group by county-day
  group_by(fips, year, month, day_of_month) %>%
  # Get total employment by county across all industries
  mutate(tot_emplvl = sum(month_emplvl)) %>%
  # Ungroup so nothing weird happens with other commands
  ungroup() %>%
  # Generate percent employment (within each county) by industry
  mutate(pct_emplvl = month_emplvl / tot_emplvl) %>%
  # Generate a binary for whether an accident occurred
  mutate(accident_occurred = num_injured > 0)

# I think I need to one-hot encode poll_cluster, then multiply each column by angle to get a set of instruments
# equal to the number of pollution groups. The following code should accomplish that goal directly, by
# one-hot encoding poll_cluster with values from angle rather than with ones.

#mydata %<>%
#  spread(poll_cluster, angle, fill = 0, sep = '_')

# Then I encode fips by using the 'sufrep' package means option, using the means of employment
# level and distribution and weather variables

mydata %<>%
  drop_na(c('tot_emplvl', 'pct_emplvl', 'raw_prcp', 'raw_tmax', 'raw_tmin', 'fips', 'naics'))

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

X <- mydata %>%
  select(year, month, day_of_month, tot_emplvl, pct_emplvl, raw_prcp, raw_tmax, raw_tmin, wind_speed) %>%
  bind_cols(fips_encoded_data) %>%
  bind_cols(naics_encoded_data) %>%
  as.matrix()

Y <- mydata %>%
  pull(accident_occurred)

W <- mydata %>%
  pull(PM25_conc)

Z <- mydata %>%
  pull(angle)

#tau.forest <- instrumental_forest(X, Y, W, Z, num.trees = 4000)
#
#average_partial_effect(tau.forest)
