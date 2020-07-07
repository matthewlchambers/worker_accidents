# Title     : Prepare inversion data from Bing Yang Tan
# Objective : see above
# Created by: Matthew Chambers
# Created on: 7/3/2020

library(magrittr)
library(lubridate)
library(tidyverse)
library(haven)

# Set the directory and filename.
input_dir <- 'C:/Users/Matthew Chambers/Desktop/Inversion Data'
input_file_prefix <- 'inversions_'
output_dir <- 'C:/Users/Matthew Chambers/Desktop/Inversion Data/Combined CSVs'
output_file_prefix <- 'inversions_'

# Set the year range
begin_year <- 2000
end_year <- 2016

# Save the number of seconds from 1-1-1800 to 1-1-1970 as a constant
conversion_constant <- (ymd_hms('1970-01-01 00:00:00') - ymd_hms('1800-01-01 00:00:00')) %>%
  as.double.difftime(units = 'secs')

#
all_data <- NULL

for (i in begin_year:end_year) {
  year_data <- NULL

  for (j in 1:2) {
    month_data <- read_dta(file.path(input_dir, paste0(input_file_prefix, i, str_pad(j, 2, pad = '0'), '.dta'))) %>%
      mutate(ti = as_datetime(ti * 3600 - conversion_constant)) %>%
      mutate(day_of_month = day(ti)) %>%
      mutate(fips = statefip * 1000 + countyfip) %>%
      mutate(date = make_date(year, month, day_of_month)) %>%
      select(!c('statefip', 'countyfip', 'ti', 'year', 'month', 'day_of_month')) %>%
      mutate(inversion_coverage = inversions_fraction / total_weight) %>%
      group_by(fips, date) %>%
      summarise_all(mean) %>%
      ungroup()

    if (is.null(year_data)) {
      year_data <- month_data
    } else {
      year_data %<>%
        bind_rows(month_data)
    }
  }

  if (is.null(all_data)) {
    all_data <- year_data
  } else {
    all_data %<>%
      bind_rows(year_data)
  }

  write.csv(year_data, file.path(output_dir, paste0(output_file_prefix, i, '.csv')), row.names = FALSE)
}

write.csv(all_data, file.path(output_dir, paste0(output_file_prefix, 'all_years.csv')), row.names = FALSE)
