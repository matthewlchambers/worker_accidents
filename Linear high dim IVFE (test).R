# Title     : First steps in R
# Objective : Figure out how to do lfe regression using R
# Created by: Matthew Chambers
# Created on: 5/19/2020

library(tidyverse)
library(magrittr)
library(lfe)

mydata <- read_csv("E:/Research Projects/Worker Accidents and Pollution/Data/State files/Deryugina-QCEW_naics2-OSHA_CA.csv",
                   col_types = "fffiffffdddddffdiddddi")
#mydata <- read_csv("E:/Research Projects/Worker Accidents and Pollution/Data/Deryugina-QCEW_naics2-OSHA.csv",
#                   col_types = "fffiffffdddddffdiddddi")

head(mydata)

#mydata %>%
#  select(num_injured) %>%
#  colSums(na.rm = TRUE, dims = 1)

mydata %<>%
  filter(mydata$naics == 23)

mydata %<>%
  dplyr::mutate(num_injured = replace_na(num_injured, 0))

mydata %<>%
  drop_na(c("num_injured", "weather", "fips", "naics", "year", "PM25_conc", "ang_range", "poll_cluster"))

mydata %<>%
  add_column(accident_prob = ifelse(mydata$month_emplvl == 0, 0, mydata$num_injured / mydata$month_emplvl))

#mydata %>%
#  select(accident_prob) %>%
#  colSums(na.rm = TRUE, dims = 1)

#est <- felm(mydata$accident_prob ~ 1 | mydata$weather + mydata$fips + mydata$naics + mydata$month + mydata$state_fips | (mydata$PM25_conc ~ mydata$ang_range:mydata$poll_cluster) | mydata$fips, exactDOF = TRUE)
est <- felm(mydata$accident_prob ~ 1 | mydata$weather + mydata$fips + mydata$month | (mydata$PM25_conc ~ mydata$ang_range:mydata$poll_cluster) | mydata$fips, exactDOF = TRUE)

summary(est)
