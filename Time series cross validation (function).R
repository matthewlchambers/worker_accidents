# Time series data require special attention in cross-validating. Standard k-fold cross-validation will overstate
# the strength of a model as a result of using future points to predict past points. Instead, validation or testing
# sets must always follow their training sets temporally. Moreover, in order to secure almost unbiased estimates
# of model error, a process of nested cross-validation must be used (see Varma and Simon 2006) to avoid potential
# bias arrising from training
#
# This function is designed to work with the caret package,
# by creating the appropriate lists of indices for submitting to trainControl() as the index and indexOut keyword
# arguments.

library(caret)
library(tidyverse)
library(lubridate)
library(magrittr)

y <- make_date(2011, 12, 15):make_date(2011, 12, 30)

time_slice <- createTimeSlices(y, 5, 1, fixedWindow = TRUE)

extract_from_y <- function (index, data) {
  return(data[[index]])
}

is_in_date_slice <- function (slice, date) {
  return(date %in% slice)
}

custom_mapping <- function (date, set_of_slices) {
  return(map_depth(set_of_slices, 1, function(slice, date) return(date %in% slice), date = date))
}

date_slice <- map_depth(time_slice, 3, extract_from_y, data = y)

#y_train <- map(y, custom_mapping, set_of_slices = date_slice$train)

mydata <- y %>% as_tibble() %>%
  mutate(y_train = map(y, custom_mapping, set_of_slices = date_slice$train))

mydata %<>%
  unnest_wider(y_train)

# I think I may want to use the which() function to identify the row indices of observations matching the
# training (or test) dates