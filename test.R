# Title     : TODO
# Objective : TODO
# Created by: Matthew Chambers
# Created on: 7/4/2020

library(ivpack)

data(card.data)
ivmodel <- ivreg(lwage ~ educ + exper + expersq + black + south + smsa + smsa66,
~ nearc4 + exper + expersq + black + south + smsa + smsa66, data=card.data)
# Compute cluster robust standard errors when the clustering is by region
cluster.robust.se(ivmodel, card.data$region)