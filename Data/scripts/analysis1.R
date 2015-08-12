library(dplyr)

setwd("~/Academic/SGPE/Dissertation/Data/csv")

data <- read.csv("final.csv")
data$date <- as.Date(data$date)

cy = "ie"

## Perform analysis for one country, get result formatting down then loop over cy.

df <- filter(data, country == cy)
