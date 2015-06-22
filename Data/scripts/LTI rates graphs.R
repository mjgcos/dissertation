require(ggplot2)
require(zoo)
require(reshape2)

setwd("~/Academic/SGPE/Dissertation/Data/csv/")

data <- read.csv("long_term_interest_rates_ECB.csv")
data$date <- as.yearmon(data$date, "%Y%b")
