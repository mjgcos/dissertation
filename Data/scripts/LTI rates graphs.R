require(ggplot2)
require(zoo)
require(reshape2)
require(RColorBrewer)
require(tidyr)
require(dplyr)

setwd("~/Academic/SGPE/Dissertation/Data/csv/")

data <- read.csv("long_term_interest_rates_ECB.csv")
data$date <- as.yearmon(data$date, "%Y%b")

x <- colnames(data)

df <- data %>% 
  gather(country, yield, Austria..Euro:Slovakia..Euro)
