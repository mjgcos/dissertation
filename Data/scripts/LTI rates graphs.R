require(ggplot2)
require(zoo)
require(reshape2)
require(RColorBrewer)
require(tidyr)
require(dplyr)
require(plyr)

setwd("~/Academic/SGPE/Dissertation/Data/csv/")

data <- read.csv("long_term_interest_rates_ECB.csv")
data$date <- as.yearmon(data$date, "%Y%b")

x <- colnames(data)

df <- data %>% 
  gather(country, yield, Austria..Euro:Slovakia..Euro)

levels(df$country) <- c("AT", "BE", "CY", "DE", "ES", "FI", "FR", "UK", "GR", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK")
