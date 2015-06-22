require(reshape2)

setwd("~/Academic/SGPE/Dissertation/Data/csv/")

data <- read.csv("long_term_interest_rates_ECB.csv")
data[,1] <- paste(data[,1])

data$year <- paste(substr(data[,1], 1, 4))
data$month <- paste(substr(data[,1], 5, 7))
data$month <- paste(match(tolower(data$month), tolower(month.abb)))


#Neither of these lines work. Both produce NA.
data$dates <- as.Date(paste(data$year,"-",data$month,"-01"), format = "%Y-%m-%d")
data$dates2 <- as.Date(paste(data$date), format = "%Y%b")
