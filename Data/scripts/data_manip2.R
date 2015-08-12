library(dplyr)
library(tidyr)
library(rugarch)

setwd("~/Academic/SGPE/Dissertation/Data/csv")

data <- read.csv("dated.csv")
data$date <- as.Date(data$date)
## Objective is to manipulate dataframe into objects that can be taken as arguments in
## the GIVE formulation. 

#Common Factors

common <- filter(data, country == "all")
common <- select(common, c(date, euribor, vix, vstoxx))

#Individual Country
# As each country equation is estimated individually, can separate once
# crisis dummies etc in correct framework.
include = c("es", "fr", "gr", "ie", "it","pt", "nl")
ind <- data[, c(1:4, 6, 9:15)]
ind <- filter(ind, country %in% include )

crisis_1.5 <- select(ind, c(date, country, crisis_b_1.5))
crisis_1.5 <- spread(crisis_1.5, country, crisis_b_1.5)
nms <- names(crisis_1.5)
for(i in 2:8){
  name <- nms[i]
  names(crisis_1.5)[i] <- paste(name, "b", "1.5", sep="_")
}

crisis_2 <- select(ind, c(date, country, crisis_b_2))
crisis_2 <- spread(crisis_2, country, crisis_b_2)
nms <- names(crisis_2)
for(i in 2:8){
  name <- nms[i]
  names(crisis_2)[i] <- paste(name, "b", "2", sep="_")
}

## Now merging B schedule crisis dataframes with ind

crisis <- merge(crisis_1.5, crisis_2, by = "date")
ind2 <- ind[1:10]
ind2 <-  merge(ind2, crisis, by = "date")

## Combining the ind and common factors:

final <- merge(ind2, common, by="date")
final <- arrange(final, country, date)

#write.csv(final, "final.csv")