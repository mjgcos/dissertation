library(dplyr)
library(tidyr)
library(urca)
#library(data.table)
setwd("~/Academic/SGPE/Dissertation/Data/csv")

data <- read.csv("dated.csv")
data$date <- as.Date(data$date)
## Objective is to manipulate dataframe into objects that can be taken 
##as arguments in the GIVE formulation. 

#Common Factors

common <- filter(data, country == "all")
common <- select(common, c(date, euribor, vix, vstoxx))
#Log difference VSTOXX and VIX
common <- mutate(common, vix = log(vix))
common <- mutate(common, vstoxx = log(vstoxx))

common$dvix <- c(NA, diff(common$vix, differences = 1))
common$dvstoxx <- c(NA, diff(common$vstoxx, differences = 1))

common <- common[,c(1,2,5,6)]

#Lagged spread between Euribor and german t bills
dat <- read.csv("datastream_combined.csv", colClasses = "character")
dat <- dat[,c(1,2)]
dat[,1] <- as.Date(dat[,1], format="%d/%m/%Y")
dat <- subset(dat, dat$date > as.Date("2007-01-01"))
common <- merge(common, dat, by = "date")
names(common)[5] <- "de"
common[,5] <- as.numeric(common[,5])
common <- mutate(common, euribor = euribor - de)
common <- mutate(common, euribor = lag(euribor))
common <- common[,1:4]

#Individual Country
# As each country equation is estimated individually, can separate once
# crisis dummies etc in correct framework.
include = c("es", "fr", "gr", "ie", "it","pt", "nl")
ind <- data[, c(1:4, 6, 9:16)]
ind <- filter(ind, country %in% include )

###### This section has been moved to data_manip.R to allow crisis dating process
###### to use differenced spreads. Original retained for coherency.

# ### Time series modelling of bsp ###
# countries <- as.character(levels(country))
# times <- filter(data, country == "fr")
# acf(times$bsp)
# ur.df(times$bsp)
# qnorm(c(0.01, 0.05, 0.1))
# #although can reject null for NL at 10%, others cannot be rejected
# #conclude that series are not stationary, difference.
# 
# tind <- ind[,c(1,2,3)]
# tind <- spread(tind, country,  bsp)
# n <- names(tind)
# 
# for(i in 2:8){
#   tind[,i] <- c(NA, diff(tind[,i], differences = 1))
#   }
# tind <- gather(tind,country,bsp, -date)
# 
# ind <- ind[,c(-2, -3)]
# ind <- data.frame(tind, ind)
# ind <- ind[,-4]


####### More TS testing####

for(a in 1:7){
  cy <- include[a]
  times <- subset(ind, country == cy)
  times <- times[2:nrow(times),]
  y <- times$bsp
  print(cy)
  ur.df(y)
}

######### Crisis dummies

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

write.csv(final, "final.csv", row.names = FALSE)
