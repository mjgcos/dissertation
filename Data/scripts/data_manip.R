#library(xlsx)
library(dplyr)
library(tidyr)
library(rugarch)

setwd("~/Academic/SGPE/Dissertation/Data/csv")
#setwd("~/dissertation/Data/csv/")


dat <- read.csv("datastream_combined.csv", colClasses = "character")

#basic manipulation
for(i in 2:65){
    dat[,i] <- as.numeric(dat[,i])
}
dat[,1] <- as.Date(dat[,1], format = "%d/%m/%Y")
dat <- subset(dat, dat$date > as.Date("2007-01-01"))

#Generate bond spreads to DE
dat$bsp_at <- dat[,5] - dat[,2]
dat$bsp_be <- dat[,6] - dat[,2]
dat$bsp_es <- dat[,15] - dat[,2]
dat$bsp_fn <- dat[,11] - dat[,2]
dat$bsp_fr <- dat[,3] - dat[,2]
dat$bsp_gr <- dat[,7] - dat[,2]
dat$bsp_ie <- dat[,8] - dat[,2]
dat$bsp_it <- dat[,4] - dat[,2]
dat$bsp_lt <- dat[,12] - dat[,2]
dat$bsp_ma <- dat[,13] - dat[,2]
dat$bsp_nl <- dat[,9] - dat[,2]
dat$bsp_pt <- dat[,10] - dat[,2]
dat$bsp_sk <- dat[,14] - dat[,2]


#Global Indicators
dat$vix_all <- dat[,16]
dat$vstoxx_all <- dat[,17]
dat$euribor_all <- dat[,18]

#Stock price indices
dat$stocks_at <- dat[,27]
dat$stocks_de <- dat[,20]
dat$stocks_es <- dat[,26]
dat$stocks_fr <- dat[,21]
dat$stocks_gr <- dat[,25]
dat$stocks_ie <- dat[,19]
dat$stocks_it <- dat[,24]
dat$stocks_nl <- dat[,22]
dat$stocks_pt <- dat[,23]

#CDS
dat$cds_at <- dat[,43]
dat$cds_be <- dat[,44]
dat$cds_cy <- dat[,46]
dat$cds_de <- dat[,39]
dat$cds_es <- dat[,40]
dat$cds_fr <- dat[,45]
dat$cds_gr <- dat[,42]
dat$cds_ie <- dat[,41]
dat$cds_it <- dat[,38]
dat$cds_nl <- dat[,47]

#non-EZ
dat$cds_dk <- dat[,62]
dat$cds_ic <- dat[,64] #iceland
dat$cds_sv <- dat[,59] #sweden
dat$cds_uk <- dat[,60]
dat$cds_us <- dat[,61]

df <- cbind(dat[,1], dat[,66:105])
names(df)[1] <- "date"

#write.csv(df,"cleaned_datastram.csv", row.names = FALSE)

#######################
#######Tidying#########
#######################

#cols: date, country, variable, value. 

#using dplyr and tidyr notation, condense variables into entries by date (NB, NAs removed)
#%>% is dplyr pipe function
tidier <- df %>% gather(key, value, -date)
#Split key column into country and variable:
tidy <- tidier %>% separate(key, into = c("variable", "country"), sep = "\\_")
#list variables and countries as factor rather than character vector
tidy$variable <- as.factor(tidy$variable)
tidy$country <- as.factor(tidy$country)
#reorder rows by country then date
tidy <- tidy %>% arrange(date, country)

clean <- tidy %>% spread(variable, value)
clean <- clean %>% arrange(country, date)

######################
##Dummy Variables#####
######################

#Date schedule A

greek_1 <- seq(as.Date("2009-10-20"), as.Date("2012-03-09"), by="days")
greek_2 <- seq(as.Date("2015-01-25"), as.Date("2015-07-02"), by="days")
greek <- c(greek_1, greek_2)
ireland <- seq(as.Date("2010-10-18"), as.Date("2011-03-09"), by="days")
portugal <- seq(as.Date("2011-03-23"), as.Date("2011-06-05"), by="days")
spain <- seq(as.Date("2012-05-25"), as.Date("2012-06-09"), by="days")

clean <- mutate(clean, greece_a = ifelse(country == "gr" & date %in% greek, 1, 0))
clean <- mutate(clean, ireland_a = ifelse(country == "ie" & date %in% ireland, 1, 0))
clean <- mutate(clean, portugal_a = ifelse(country == "pt" & date %in% portugal, 1, 0))
clean <- mutate(clean, spain_a = ifelse(country == "es" & date %in% spain, 1, 0))
clean <- mutate(clean, crisis_a = ifelse(greece_a + ireland_a + portugal_a +spain_a == 0, 0, 1))


#Date Schedule B

#set strength of tollerance

c = 2
###WARNING: Loop not optimised, inefficient computation of 46,557 obs.
for(i in 1:nrow(clean)){
  # Indicator variable A = s_{it} - c*s_i
  # First calculate sd of each country
  cy <- clean[i,2]
  mat <- subset(clean, country == cy)
  sigma <- sd(mat$bsp, na.rm = TRUE)
  #Scale by tollerance
  csigma <- c*sigma
  #generate variabe A
  A <- clean[i,3] - csigma
  #Apply indicator function
  clean[i,14] <- ifelse(A > 0, 1, 0)
  
  
  print(i)
}
names(clean)[14] <- "crisis_b_2"

c= 1.5

for(i in 1:nrow(clean)){
  # Indicator variable A = s_{it} - c*s_i
  # First calculate sd of each country
  cy <- clean[i,2]
  mat <- subset(clean, country == cy)
  sigma <- sd(mat$bsp, na.rm = TRUE)
  #Scale by tollerance
  csigma <- c*sigma
  #generate variabe A
  A <- clean[i,3] - csigma
  #Apply indicator function
  clean[i,15] <- ifelse(A > 0, 1, 0)
  
  
  print(i)
}
names(clean)[15] <- "crisis_b_1.5"

## Due to length of computation time, csv file saved and a new script started
## see data_manip2.R

write.csv(clean, file = "dated.csv", row.names = FALSE)
