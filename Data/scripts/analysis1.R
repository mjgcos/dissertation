library(dplyr)
library(tidyr)
library(AER) #for IV estimator
library(systemfit)
library(urca)

setwd("~/Academic/SGPE/Dissertation/Data/csv")

data <- read.csv("final.csv")
data$date <- as.Date(data$date)
attach(data)

### Instruments ###

insts <- data.frame(data[,c(1,2, 5)])
insts <- spread(insts, country, stocks)
nms <- names(insts)
for(i in 2:8){
  name <- nms[i]
  names(insts)[i] <- paste(name, "stock", sep = "_")
} 
data <- merge(data, insts, by="date")

##########################
###### OLS & GIVE ########
##########################
m = 6
#i = 5

countries <- as.character(levels(country))

# Loop transformations and OLS/GIVE over countries, then send to Document/out/

for(i in 1:7){
    #filter by country i
    cy <- countries[i]
    df <- filter(data, country == cy)
    # Bond spreads
    s <- data.frame(df$date, df$bsp)
    names(s)[2] <- "bsp"
    # add lags of dependent var
    s$bsp_1 <- c(NA, diff(s$bsp, lag = 1))
    s$bsp_2 <- c(NA, diff(s$bsp_1, lag = 1))    
    s$bsp_3 <- c(NA, diff(s$bsp_2, lag = 1))    
    s$bsp_4 <- c(NA, diff(s$bsp_3, lag = 1))    
    s$bsp_5 <- c(NA, diff(s$bsp_4, lag = 1))    
    # common factors
    common <- data.frame(df$date, df$euribor, df$dvstoxx)
    names(common)[c(2,3)] <- c("euribor", "vol")
    # Market specific factors
    x <- data.frame(df$date, df$stocks)
    names(x)[2] <- "stocks"
    
    #Crisis dummiess
    # Currently using B_2 scheme, can add more in later.
    D <-  data.frame(df[,18:24], df$date)
    D <- D[,-i]
    d_names <- names(D)[-7] #generates name vector without date
    
    #Instruments, 
    Z1 <- data.frame(df[,28:34], df$date)
    Z1 <- Z1[,-i]
    i_names <- names(Z1)[-7] #generates name vector without date
    
    #Merge to form regression frame for cy.
    ready <- merge(s, x, by="df.date")
    ready <- merge(ready, common, by="df.date")
    ready <- merge(ready, D, by="df.date")
    ready <- merge(ready, Z1, by="df.date")
    #rename variables to put in model equation
    names(ready)[c(11:16)] <- c("d_1", "d_2", "d_3", "d_4", "d_5", "d_6")
    names(ready)[c(17:22)] <- c("i_1", "i_2", "i_3", "i_4", "i_5", "i_6")
    #Create a name key:
    d_namekey <- NULL
    for(j in 1:6){
      key <- paste(as.character(names(ready)[10+j]), as.character(d_names[j]),
                   sep = "=")
      d_namekey <- paste(d_namekey, key, sep = "||")
    }
    i_namekey <- NULL
    for(j in 1:6){
      key <- paste(as.character(names(ready)[16+j]), as.character(i_names[j]),
                   sep = "=")
      i_namekey <- paste(i_namekey, key, sep = "||")
    }    
    
    #### OLS ###
    
    olsreg <- lm(bsp ~ stocks + euribor + vol + 
                   d_1 + d_2 + d_3 + d_4 + d_5 + d_6, data = ready)
    # print ols output to text file
    # First, create variables to pass to cat
    fname <- paste("../../Document/out/ols/summary_ols_", cy, ".txt", sep = "")
    out <- capture.output(summary(olsreg))
    header <- paste("OLS Output:", cy)
    # print to text file. append = FALSE means data will be overwritten.
    cat(header, d_namekey, i_namekey, out, file= fname, 
        sep="\n", append=FALSE)
    
    ##GIVE##
    #First, only use one instrument.
    iv.reg <- ivreg(bsp ~ stocks + euribor + vol + 
                d_1 + d_2 + d_3 + d_4 + d_5 + d_6 | stocks + euribor + vol +
                i_1 + i_2 + i_3 + i_4 + i_5 + i_6, data = ready)
    fname <- paste("../../Document/out/give/summary_give_", cy, ".txt", sep = "")
    ivout <- capture.output(summary(iv.reg))
    header <- paste("GIVE Output:", cy)
    # print to text file. append = FALSE means data will be overwritten.
    cat(header, d_namekey, i_namekey, ivout, file= fname, 
        sep="\n", append=FALSE)
    
    #then generate polynomial and use 6 instruments. (include m in file name)
    #polynomial of order m
#    W <- data.frame(df$date, df$stocks)
#    for(j in 1:m){
#      W[,1+j] <- (W[,2])^j
#      names(W)[1+j] <- paste("inst_",j, sep = "")
#    }

}


