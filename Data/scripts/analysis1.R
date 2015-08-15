library(dplyr)
library(AER) #for IV estimator
library(systemfit)

setwd("~/Academic/SGPE/Dissertation/Data/csv")

data <- read.csv("final.csv")
data$date <- as.Date(data$date)
attach(data)

##########################
###### OLS & GIVE ########
##########################

countries <- as.character(levels(country))
m = 6



#Begin for loop
for(i in 1:7){
    cy <- countries[i]
    
    ## Perform analysis for one country, get result formatting down then loop over cy.
    
    df <- filter(data, country == cy)
    # Bond spreads
    s <- data.frame(df$date, df$bsp)
    names(s)[2] <- "bsp"
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
    d_names <- names(D)[-7]
    
    #Instruments, polynomial of order m
    W <- data.frame(df$date, df$stocks)
    for(j in 1:m){
      W[,1+j] <- (W[,2])^j
      names(W)[1+j] <- paste("inst_",j, sep = "")
    }
    
    
    ready <- merge(s, x, by="df.date")
    ready <- merge(ready, common, by="df.date")
    ready <- merge(ready, D, by="df.date")
    ready <- merge(ready, W, by="df.date")
    #rename variables to put in model equation
    names(ready)[c(6:11)] <- c("d_1", "d_2", "d_3", "d_4", "d_5", "d_6")
    #Create a name key:
    namekey <- NULL
    for(j in 1:6){
      key <- paste(as.character(names(ready)[5+j]), as.character(d_names[j]), sep = "=")
      namekey <- paste(namekey, key, sep = "||")
    }
    
    olsreg <- lm(bsp ~ stocks + euribor + vol + 
                   d_1 + d_2 + d_3 + d_4 + d_5 + d_6, data = ready)
    # print ols output to text file
    # First, create variables to pass to cat
    fname <- paste("../../Document/out/summary_ols_", cy, ".txt", sep = "")
    out <- capture.output(summary(olsreg))
    header <- paste("OLS Output:", cy)
    # print to text file. append = FALSE means data will be overwritten.
    cat(header, namekey, out, file= fname, 
        sep="\n", append=FALSE)


}

#ivreg(bsp ~ stocks + euribor + vol + 
#        d_1 + d_2 + d_3 + d_4 + d_5 + d_6 | stocks + euribor + vol +
#        , data = ready)

#print ivreg output to text file


