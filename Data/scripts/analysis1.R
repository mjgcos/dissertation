library(dplyr)
library(tidyr)
library(AER) #for IV estimator
library(systemfit)
library(urca)
library(rugarch)

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

# Select first full day of observations
data <- subset(data, date > as.Date("2007-09-01"))

### TS

# times <- data[,1:3]
# times <- spread(times, country, bsp)
# specification <- ugarchspec(mean.model = list(armaOrder = c(5, 0),
#                                               include.mean = TRUE,
#                                               archm = FALSE,
#                                               archpow = 1,
#                                               arfima = FALSE,
#                                               external.regressors = NULL,
#                                               archex = FALSE))
# y <- times[2:nrow(times),4]
# garch.model <- ugarchfit(spec = specification, data = y)
# plot(residuals(garch.model))
# coef(garch.model)

##########################
###### OLS & GIVE ########
##########################
m = 6
i = 3

countries <- as.character(levels(country))

# Loop transformations and OLS/GIVE over countries, then send to Document/out/
a_coefs <- data.frame(nrow=15)
a_ses <- data.frame(nrow = 15)
a_coefs_i <- data.frame(nrow=15)
a_ses_i <- data.frame(nrow = 15)
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
  D <-  data.frame(df[,11:17], df$date)
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
  
  olsreg <- lm(bsp ~ bsp_1 + bsp_2 + bsp_3 + bsp_4 + bsp_5 +
                 stocks + euribor + vol + 
                 d_1 + d_2 + d_3 + d_4 + d_5 + d_6, data = ready)
  # print ols output to text file
  # First, create variables to pass to cat
  fname <- paste("../../Document/out/ols/summary_ols_", cy, "_1.5.txt", sep = "")
  out <- capture.output(summary(olsreg))
  header <- paste("OLS Output:", cy)
  # print to text file. append = FALSE means data will be overwritten.
  cat(header, d_namekey, i_namekey, out, file= fname, 
      sep="\n", append=FALSE)
  
  #copy to file
  d <- data.frame(summary(olsreg)$coefficients)
  c <- d$Estimate
  s <- d$Std..Error
  a_coefs <- cbind(a_coefs, c)
  a_ses <- cbind(a_ses, s)
  
  ##GIVE##
  #First, only use one instrument.
  iv.reg <- ivreg(bsp ~ bsp_1 + bsp_2 + bsp_3 + bsp_4 + bsp_5 +
                    stocks + euribor + vol + 
                    d_1 + d_2 + d_3 + d_4 + d_5 + d_6 | bsp_1 + 
                    bsp_2 + bsp_3 + bsp_4 + bsp_5 +
                    stocks + euribor + vol +
                    i_1 + i_2 + i_3 + i_4 + i_5 + i_6, data = ready)
  
  fname <- paste("../../Document/out/give/summary_give_", cy, "_1.5.txt", sep = "")
  ivout <- capture.output(summary(iv.reg, vcov = sandwich))
  header <- paste("GIVE Output:", cy)
  # print to text file. append = FALSE means data will be overwritten.
  cat(header, d_namekey, i_namekey, ivout, file= fname, 
      sep="\n", append=FALSE)
  
  d <- data.frame(summary(iv.reg)$coefficients)
  c <- d$Estimate
  s <- d$Std..Error
  a_coefs_i <- cbind(a_coefs_i, c)
  a_ses_i <- cbind(a_ses_i, s)
  
  #then generate polynomial and use 6 instruments. (include m in file name)
  #polynomial of order m
  #    W <- data.frame(df$date, df$stocks)
  #    for(j in 1:m){
  #      W[,1+j] <- (W[,2])^j
  #      names(W)[1+j] <- paste("inst_",j, sep = "")
  #    }
  
}
a_coefs <- a_coefs[,c(-1,-2)]
a_ses <- a_ses[,c(-1,-2)]
a_coefs_i <- a_coefs_i[,c(-1,-2)]
a_ses_i <- a_ses_i[,c(-1,-2)]


write.table(format(a_coefs, digits=2, scientific = F), 
            file = "../../Document/out/tables/cols_a.txt", sep = "&")
write.table(format(a_ses, digits=2, scientific = F), 
            file = "../../Document/out/tables/sols_a.txt", sep = ")&(")    

write.table(format(a_coefs_i, digits=2, scientific = F), 
            file = "../../Document/out/tables/civ_a.txt", sep = "&")
write.table(format(a_ses_i, digits=2, scientific = F), 
            file = "../../Document/out/tables/siv_a.txt", sep = ")&(")





a_coefs <- data.frame(nrow=15)
a_coefs_i <- data.frame(nrow=15)
a_ses <- data.frame(nrow = 15)
a_ses_i <- data.frame(nrow=15)

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
    
    olsreg <- lm(bsp ~ bsp_1 + bsp_2 + bsp_3 + bsp_4 + bsp_5 +
                   stocks + euribor + vol + 
                   d_1 + d_2 + d_3 + d_4 + d_5 + d_6, data = ready)
    # print ols output to text file
    # First, create variables to pass to cat
    fname <- paste("../../Document/out/ols/summary_ols_", cy, ".txt", sep = "")
    out <- capture.output(summary(olsreg))
    header <- paste("OLS Output:", cy)
    # print to text file. append = FALSE means data will be overwritten.
    cat(header, d_namekey, i_namekey, out, file= fname, 
        sep="\n", append=FALSE)
    
    d <- data.frame(summary(olsreg)$coefficients)
    c <- d$Estimate
    s <- d$Std..Error
    a_coefs <- cbind(a_coefs, c)
    a_ses <- cbind(a_ses, s)
    
    ##GIVE##
    #First, only use one instrument.
    iv.reg <- ivreg(bsp ~ bsp_1 + bsp_2 + bsp_3 + bsp_4 + bsp_5 +
                      stocks + euribor + vol + 
                  d_1 + d_2 + d_3 + d_4 + d_5 + d_6 | bsp_1 + 
                  bsp_2 + bsp_3 + bsp_4 + bsp_5 +
                  stocks + euribor + vol +
                  i_1 + i_2 + i_3 + i_4 + i_5 + i_6, data = ready)
                  
    fname <- paste("../../Document/out/give/summary_give_", cy, ".txt", sep = "")
    ivout <- capture.output(summary(iv.reg, vcov = sandwich))
    header <- paste("GIVE Output:", cy)
    # print to text file. append = FALSE means data will be overwritten.
    cat(header, d_namekey, i_namekey, ivout, file= fname, 
        sep="\n", append=FALSE)
    
    d <- data.frame(summary(iv.reg)$coefficients)
    c <- d$Estimate
    s <- d$Std..Error
    a_coefs_i <- cbind(a_coefs_i, c)
    a_ses_i <- cbind(a_ses_i, s)
    
    #then generate polynomial and use 6 instruments. (include m in file name)
    #polynomial of order m
#    W <- data.frame(df$date, df$stocks)
#    for(j in 1:m){
#      W[,1+j] <- (W[,2])^j
#      names(W)[1+j] <- paste("inst_",j, sep = "")
#    }

}
    
a_coefs <- a_coefs[,c(-1,-2)]
a_ses <- a_ses[,c(-1,-2)]
a_coefs_i <- a_coefs_i[,c(-1,-2)]
a_ses_i <- a_ses_i[,c(-1,-2)]



write.table(format(a_coefs, digits=2, scientific = F), 
            file = "../../Document/out/tables/cols_b.txt", sep = "&")
write.table(format(a_ses, digits=2, scientific = F), 
            file = "../../Document/out/tables/sols_b.txt", sep = ")&(")    

# 
# for(i in 1:7){
#   #filter by country i
#   cy <- countries[i]
#   df <- filter(data, country == cy)
#   # Bond spreads
#   s <- data.frame(df$date, df$bsp)
#   names(s)[2] <- "bsp"
#   # add lags of dependent var
#   s$bsp_1 <- c(NA, diff(s$bsp, lag = 1))
#   s$bsp_2 <- c(NA, diff(s$bsp_1, lag = 1))    
#   s$bsp_3 <- c(NA, diff(s$bsp_2, lag = 1))    
#   s$bsp_4 <- c(NA, diff(s$bsp_3, lag = 1))    
#   s$bsp_5 <- c(NA, diff(s$bsp_4, lag = 1))    
#   # common factors
#   common <- data.frame(df$date, df$euribor, df$dvstoxx)
#   names(common)[c(2,3)] <- c("euribor", "vol")
#   # Market specific factors
#   x <- data.frame(df$date, df$stocks)
#   names(x)[2] <- "stocks"
#   
#   #Crisis dummiess
#   # Currently using B_2 scheme, can add more in later.
#   D <-  data.frame(df[,6:9], df$date)
#   if(cy == "gr"){
#     D <- D[,2:6]
#   }
#   if(cy == "ie"){
#     D <- D[,c(1, 3:6)]
#   }
#   if(cy =="pt"){
#     D <- D[,c(1,2, 4:6)]
#   }
#   if(cy =="es"){
#     D <- D[,c(1, 3:6)]
#   }
#   d_names <- names(D)[-7] #generates name vector without date
#   
#   #Instruments, 
#   Z1 <- data.frame(df[,28:34], df$date)
#   Z1 <- Z1[,-i]
#   i_names <- names(Z1)[-7] #generates name vector without date
#   
#   #Merge to form regression frame for cy.
#   ready <- merge(s, x, by="df.date")
#   ready <- merge(ready, common, by="df.date")
#   ready <- merge(ready, D, by="df.date")
#   ready <- merge(ready, Z1, by="df.date")
#   #rename variables to put in model equation
#   if(cy %in% c("gr", "ie", "pt", "es")){
#     names(ready)[c(11:13)] <- c("d_1", "d_2", "d_3")
#     names(ready)[c(14:19)] <- c("i_1", "i_2", "i_3", "i_4", "i_5", "i_6")
#   } else{
#     names(ready)[c(11:14)] <- c("d_1", "d_2", "d_3", "d_4")
#     names(ready)[c(15:20)] <- c("i_1", "i_2", "i_3", "i_4", "i_5", "i_6")
#   }
#   #Create a name key:
#   d_namekey <- NULL
#   for(j in 1:6){
#     key <- paste(as.character(names(ready)[10+j]), as.character(d_names[j]),
#                  sep = "=")
#     d_namekey <- paste(d_namekey, key, sep = "||")
#   }
#   i_namekey <- NULL
#   for(j in 1:6){
#     key <- paste(as.character(names(ready)[16+j]), as.character(i_names[j]),
#                  sep = "=")
#     i_namekey <- paste(i_namekey, key, sep = "||")
#   }    
#   
#   #### OLS ###
#   if(cy %in% c("gr", "ie", "pt", "es")){
#   olsreg <- lm(bsp ~ bsp_1 + bsp_2 + bsp_3 + bsp_4 + bsp_5 +
#                  stocks + euribor + vol + 
#                  d_1 + d_2 + d_3, data = ready)
#   } else{
#     olsreg <- lm(bsp ~ bsp_1 + bsp_2 + bsp_3 + bsp_4 + bsp_5 +
#                    stocks + euribor + vol + 
#                    d_1 + d_2 + d_3 + d_4, data = ready)
#   }
#   # print ols output to text file
#   # First, create variables to pass to cat
#   fname <- paste("../../Document/out/ols/summary_ols_a", cy, ".txt", sep = "")
#   out <- capture.output(summary(olsreg))
#   header <- paste("OLS Output:", cy)
#   # print to text file. append = FALSE means data will be overwritten.
#   cat(header, d_namekey, i_namekey, out, file= fname, 
#       sep="\n", append=FALSE)
#   
#   ##GIVE##
#   #First, only use one instrument.
#   if(cy %in% c("gr", "ie", "pt", "es")){  
#   iv.reg <- ivreg(bsp ~ bsp_1 + bsp_2 + bsp_3 + bsp_4 + bsp_5 +
#                     stocks + euribor + vol + 
#                     d_1 + d_2 + d_3 + d_4 | bsp_1 + 
#                     bsp_2 + bsp_3 + bsp_4 + bsp_5 +
#                     stocks + euribor + vol +
#                     i_1 + i_2 + i_3 + i_4 + i_5 + i_6, data = ready)
#   } else {
#     iv.reg <- ivreg(bsp ~ bsp_1 + bsp_2 + bsp_3 + bsp_4 + bsp_5 +
#                       stocks + euribor + vol + 
#                       d_1 + d_2 + d_3 | bsp_1 + 
#                       bsp_2 + bsp_3 + bsp_4 + bsp_5 +
#                       stocks + euribor + vol +
#                       i_1 + i_2 + i_3 + i_4 + i_5 + i_6, data = ready)
#   }
#   fname <- paste("../../Document/out/give/summary_give_a", cy, ".txt", sep = "")
#   ivout <- capture.output(summary(iv.reg))
#   header <- paste("GIVE Output:", cy)
#   # print to text file. append = FALSE means data will be overwritten.
#   cat(header, d_namekey, i_namekey, ivout, file= fname, 
#       sep="\n", append=FALSE)
#   
#   #then generate polynomial and use 6 instruments. (include m in file name)
#   #polynomial of order m
#   #    W <- data.frame(df$date, df$stocks)
#   #    for(j in 1:m){
#   #      W[,1+j] <- (W[,2])^j
#   #      names(W)[1+j] <- paste("inst_",j, sep = "")
#   #    }
#   
# }
# 
