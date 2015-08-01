#library(xlsx)

setwd("~/Academic/SGPE/Dissertation/Data/csv")
dat <- read.csv("datastream_combined.csv", colClasses = "character")

#basic manipulation
for(i in 2:65){
    dat[,i] <- as.numeric(dat[,i])
}
dat[,1] <- as.Date(dat[,1], format = "%d/%m/%Y")

#Generate bond spreads to DE
dat$bsp_fr <- dat[,3] - dat[,2]
dat$bsp_it <- dat[,4] - dat[,2]
dat$bsp_at <- dat[,5] - dat[,2]
dat$bsp_be <- dat[,6] - dat[,2]
dat$bsp_gr <- dat[,7] - dat[,2]
dat$bsp_ie <- dat[,8] - dat[,2]
dat$bsp_nl <- dat[,9] - dat[,2]
dat$bsp_pt <- dat[,10] - dat[,2]
dat$bsp_fn <- dat[,11] - dat[,2]
dat$bsp_lt <- dat[,12] - dat[,2]
dat$bsp_ma <- dat[,13] - dat[,2]
dat$bsp_sk <- dat[,14] - dat[,2]
dat$bsp_es <- dat[,15] - dat[,2]

###################
###SUMMARY STATS###
###################

