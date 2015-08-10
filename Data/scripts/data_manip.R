#library(xlsx)

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
dat$vol_vix <- dat[,16]
dat$vol_vstoxx <- dat[,17]
dat$euribor <- dat[,18]

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

df <- cbind(dat$date, dat[,66:105])

#write.csv(df,"cleaned_datastram.csv", row.names = FALSE)

#######################
#######MELTING#########
#######################

#cols: date, country, variable, value. 