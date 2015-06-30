require(ggplot2)
require(zoo)
require(reshape2)
require(RColorBrewer)
require(tidyr)
require(dplyr)
require(plyr)
require(scales) #for percentage point scales
require(chron)
require(manipulate) # for checkbox/picker.

setwd("~/Academic/SGPE/Dissertation/Data/csv/")

data <- read.csv("long_term_interest_rates_ECB.csv")
data$date <- as.yearmon(data$date, "%Y%b")
data$date <- as.Date(data$date, "%b %Y")

x <- colnames(data)

df <- data %>% 
  gather(country, yield, Austria..Euro:Slovakia..Euro)

levels(df$country) <- c("AT", "BE", "CY", "DE", "ES", "FI", "FR", "UK", "GR", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK")
df$date <- as.chron(df$date)
df$date <- as.Date(df$date, format="%m/%d/%y")

#save manipulations
#write.csv(df, "ecb_lti_clean.csv", row.names = FALSE)

#Graphs
b1 <- ggplot(data=df,
             aes(date, yield/100, colour=country, linetype=country, size=country)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  scale_size_manual(values=c(rep.int(1, 19))) +
  scale_linetype_manual(values=c(rep.int(1, 19))) +
  ylab("European Long Term Bond Yields (ECB)") + 
  theme_bw()

#NOTES: Great convergence in 1990s, great divergence in 2010s.
#Find pre-ECB data to see extent of convergence.


#For greater simplicity of selection, impose filter for countries
b2 <- function(cy){
  subdf <- subset(df, country == cy)
  ggplot(subdf,
             aes(x=date, y=(yield/100), colour=country, linetype=country, size=country)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  #scale_size_manual(values=c(rep.int(1, 19))) +
  #scale_linetype_manual(values=c(rep.int(1, 19))) +
  ylab("European Long Term Bond Yields (ECB)") + 
  theme_bw()
}
x <- as.list(as.character(unique(df$country)))

graph <- manipulate (b2(cy), cy = picker(x, label = "Country"))
#Lehman Bros collapse: 15 Sep 2008. (wiki)
#Greece: 20 Oct 2009 - defecit revelation. Dec 2009 ratings agencies downgrade (A1 to A2)
#9 Feb 2010 first austerity package. 3 March second package. April - more downgrades
#2 May 2010 bailout. 5 May general strike.


b3 <- ggplot(data=subset(df, country %in% c("PT", "IT", "IE", "GR", "ES", "DE")),
             aes(date, yield/100, colour=country, linetype=country, size=country)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  scale_size_manual(values=c(rep.int(1, 19))) +
  scale_linetype_manual(values=c(rep.int(1, 19))) +
  ylab("European Long Term Bond Yields (ECB)") + 
  theme_bw()

#Adding Greek bailout date and Euro circulation date
b3 + 
  geom_vline(xintercept=as.numeric(as.Date("2009-10-20"))) + 
  geom_vline(xintercept=as.numeric(as.Date("2002-01-01")))  
