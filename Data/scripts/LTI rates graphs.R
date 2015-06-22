require(ggplot2)
require(zoo)
require(reshape2)
require(RColorBrewer)
require(tidyr)
require(dplyr)
require(plyr)
require(scales) #for percentage point scales
require(chron)

setwd("~/Academic/SGPE/Dissertation/Data/csv/")

data <- read.csv("long_term_interest_rates_ECB.csv")
data$date <- as.yearmon(data$date, "%Y%b")
data$date <- as.Date(data$date, "%b %Y")

x <- colnames(data)

df <- data %>% 
  gather(country, yield, Austria..Euro:Slovakia..Euro)

levels(df$country) <- c("AT", "BE", "CY", "DE", "ES", "FI", "FR", "UK", "GR", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK")
df$date <- as.chron(df$date)

#Graphs
b1 <- ggplot(data=df,
             aes(date, yield, colour=country, linetype=country, size=country)) +
  geom_line() +
  scale_y_continuous() +
  scale_size_manual(values=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)) +
  scale_linetype_manual(values=c(rep.int(1, 19))) +
  ylab("European Long Term Bond Yields (ECB)") + 
  theme_bw()
