## Plots for Data Section
library(ggplot2)
require(scales) 

setwd("~/Academic/SGPE/Dissertation/Data/csv")
data <- read.csv("final.csv")
data$date <- as.Date(data$date)
attach(data)
countries <- as.character(levels(country))
country.colours <- as.character(c("orange", "red", "blue", "green", 
                                  "darkgreen", "orange", "purple"))

a <- ggplot(data, aes(date, bsp/100, colour = country)) +
  geom_line() +
  theme_bw() +
  scale_size_manual(values=c(rep.int(1, 19))) +
  scale_y_continuous(labels = percent_format()) +
#  scale_color_manual(values = country.colours) +
  scale_color_manual(values = rep("black", 7)) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(-0.025,0.025)) +
  ylab("Differenced Bond Spreads")

d_bsp_graph <- a + facet_wrap( ~ country , ncol =2)
  
pdf(file = "../graphics/d_bsp_graph.pdf")
  d_bsp_graph
dev.off()
