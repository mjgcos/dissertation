#### Crisis Plots####
setwd("~/Academic/SGPE/Dissertation/Data/csv")

data <- read.csv("final.csv")
data$date <- as.Date(data$date)
attach(data)

### Base plot with dates on the x-axis.
### mutate each dummy variable, multiply by number between 1 and 6. Then plot as points
### rename each number as country.

a.data <- data[,c(1, 6:9)]
b.1.data <- data[,c(1, 11:17)]
b.2.data <- data[,c(1, 18:24)]

a <- 1:4
a.data[,2] <- a.data[,2]*1 #GR
a.data[,3] <- a.data[,3]*2 #IE
a.data[,4] <- a.data[,4]*3 #PT
a.data[,5] <- a.data[,5]*4 #ES
a.data$no <- rep(0, nrow(a.data))

pdf(file = "../graphics/crisis_a.pdf")
with(a.data, plot(date, spain_a, col = "yellow",
                  main = "Date Scheme A"))
with(a.data, points(date, greece_a, col = "blue"))
with(a.data, points(date, ireland_a, col = "green"))
with(a.data, points(date, portugal_a, col = "purple"))
with(a.data, points(date, no, col = "white"))
axis(2, labels = FALSE)
axis(2, at = c(0,1,2,3,4) , labels = c("", "GR", "IE", "PT", "ES"))
