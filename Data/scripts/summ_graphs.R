###################
###SUMMARY GRAPH###
###################
dat <- subset(dat, dat$date > as.Date("2007-01-01"))


pdf(file = "../graphics/bsp.pdf", width = 10, height = 7)
layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), heights = c(3,3,1))
par(oma = c(0,2,0,0))
(with(dat, {
  par(mar = c(2,2,1,1))
  plot(date, bsp_gr, type="n")
  lines(date, bsp_fr, col="blue")
  lines(date, bsp_gr, col="red")
  lines(date, bsp_ie, col="green")
  lines(date, bsp_es, col="yellow")
  lines(date, bsp_it, col="black")
  lines(date, bsp_pt, col="darkred")
  lines(date, bsp_nl, col="orange")
  #legend("topleft", c("ES", "FR", "GR", "IE", "IT", "NL", "PT"), lty = 1, 
  #col = c("yellow", "blue","red", "green", "black", "orange", "darkred"))
  par(mar=c(2,2,1,1))
  plot(date, bsp_pt, type="n")
  lines(date, bsp_fr, col="blue")
  #with(dat, lines(date, bsp_gr, col="red"))
  lines(date, bsp_ie, col="green")
  lines(date, bsp_es, col="yellow")
  lines(date, bsp_it, col="black")
  lines(date, bsp_pt, col="darkred")
  lines(date, bsp_nl, col="orange")
  
  par(mar = c(0,0,0,0), pin = c(7, 0.35))
  plot.new()
  legend("center", c("ES", "FR", "GR", "IE", "IT", "NL", "PT"), lty = 1, 
         col = c("yellow", "blue","red", "green", "black", "orange", "darkred"),
         ncol = 7)}
)
)
dev.off()
