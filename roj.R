library(psoptim)

setwd("D:/")

iterations <- 10

algValues <- c()

for(valC1 in c(0.0, 0.2, 0.4)) {
	for(valC2 in c(0.0, 0.2, 0.4)) {
		for(valW in c(0.6, 0.8, 1.0)) {
			sred <- 0
			for(i in 1:iterations) {
				OPT <- psoptim(function(x) f(x[1], x[2], x[3], x[4], x[5], 7), n=100, max.loop=100, w=valW, c1=valC1, c2=valC2,
						xmin=c(-10, -10, -10, -10, -10), xmax=c(10, 10, 10, 10, 10), vmax=c(10, 10, 10, 10, 10), seed=NULL, anim=TRUE)
				sred <- sred + OPT$val
			}
			sred <- sred / iterations
			algValues <- c(algValues, sred)
		}
	}
}