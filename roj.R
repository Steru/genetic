library(psoptim)

setwd("D:/")

iterations <- 10

algValues <- c()
sred <- 0
for(valC1 in c(0.0, 0.2, 0.4)) {
#valC1 = 0.2
	for(valC2 in c(0.0, 0.2, 0.4)) {
#valC2 = 0.2
		for(valW in c(0.6, 0.8, 1.0)) {
#valW = 0.8
            myTitle.valC1 <- paste(toString(valC1))
            myTitle.valC2 <- paste(toString(valC2))
            myTitle.valw <- paste(toString(valW))

            sred <- 0
			OPT <- psoptim(function(x) f(x[1], x[2], x[3], x[4], x[5], 7), 
			               n=100, 
			               max.loop=100, 
			               w=valW, 
			               c1=valC1, 
			               c2=valC2,
					       xmin=c(-10, -10, -10, -10, -10), 
					       xmax=c(10, 10, 10, 10, 10), 
					       vmax=c(10, 10, 10, 10, 10), 
					       seed=NULL, 
					       anim=TRUE)
			
			myPlotTitle <- paste("F-cja psoptim\nC1 = ", myTitle.valC1, " | C2 = ", myTitle.valC2, " | w = ", myTitle.valw)
			title(myPlotTitle)
			
			myFilename <- paste("psoptim__c1_", myTitle.valC1, "__c2_", myTitle.valC2,"__w_", myTitle.valw, ".jpeg")
			dev.copy(jpeg, file=myFilename)
			dev.off()
		}
	}
}