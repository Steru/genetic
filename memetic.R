library(GA)

setwd("D:/")

f <- function(x1,x2,x3,x4,x5, functionNumber) {
     mat = matrix(nrow=length(x1), ncol=5)
     for(i in 1:length(x1)) {
         mat[i, 1] = x1[i]
         mat[i, 2] = x2[i]
         mat[i, 3] = x3[i]
         mat[i, 4] = x4[i]
         mat[i, 5] = x5[i]
     }
     cec2013::cec2013(functionNumber,mat)
}

popS <- 25
cros <- 0.8
mut <- 0.2

iteration <- 15

myTitle.func <- "memetic"
for(valPoptim in c(0.0, 0.05, 0.1)) {
    myTitle.valPoptim <- toString(valPoptim)
	for(valPressel in c(0.4, 0.5, 0.6)) {
	    myTitle.valPressel <- toString(valPressel)
		sred <- 0
		bufMean <- bufBest <- bufMedian <- 1
		for(i in 1:iteration) {
			GA <- ga(type = "real-valued", 
				 fitness =  function(x) f(x[1], x[2], x[3], x[4], x[5], 7),
				 min = c(-10, -10, -10, -10, -10), max = c(10, 10, 10, 10, 10), 
				 popSize = popS, maxiter = 1000, run = 100,
				 pcrossover = cros, pmutation = mut, optim = TRUE,
				 optimArgs = list(method = "L-BFGS-B",
								poptim = valPoptim,
								pressel = valPressel,
								control = list(fnscale = -1, maxit = 100)))
									 

				
				currentMeanVal <- attr(GA, "summary")[,"mean"]
				currentMeanLength <- length(currentMeanVal)
				bufMeanLength <- length(bufMean)
				# when the next iteration result is shorter, get the longer buffer
				if(bufMeanLength == 1){
				    bufMean <- currentMeanVal
				} else {
				    if(currentMeanLength < bufMeanLength){
				        length(currentMeanVal) <- bufMeanLength
				        for(iter in currentMeanLength : bufMeanLength){
				            currentMeanVal[iter] <- currentMeanVal[currentMeanLength]
				        }
				    } else {
				        length(bufMean) <- currentMeanLength
				        for(iter in bufMeanLength : currentMeanLength){
				            bufMean[iter] <- bufMean[bufMeanLength]
				        }
				    }
				    bufMean <- bufMean + currentMeanVal
				}
				
				currentBestVal <- attr(GA, "summary")[,"max"]
				currentBestLength <- length(currentBestVal)
				bufBestLength <- length(bufBest)
				if(bufBestLength == 1){
				    bufBest <- currentBestVal
				} else {
				    if(currentBestLength < bufBestLength){
				        length(currentBestVal) <- bufBestLength
				        for(iter in currentBestLength : bufBestLength){
				            currentBestVal[iter] <- currentBestVal[currentBestLength]
				        }
				    } else {
				        length(bufBest) <- currentBestLength
				        for(iter in bufBestLength : currentBestLength){
				            bufBest[iter] <- bufBest[bufBestLength]
				        }
				    }
				    bufBest <- bufBest + currentBestVal
				}
				
				currentMedianVal <- attr(GA, "summary")[,"median"]
				currentMedianLength <- length(currentMedianVal)
				bufMedianLength <- length(bufMedian)
				if(bufMedianLength == 1){
				    bufMedian <- currentMedianVal
				} else {
				    if(currentMedianLength < bufMedianLength){
				        length(currentMedianVal) <- bufMedianLength
				        for(iter in currentMedianLength : bufMedianLength){
				            currentMedianVal[iter] <- currentMedianVal[currentMedianLength]
				        }
				    } else {
				        length(bufMedian) <- currentMedianLength
				        for(iter in bufMedianLength : currentMedianLength){
				            bufMedian[iter] <- bufMedian[bufMedianLength]
				        }
				    }
				    bufMedian <- bufMedian + currentMedianVal
				}
		} # iteration
		
		# plotting 
		plot.new
		plot(GA)
		filename <- paste(myTitle.func, "LAST_RUN", "__poptim_", myTitle.valPoptim, "__pressel_", myTitle.valPressel, ".jpeg")
		dev.copy(jpeg, file=filename)
		dev.off()
		
		plot.new
		plot(y0, bufMean, col="red", 
		     xlab="Iteration", ylab="Value", 
		     type="l", 
		     ylim=range(c(bufMean, bufBest, bufMedian)))
		lines(y0, bufBest, col="green")
		lines(y0, bufMedian, col="blue")
		legend(x = "bottomright", c("mean", "best", "median"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("red", "green","blue"))
		myPlotTitle <- paste("Memetic \npoptim= ", myTitle.valPoptim, " | pressel = ", myTitle.valPressel)
		title(myPlotTitle)
		
		# save the file
		myFilename <- paste(myTitle.func, "__poptim_", myTitle.valPoptim, "__pressel_", myTitle.valPressel, ".jpeg")
		dev.copy(jpeg, file=myFilename)
		dev.off()
		##
		
	}
}
