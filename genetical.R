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

fitnessValues <- matrix(1:6, ncol=6)
ITERATION_COUNT <- 5
# #################################################
#      Main loops of the program. 
#
# First, there were three iterators:
# > population size 
# > crossover probability
# > mutation probability
#
# That's why these for loops are still here. 
# Should be uncommented and commented when needed.
# #################################################

#for(popS in c(10, 20, 30, 100, 500)) { # value of population size
    popS <- 30
     myTitle.popS <- toString(popS)
     #for(cros in seq(0,1,0.2)) { # value of crossover probability
     cros <- 0.5
         myTitle.cros <- toString(cros)
         for(mut in seq(0,1,0.2)) { # value of mutation probability
            #mut <- 0.5
            myTitle.mut <- toString(mut)
         	fitnessValue1Variable <- fitnessValue3Variables <- fitnessValue5Variables <- 0
         	bufMean <- bufBest <- bufMedian <- 1
         	
         	# !! update accordingly
         	myTitle.func <- "f5"
         	for (iter in 1:5) {
         	    
         	    ## # # # # # # # # # # # # # # # #
         	    ## <changeable piece>
         	    
         	    GA <- ga(type = "real-valued", 
         	             fitness =  function(x) f(x[1], x[2], x[3], x[4], x[5], 7),
         	             min = c(-10, -10, -10, -10, -10), max = c(10, 10, 10, 10, 10), 
         	             popSize = popS, maxiter = 1000, run = 100,
         	             pcrossover = cros, pmutation = mut)
         	    fitnessValue5Variables <- fitnessValue5Variables + attr(GA, "fitnessValue")
         	    
         	    ## </changeable piece> 
         	    ## # # # # # # # # # # # # # # # #
         	    
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
         	} # end of "for (iter in 1:5)"
         	
         	
         	bufMean <- bufMean / 5
         	bufBest <- bufBest / 5
         	bufMedian <- bufMedian / 5
         	
         	y0 <- 1:length(bufMean)
         	
         	# plotting 
         	plot.new
         	plot(GA)
         	filename <- paste(myTitle.func, ".jpeg")
         	dev.copy(jpeg, file=filename)
         	dev.off()
         	
         	plot.new
         	plot(y0, bufMean, col="red", xlab="Iteration", ylab="Value", type="l")
         	lines(y0, bufBest, col="green")
         	lines(y0, bufMedian, col="blue")
         	legend(x = "bottomright", c("mean", "best", "median"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("red", "green","blue"))
         	my.title <- paste("Wielkoœæ populacji = ", 
         	                  myTitle.popS, "\nPrawdop. mutacji = ", 
         	                  myTitle.mut, "\nPrawdop. krosowania = ", 
         	                  myTitle.cros)
         	title(my.title)
         	
         	# save the file
         	myFilename <- paste(myTitle.func,myTitle.cros, myTitle.mut, myTitle.popS, ".jpeg", sep="_")
         	dev.copy(jpeg, file=myFilename)
         	dev.off()
         	##
         	
         	
         	fitnessValue1Variable <- fitnessValue1Variable / 5
         	fitnessValue3Variables <- fitnessValue3Variables / 5
         	fitnessValue5Variables <- fitnessValue5Variables / 5
         	fitnessValues <- rbind(fitnessValues, c(popS, mut, cros, fitnessValue1Variable, fitnessValue3Variables, fitnessValue5Variables))
         	print(fitnessValues)
         }
	#}
#}
         
         
# ## used when needed         
# ################################################################
# # -------- JEDNEJ ZMIENNEJ
# myTitle.func <- "f1"
# 
#  GA <- ga(type = "real-valued", 
#           fitness =  function(x) f(x[1], x[2], x[3], x[4], x[5], 25),
#           min = c(-10, 0, 0, 0, 0), max = c(10, 0, 0, 0, 0), 
#           popSize = popS, maxiter = 1000, run = 100,
#           pcrossover = cros, pmutation = mut)
#  fitnessValue1Variable <- fitnessValue1Variable + attr(GA, "fitnessValue")
# 
# ################################################################
# # --------- TRZECH ZMIENNYCH
# myTitle.func <- "f3"
# 
# GA <- ga(type = "real-valued", 
#          fitness =  function(x) f(x[1], x[2], x[3], x[4], x[5], 15),
#          min = c(-10, -10, -10, 0, 0), max = c(10, 10, 10, 0, 0), 
#          popSize = popS, maxiter = 1000, run = 100,
#          pcrossover = cros, pmutation = mut)
# fitnessValue3Variables <- fitnessValue3Variables + attr(GA, "fitnessValue")
# 
# ################################################################
# # --------- PIECIU ZMIENNYCH
# myTitle.func <- "f5"
# 
# GA <- ga(type = "real-valued", 
#          fitness =  function(x) f(x[1], x[2], x[3], x[4], x[5], 7),
#          min = c(-10, -10, -10, -10, -10), max = c(10, 10, 10, 10, 10), 
#          popSize = popS, maxiter = 1000, run = 100,
#          pcrossover = cros, pmutation = mut)
# fitnessValue5Variables <- fitnessValue5Variables + attr(GA, "fitnessValue")