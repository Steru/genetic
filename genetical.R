library(GA)

setwd("D:/")

ga_Selection <- function(object, ...)
{
    # Proportional (roulette wheel) selection with ratio solution to the best solution
    prob <- abs(object@fitness)/max(abs(object@fitness))
    sel <- sample(1:object@popSize, size = object@popSize, 
                  prob = pmin(pmax(0, prob), 1, na.rm = TRUE),
                  replace = TRUE)
    out <- list(population = object@population[sel,,drop=FALSE],
                fitness = object@fitness[sel])
    return(out)
}

ga_Mutation <- function(object, parent, ...)
{
    # Swap element[m] with first
    mutate <- parent <- as.vector(object@population[parent,])
    n <- length(parent)
    m <- sample(1:n, size = 1)
    value <- parent[m]
    mutate[m] <- parent[1]
    mutate[1] <- value
    return(mutate)
}


ga_Crossover <- function(object, parents, ...)
{
    # Mix every second element
    parents <- object@population[parents,,drop = FALSE]
    n <- ncol(parents)
    children <- matrix(as.double(NA), nrow = 2, ncol = n)
    children[1,] <- 	c(parents[1,seq(1, n, by = 2)],
                       parents[2,seq(2, n, by = 2)])
    children[2,] <- 	c(parents[2,seq(1, n, by = 2)],
                       parents[1,seq(2, n, by = 2)])

    out <- list(children = children, fitness = rep(NA, 2))
    return(out)
}


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
ITERATION_COUNT <- 30
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
popS <- 25
cros <- 0.8
mut <- 0.2

#for(popS in c(10, 20, 30, 100, 500)) { # value of population size
     myTitle.popS <- toString(popS)
     #for(cros in seq(0,1,0.2)) { # value of crossover probability
         myTitle.cros <- toString(cros)
         #for(mut in seq(0,1,0.2)) { # value of mutation probability
            myTitle.mut <- toString(mut)
         	fitnessValue1Variable <- fitnessValue3Variables <- fitnessValue5Variables <- 0
         	bufMean <- bufBest <- bufMedian <- 1
         	
         	# !! update accordingly
         	myTitle.func <- "f5_no_switches"
         	#myTitle.func <- "f5_crossover"
         	#myTitle.func <- "f5_mutation"
         	#myTitle.func <- "f5_selection"
         	#myTitle.func <- "f5_all_switches"
         	for (iter in 1:ITERATION_COUNT) {
         	    
         	    ## # # # # # # # # # # # # # # # #
         	    ## <changeable piece>
         	    
         	    GA <- ga(type = "real-valued", 
         	             fitness =  function(x) f(x[1], x[2], x[3], x[4], x[5], 7),
         	             min = c(-10, -10, -10, -10, -10), max = c(10, 10, 10, 10, 10), 
         	             popSize = popS, maxiter = 1000, run = 100,
                         # !!! comment unwanted method switches        	            
         	             #selection = ga_Selection,
         	             #crossover = ga_Crossover,
         	             #mutation = ga_Mutation,
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
         	} # end of "for (iter in 1:ITERATION_COUNT)"
         	
         	
         	bufMean <- bufMean / ITERATION_COUNT
         	bufBest <- bufBest / ITERATION_COUNT
         	bufMedian <- bufMedian / ITERATION_COUNT
         	
         	y0 <- 1:length(bufMean)
         	
         	# plotting 
         	plot.new
         	plot(GA)
         	filename <- paste(myTitle.func, ".jpeg")
         	dev.copy(jpeg, file=filename)
         	dev.off()
         	
         	plot.new
         	plot(y0, bufMean, col="red", xlab="Iteration", ylab="Value", type="l", ylim=range(c(bufMean, bufBest, bufMedian)))
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
         	
         	
         	fitnessValue1Variable <- fitnessValue1Variable / ITERATION_COUNT
         	fitnessValue3Variables <- fitnessValue3Variables / ITERATION_COUNT
         	fitnessValue5Variables <- fitnessValue5Variables / ITERATION_COUNT
         	fitnessValues <- rbind(fitnessValues, c(popS, mut, cros, fitnessValue1Variable, fitnessValue3Variables, fitnessValue5Variables))
         	print(fitnessValues)
         #}
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