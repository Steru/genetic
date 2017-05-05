library(TSP)
library(GA)
library(igraph)

setwd("D:/")

#---------------------------------------------------------------
#
# FUNCTION FOR GA
#
#----------------------------------------------------------------
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

ga_Crossover <- function(object, parents, ...)
{
# Random value to cross (change will be value in 1..RANDOM)
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  cxPoints <- sample(1:n, size = 1)
  cxPoints <- seq(1, cxPoints)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  children[,cxPoints] <- parents[,cxPoints]
  for(i in setdiff(1:n, cxPoints))
     { if(!any(parents[2,i] == children[1,cxPoints]))
         { children[1,i] <- parents[2,i] }
       if(!any(parents[1,i] == children[2,cxPoints]))
         { children[2,i] <- parents[1,i] }
     }
  children[1,is.na(children[1,])] <- setdiff(parents[2,], children[1,])
  children[2,is.na(children[2,])] <- setdiff(parents[1,], children[2,])
  out <- list(children = children, fitness = rep(NA,2))
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


# given a tour, calculate the total distance
tourLength <- function(tour, distMatrix) {
    tour <- c(tour, tour[1])
    route <- embed(tour, 2)[, 2:1]
    sum(distMatrix[route])
}
# inverse of thetotal distance is the fitness
tpsFitness <- function(tour, ...) 1/tourLength(tour, ...)

#---------------------------------------------------------------
# load data
#---------------------------------------------------------------
tspFile <- "att48.tsp"
r <- read_TSPLIB("PWr/Lab35/" + tspFile)

#---------------------------------------------------------------
#
# START COUNTING
#
#----------------------------------------------------------------
D <- generateDistMatrix(as.matrix(r))

len <- length(r)/2
result <- 0
iteration <- 15
bufMean <- bufBest <- bufMedian <- 1

for (iter in 1:iteration) {
         	    
	## # # # # # # # # # # # # # # # #
	## <changeable piece>
	
	GA <- ga(type = "permutation", fitness = tpsFitness, distMatrix = D, min = 1, 
		max = len, popSize = 10, maxiter = 500, run = 100, pmutation = 0.2,
		selection = ga_Selection,
		crossover = ga_Crossover,
		mutation = ga_Mutation,
		monitor = NULL)
	
	result <- result + 1/attr(GA, "fitnessValue")
	print(result)
	
	## </changeable piece> 
	## # # # # # # # # # # # # # # # #
	
	currentMeanVal <- 1/attr(GA, "summary")[,"mean"]
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
	
	currentBestVal <- 1/attr(GA, "summary")[,"max"]
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
	
	currentMedianVal <- 1/attr(GA, "summary")[,"median"]
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
} 

#---------------------------------------------------------------
#
# DRAWING
#
#----------------------------------------------------------------
bufMean <- bufMean / iteration
bufBest <- bufBest / iteration
bufMedian <- bufMedian / iteration

y0 <- 1:length(bufMean)

# plotting 
plot.new
plot(GA)
#filename <- paste(tspFile, ".jpeg")
#dev.copy(jpeg, file=filename)
#dev.off()

plot.new
plot(y0, bufMean, col="red", xlab="Iteration", ylab="Value", type="l")
lines(y0, bufBest, col="green")
lines(y0, bufMedian, col="blue")
legend(x = "topright", c("mean", "best", "median"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("red", "green","blue"))
my.title <- paste(tspFile)
title(my.title)

# save the file
myFilename <- paste(tspFile, "Better.jpeg", sep="_")
dev.copy(jpeg, file=myFilename)
dev.off()
##

         	

#---------------------------------------------------------------
#
# FUNCTION TO COUNTING
#
#----------------------------------------------------------------
#generateDistMatrix <- function(cordinates) {
#	len <- length(cordinates)/2
#	dists <- matrix(nrow = len, ncol=len)
#	for(i in 1:len) {
#		for(j in 1:len) {
#			if(i == j) {
#				dists[i,j] <- 0
#			} else {
#				dists[i,j] <- sqrt((cordinates[i,1] - cordinates[j,1])^2 + (cordinates[i,2] - cordinates[j,2])^2)
#			}
#		}
#	}
#	return(dists)
#}
#---------------------------------------------------------------
#
# DRAWING
#
#----------------------------------------------------------------

#mat <- as.matrix(r)
#x <- mat[,1]
#y <- mat[,2]
#n <- length(x)
#tour <- GA.fit@solution[1, ]
#tour <- c(tour, tour[1])

#plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", main = "Tour after GA converged")
#points(x, y, pch = 16, cex = 1.5, col = "grey")
#abline(h = pretty(range(x), 10), v = pretty(range(y), 10), col = "lightgrey")
#n <- length(tour)
#arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]], length = 0.15, angle = 45, 
#    col = "steelblue", lwd = 2)
#text(x, y - 100, attr(GA.fit, "solution"), cex = 0.8)

# source: http://rpubs.com/somasdhavala/GAeg