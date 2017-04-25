library(TSP)
library(GA)
library(igraph)

setwd("D:/")

#---------------------------------------------------------------
#
# FUNCTION TO DRAWING
#
#----------------------------------------------------------------
plot.tour <- function(x, y, tour) {
    n <- nrow(tour)
    for (ii in seq(2, n)) {
        for (jj in seq(1, ii)) {
            w <- tour[ii, jj]
            if (w > 0) 
                lines(x[c(ii, jj)], y[c(ii, jj)], lwd = w, col = "lightgray")
        }
    }
}
#---------------------------------------------------------------
#
# FUNCTION TO COUNTING
#
#----------------------------------------------------------------
generateDistMatrix <- function(cordinates) {
	len <- length(cordinates)/2
	dists <- matrix(nrow = len, ncol=len)
	for(i in 1:len) {
		for(j in 1:len) {
			if(i == j) {
				dists[i,j] <- 0
			} else {
				dists[i,j] <- sqrt((cordinates[i,1] - cordinates[j,1])^2 + (cordinates[i,2] - cordinates[j,2])^2)
			}
		}
	}
	return(dists)
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
r <- read_TSPLIB("PWr/Lab35/att48.tsp")

#---------------------------------------------------------------
#
# START COUNTING
#
#----------------------------------------------------------------
D <- generateDistMatrix(as.matrix(r))

len <- length(r)/2
result <- 0
iteration <- 30

for(i in 1:iteration) {
# run a GA algorithm
GA.fit <- ga(type = "permutation", fitness = tpsFitness, distMatrix = D, min = 1, 
    max = len, popSize = 10, maxiter = 500, run = 100, pmutation = 0.2, 
    monitor = NULL)

	result <- result + 1/attr(GA.fit, "fitnessValue")
}

result <- result/iteration

#---------------------------------------------------------------
#
# DRAWING
#
#----------------------------------------------------------------

mat <- as.matrix(r)
x <- mat[,1]
y <- mat[,2]
n <- length(x)
tour <- GA.fit@solution[1, ]
tour <- c(tour, tour[1])

plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", main = "Tour after GA converged")
points(x, y, pch = 16, cex = 1.5, col = "grey")
abline(h = pretty(range(x), 10), v = pretty(range(y), 10), col = "lightgrey")
n <- length(tour)
arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]], length = 0.15, angle = 45, 
    col = "steelblue", lwd = 2)
text(x, y - 100, attr(GA.fit, "solution"), cex = 0.8)

# source: http://rpubs.com/somasdhavala/GAeg