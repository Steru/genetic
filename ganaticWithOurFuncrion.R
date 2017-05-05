library(GA)

setwd("D:/")

#---------------------------------------------------------------
# FUNCTION FOR GA
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

ga_Mutation <- function(object, parent, ...)
{
# Uniform sqrt mutation
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  j <- sample(1:n, size = 1) {
	mutate[j] <- sqrt(mutate[j])
  }
  return(mutate)
}

#---------------------------------------------------------------
# TARGET FUNCTION
#----------------------------------------------------------------
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

#---------------------------------------------------------------
# GA PARAMETERS
#----------------------------------------------------------------
ITERATION <- 30
popS <- 25
cros <- 0.8
mut <- 0.2
medianValue <- 0

#---------------------------------------------------------------
# GA MAIN LOOP
#----------------------------------------------------------------
for (iter in 1:ITERATION) {        	    
	GA <- ga(type = "real-valued", 
			 fitness =  function(x) f(x[1], x[2], x[3], x[4], x[5], 7),
			 min = c(-10, -10, -10, -10, -10), max = c(10, 10, 10, 10, 10), 
			 popSize = popS, maxiter = 1000, run = 100,
			 pcrossover = cros, pmutation = mut,
			 selection = ga_Selection,
			 crossover = ga_Crossover)
	medianValue <- medianValue + attr(GA, "fitnessValue")
}

medianValue <- medianValue / ITERATION