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

for(valPoptim in c(0.0, 0.05, 0.1)) {
	for(valPressel in c(0.4, 0.5, 0.6)) {
		sred <- 0
		for(i in 1:iteration) {
			GA <- ga(type = "real-valued", 
				 fitness =  function(x) f(x[1], x[2], x[3], x[4], x[5], 7),
				 min = c(-10, -10, -10, -10, -10), max = c(10, 10, 10, 10, 10), 
				 popSize = popS, maxiter = 1000, run = 100,
				 pcrossover = cros, pmutation = mut, optim = TRUE,
				 optimArgs = list(method = "L-BFGS-B",
								poptim = 0.05,
								pressel = 0.5,
								control = list(fnscale = -1, maxit = 100)))
									 
				sred <- sred + attr(GA, "fitnessValue")
		}
	}
}