#this file contains all the codes necessary for simulation
C <- sapply(1:10000, function(i) simulate_capon(20, 30, rGamma, theta = 1))
#plot histogram
hist(C, breaks = 100)
mean(C)


p = 0.05 #done for a specific value
m = seq(10,20,5)
n = seq(10,20,5)


find_rejection_matrix <- function(nvec, mvec, 
                                  stat_rejection = capon_rejection, 
                                  rdist = rNorm, parameter = 1, 
                                  repl = 1000, p = 0.05)
{
  size = matrix(0,length(mvec),length(nvec))
  sapply(nvec, function(i){
    sapply(mvec, function(j){
      size[i,j] = stat_rejection(i, j, rdist, parameter, repl, p)
    })
  })
}
