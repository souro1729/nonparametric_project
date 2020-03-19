## This file contains code for different simulations and storing them into matrices

#short function to convert integers into string
c <- function(x) as.character(x)

#returns rejection probability using F statistic
parametric_rejection = function(n, m, 
                                rdist = rNorm, theta=1,
                                repl = 1000, p = 0.05) #total replication, and level
{
  s = replicate(repl, function(){
    x = rdist(n, 1)
    y = rdist(m, theta)
    var.test(x, y, alternative = "greater")$p.value
  } )
  sum(s < 0.05) / repl
}


m = seq(10,20,5)
n = seq(10,20,5)

#returns a matrix of rejection probabilities where row index denotes m and column
#index denotes n
find_rejection_matrix <- function(nvec, mvec, 
                                  stat_rejection = capon_rejection, #change this input to get results of different statistics
                                  rdist = rNorm, theta = 1, 
                                  repl = 1000, p = 0.05)
{
  reject_prob = matrix(0,length(mvec),length(nvec))
  rownames(reject_prob) <- nvec; colnames(reject_prob) <- mvec; 
  sapply(nvec, function(i){
    sapply(mvec, function(j){
      reject_prob[c(i), c(j)] = stat_rejection(i, j, rdist, theta, parameter, repl, p)
    })
  })
  reject_prob
}

normality_matrix <- function(nvec, mvec, 
                             simulate_stat= simulate_capon, #change this input to get results of different statistics
                             p = 0.05)
{
  testmat = matrix(0,3,3)
  rownames(testmat) <- nvec; colnames(testmat) <- mvec; 
  sapply(nvec, function(i){
    sapply(mvec, function(j){
      data = replicate(repl, simulate_stat(i, j, theta = 1))
      testmat[c(i), c(j)] = shapiro.test(data)$p.value
    })
  })
  testmat
}
