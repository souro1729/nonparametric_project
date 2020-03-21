## This file contains code for different simulations and storing them into matrices

#short function to convert integers into string
st <- function(x) as.character(x)

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
  #progress bar
  pb <- txtProgressBar(style = 3, min = 1, max = length(mvec) * length(nvec), initial = 1)
  k <- 1
  #rejection probability
  reject_prob = matrix(0,length(mvec),length(nvec))
  rownames(reject_prob) <- nvec; colnames(reject_prob) <- mvec; 
  for(i in nvec){
    for(j in mvec){
      reject_prob[st(i), st(j)] = stat_rejection(i, j, rdist, theta, repl, p)
      k <- k + 1 #pb
      setTxtProgressBar(pb, k) #pb
    }
  }
  close(pb) #pb
  write.csv(reject_prob, sprintf("data/%s -- theta=%.3f -- %s -- n:%s -- m:%s.csv", 
                                 as.character(substitute(stat_rejection)),
                                 theta, as.character(substitute(rdist)), 
                                 paste(nvec, collapse = ", "), paste(mvec, collapse = ", ")))
  reject_prob
}

normality_matrix <- function(nvec, mvec, repl=1000,
                             simulate_stat= simulate_capon, #change this input to get results of different statistics
                             p = 0.05)
{
  #progress bar
  pb <- txtProgressBar(style = 3, min = 1, max = length(mvec) * length(nvec), initial = 1)
  k <- 1
  #shapiro wilk test matrix
  testmat = matrix(0, length(nvec), length(mvec))
  rownames(testmat) <- nvec; colnames(testmat) <- mvec; 
  for(i in nvec){
    for(j in mvec){
      data = replicate(repl, simulate_stat(i, j, theta = 1))
      testmat[st(i), st(j)] = shapiro.test(data)$p.value
      k <- k + 1 #pb
      setTxtProgressBar(pb, k) #pb
    }
  }
  close(pb) #pb
  write.csv(testmat, sprintf("data/%s -- %s.csv", 
                             as.character(substitute(simulate_stat)),
                             paste(nvec, collapse = ", ")))
  testmat
}
