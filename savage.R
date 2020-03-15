#savage statistic

#savage statistic
savage_stat <- function(R, N){
  n = length(R)
  sum(
    sapply(1:n, function(i){
      sum(
        sapply(1:R[i], function(j) 1/(N-j+1))
        )  
      }
    )
  )
}

simulate_savage <- function(n, m, delta=0){
  ## simulates a single sample and returns savage statistic
  
  #Draw sample from exponential distribution
  x <- rexp(n, 1)
  y <- rexp(m, 1/exp(-delta))
  combined <- c(x,y)
  #calculate value of savage statistic
  R = rank(combined, ties.method = "random")
  savage_stat(R[1:n], n + m)
}

#Store
T <- sapply(1:10000, function(i) simulate_savage(20, 30, delta = 0))
#plot histogram
hist(T, breaks = 100)
