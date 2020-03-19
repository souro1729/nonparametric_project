#savage statistic

#savage statistic
savage_stat <- function(R, N){
  sum(
    sapply(R, function(i){
      sum(
        sapply(1:i, function(j) 1/(N-j+1))
        )  
      }
    )
  )
}

simulate_savage <- function(n, m, rdist=rExp, theta=1) { #rdist should be functions from modified_draws
  #simulate single sample and return savage statistic
  
  #Draw sample from given distn distribution
  x <- rdist(n, 1)
  y <- rdist(m, theta)
  combined <- c(x,y)
  #calculate value of capon statistic
  R = rank(combined, ties.method = "random")
  savage_stat(R[1:n], n + m)
}

#function to find variance of savage statistic under null
savage_stat_var =  function(n,m)
{
  s = sum(sapply(1:(n+m), function(i) 1/i))
  var = n * m * (1 - (s / (n + m)))/(n + m -1)
  var
}

#Returns rejection probability (null: theta=1/ alt: theta > 1)
savage_rejection = function(n, m, 
                           rdist = rExp, theta=1,
                           repl = 1000, p = 0.05) #total replication, and level
{
  s = replicate(repl, simulate_savage(n, m, rdist, theta))
  crit = qnorm(p, mean = n, sd = sqrt(savage_stat_var(n, m)), lower.tail = FALSE)
  sum(s > crit)/repl
}


