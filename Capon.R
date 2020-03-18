##this file contains all the code regarding capon statistic

# functions to find expectation of ith order statistic from normal
Expectation <- function(r,n, mu=0, sigma=1) {
  integrand <- function(x,r,n,mu=0, sigma=1) {
    x*x * (1 / beta(r, n - r + 1)) * pnorm(x, mu, sigma)^(r - 1) * (1 - pnorm(x, mu, sigma))^(n - r) * dnorm(x, mu, sigma)
  }
  integrate(integrand,-Inf,Inf, r, n, mu, sigma)$value
}
#check if function is working properly
mean(sapply(1:10000, function(x) sort(rnorm(10))[3]^2))

#capon's statistic
capon_stat <- function(R, N){
  sum(sapply(R, function(i) Expectation(i, N)))
}

simulate_capon <- function(n, m, rdist=rNorm, theta=1) { #rdist should be functions from modified_draws
  #simulate single sample and return capon's statistic
  
  #Draw sample from given distn distribution
  x <- rdist(n, 1)
  y <- rdist(m, theta)
  combined <- c(x,y)
  #calculate value of capon statistic
  R = rank(combined, ties.method = "random")
  capon_stat(R[1:n], n + m)
}

#function to find variance of capons statistic under null
capon_stat_var <- function(n, m) {
  E2 <- sum(sapply(1:(n+m), function(i) Expectation(i, n + m)^2))
  (m * n /((m + n) *(m + n - 1))) * (E2 - (n + m)) #isha dewan book page 124
}

#Returns rejection probability (null: theta=1/ alt: theta > 1)
capon_rejection = function(n, m, 
                           rdist = rNorm, theta=1,
                           repl = 1000, p = 0.05) #total replication, and level
{
  s = replicate(repl, simulate_capon(n, m, rdist, theta))
  crit = qnorm(p, mean = n, sd = sqrt(capon_stat_var(n, m)), lower.tail = FALSE)
  sum(s > crit)/repl
}
