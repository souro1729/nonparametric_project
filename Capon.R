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

T <- sapply(1:10000, function(i) simulate_capon(20, 30, rGamma, theta = 1))
#plot histogram
hist(T, breaks = 100)
mean(T)
