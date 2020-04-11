#modified functions to draw samples
rExp <- function(n, theta) {
  rexp(n, rate = 1/theta)
}

rGamma <- function(n, theta, shape=0.5) {
  rgamma(n, shape = shape, scale = theta)
}

rWeibull <- function(n, theta, shape=0.5){
  rweibull(n, shape = shape, scale = theta)
}

rNorm <- function(n, theta=1) {
  rnorm(n, mean = 0, sd = theta)
}

rLogis<-function(n,theta){
  rlogis(n,scale = theta)
}
