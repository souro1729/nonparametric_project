power_curve <- function(n, m, from=0.2, to=1, len=30, 
                rdist=rNorm, stat_rejection = capon_rejection,
                                                            repl=500)
{
  #progress bar
  pb <- txtProgressBar(style = 3, min = from, max = to, initial = from)
  #theta sequence
  theta <- seq(from, to, length.out = len)
  power <- sapply(theta, function(i){ 
    setTxtProgressBar(pb, i) #pb
    stat_rejection(n, m, rdist = rdist, theta = i, repl = repl)
  })
  write.csv(power, sprintf("New/power -- %s (%.0f, %.0f) -- %s -- %.2f, %.2f, %.2f -- repl=%0.f.csv",
                           as.character(substitute(stat_rejection)), n, m,
                           as.character(substitute(rdist)), from, to, len, repl))
  close(pb) #pb
  xyplot(power ~ theta, xlim = c(to + 0.2, from - 0.2))
}


  plot_power <- function(n, m, stat, distns){
    require(lattice)
    data <- matrix(0, nrow=1000, ncol=length(distns))
    colnames(data) <- distns
    for (distn in distns){
      data[, distn] <- as.vector(read.csv(sprintf("New/power -- %s_rejection (%d, %d) -- r%s -- 0.20, 1.00, 1000.00 -- repl=1000.csv",
                                                  stat, n, m, distn))[, -1])
    }
    frml = paste(distns, collapse = " + ")
    frml = formula(sprintf("%s ~ theta", frml))
    print(frml)
    theta = seq(from = 0.2, to = 1, length.out = 1000)
    data = data.frame(data, theta)
    xyplot(frml, data=data, 
           auto.key=TRUE, xlim=c(1.05, 0.15),
           cex=0.6, type=c("p"), pch=19)
  }







