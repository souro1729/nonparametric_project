theta=seq(1,1.3,len=1000)


parametric_rejection = function(m, n, 
                                rdist = rnorm, theta=1,
                                repl = 1000, p = 0.05) #total replication, and level
{
  new=function(){
    x = rnorm(m,0,1)
    y = rnorm(n, 1,theta)
    var.test(x, y, alternative = "less")$p.value
  }
  s=replicate(repl,new())
  length(which(s<p))/repl
}

c=sapply(theta,parametric_rejection,m=1000,n=1000,repl=10000,p=0.001,rdist=rnorm)
library(lattice)
xyplot(c~theta,type="l")
