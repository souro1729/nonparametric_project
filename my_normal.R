theta=seq(1,2,len=1000)


parametric_rejection_norm = function(m, n, 
                                 theta=1,
                                repl = 1000, p = 0.05) #total replication, and level
{
  new=function(){
    x = rnorm(m,0,1)
    y = rnorm(n,0,theta)
    var.test(x, y, alternative = "less")$p.value
  }
  s=replicate(repl,new())
  length(which(s<p))/repl
}

c=sapply(theta,parametric_rejection_norm,m=10,n=10,repl=10000,p=0.05)
library(lattice)
xyplot(c~theta,type="l")
