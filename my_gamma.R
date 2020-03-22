theta=seq(1,2,len=1000)


parametric_rejection_gamma = function(m, n, 
                                 theta=1,
                                repl = 1000, p = 0.05,sc=9) #total replication, and level
{
  new=function(){
    x = rgamma(m,scale=sc,shape = 1)
    y = rgamma(m,scale=sc,shape = theta)
    var.test(x, y, alternative = "less")$p.value
  }
  s=replicate(repl,new())
  length(which(s<p))/repl
}

c1=sapply(theta,parametric_rejection_gamma,m=1000,n=1000,repl=10000,p=0.05)
library(lattice)
xyplot(c1~theta,type="l")
