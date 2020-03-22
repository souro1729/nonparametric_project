theta=seq(1,2,len=1000)


parametric_rejection_exp= function(m, n, 
                                   theta=1,
                                   repl = 1000, p = 0.05) #total replication, and level
{
  new=function(){
    x = rexp(m,rate = 1)
    y = rexp(n,rate = 1/theta)
    var.test(x, y, alternative = "less")$p.value
  }
  s=replicate(repl,new())
  length(which(s<p))/repl
}

#c=sapply(theta,parametric_rejection,m=1000,n=1000,repl=10000,p=0.05)
c3=sapply(theta,parametric_rejection_exp,m=1000,n=1000,repl=10000,p=0.05)
library(lattice)
xyplot(c3~theta,type="l")