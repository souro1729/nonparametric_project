theta=seq(1,2,len=1000)


parametric_rejection_weibull = function(m, n, 
                                     theta=1,
                                     repl = 1000, p = 0.05,sh=10) #total replication, and level
{
  new=function(){
    x = rweibull(m,shape = sh,scale = 1)
    y = rweibull(m,shape = sh,scale = theta)
    var.test(x, y, alternative = "less")$p.value
  }
  s=replicate(repl,new())
  length(which(s<p))/repl
}

c=sapply(theta,parametric_rejection_weibull,m=1000,n=1000,repl=10000,p=0.05)
library(lattice)
write.table(c,file = "Exp_rejection.csv",sep=",")
xyplot(c+c3~theta,type="l")
