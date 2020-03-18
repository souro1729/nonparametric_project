#asymptotics for Capon's test
repl = seq(110,200,10)

data=function(n,m,rdist=rNorm,theta=1)
{
  sapply(repl,function(i) replicate(i,simulate_capon(n,m)))
}

library(sm)
M=data(10,10) #change and can see the graphs
par(mfrow=c(2,5))

for(i in 1:length(repl))
{
  sm.density(M[[i]])
}

sm.density(M[1,])
