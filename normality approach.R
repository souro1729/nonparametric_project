#asymptotics for Capon's test
library(sm)

repl = 1000
mvec = seq(10, 20, 5)
nvec = seq(10, 20, 5)

par(mfrow=c(3,3))

for(m in mvec){
  for (n in nvec){
    data = replicate(repl, simulate_capon(m,n))
    sm.density(data)
  }
}

sm.density(M[1,])
