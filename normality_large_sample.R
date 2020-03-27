c=seq(10,30,1)
Chii=function(n,m=40){
  replicate(n,(rchisq(1,m)-m)/sqrt(2*m))
  #sum(s)/n
}

library(pbapply)
d=Chii(n=40)

#dd=rnorm(100)
qqnorm(d)
qqline(d)
shapiro.test(d)
d1=Chii(n=15,m=10)
qqnorm(d1)
qqline(d1)
shapiro.test(d1)
