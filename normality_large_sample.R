
Chii=function(n,m=40,breaking=20){
  library(zoo)
  x=replicate(n,(rchisq(1,m)-m)/sqrt(2*m))
  p1=hist(x,breaks = breaking)
  qplot=c(qqnorm(x),qqline(x))
  shap_test=shapiro.test(x)
  breaks_cdf <- pnorm(p1$breaks)
  null.probs <- rollapply(breaks_cdf, 2, function(x) x[2]-x[1])
  chi_test <- chisq.test(p1$counts, p=null.probs, rescale.p=TRUE, simulate.p.value=TRUE)
  my_list=list("hist"=p1,"qqplot"=qplot,"shapiro_test"=shap_test,"chisq.test"=chi_test)
  return(my_list)
}
a=Chii(n=50,m=50,breaking = 25)
a1=Chii(n=50,m=10,breaking = 25)
a1
a
