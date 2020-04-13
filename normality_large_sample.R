


require(ggplot2)
Chii=function(n,m=40,breaking=20){
  set.seed(42)
  require(zoo)
  z=data.frame(replicate(n,(rchisq(1,m)-m)/sqrt(2*m)))
  names(z)="x"
  x=as.numeric(as.character(unlist(z[1])))
  p1=hist(x,breaks = breaking)
  shap_test=shapiro.test(x)
  breaks_cdf <- pnorm(p1$breaks)
  null.probs <- rollapply(breaks_cdf, 2, function(x) x[2]-x[1])
  chi_test <- chisq.test(p1$counts, p=null.probs, rescale.p=TRUE, simulate.p.value=TRUE)
  my_list=list("data"=z,"hist"=p1,"shapiro_test"=shap_test,"chisq.test"=chi_test)
  return(my_list)
}













