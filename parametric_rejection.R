
#To run this code first run "modified_draws.R"


parametric_rejection=function(n,m,rdist=rNorm,theta=1,repl=10000,p=0.05){
  new=function(){
    x = rdist(m,theta= 1)
    y = rdist(n,theta = theta)
    var.test(y, x, alternative = "less")$p.value
}
  s=replicate(repl,new())
  length(which(s<p))/repl
}




power_curve <- function(n, m, from=0.2, to=1, len=30,
                        rdist=rNorm, stat_rejection = parametric_rejection,
                        repl=1000)
{
  require(pbapply)
  theta <- seq(from, to, length.out = len)
  power <- pbsapply(theta,function(i){

    stat_rejection(n, m, rdist = rdist, theta = i, repl = repl)
  })



  #Please create a Folder named "New" in your working directory...



 write.csv(power, sprintf("New/power -- %s (%.0f, %.0f) -- %s -- %.2f, %.2f, %.2f -- repl=%0.f.csv",
                           as.character(substitute(stat_rejection)), n, m,
                          as.character(substitute(rdist)), from, to, len, repl))

 require("lattice")
  xyplot(power~theta,type="l",xlim = c(to + 0.2, from - 0.2),ylim = c(0,1.05),grid=TRUE,
         panel = function(...){
    panel.xyplot(...)
    panel.abline(h=c(0.05,1),col.line = c("blue","red"))
  })
}







#Large_Sample_comparison
