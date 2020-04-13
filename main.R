#####
print("Run this Code....")
require(zoo)
require(ggplot2)
require(dplyr)
require(pbapply)
require(lattice)
require(grid)
require(tidyr)

Dirname=getwd()
Dirname
subDir <- "Sourojyoti"
file=file.path(Dirname,subDir)
dir.create(file, showWarnings = TRUE)
setwd(file.path(Dirname, subDir))
dir.create("New")

warning("Please Connect To your internet...")
download.file("https://github.com/souro1729/nonparametric_project/raw/master/Souro.zip",destfile = "Souro.zip")
unzip("souro.zip",exdir = file)

source("modified_draws.R")
source("parametric_rejection.R")

warning("Get Power Curve. It takes Lots of time so you can Skip\n to parametric plotting line 77
        or large sample check line 56,\n this data is already stored in  file: New....")





warning("Generating Data....")
power_curve(30,30,len=1000,repl = 1000)
power_curve(30,30,len=1000,repl=1000,rdist = rExp)
power_curve(30,30,len=1000,repl=1000,rdist = rGamma)
power_curve(30,30,len=1000,repl=1000,rdist = rLogis)
power_curve(30,30,len=1000,repl=1000,rdist = rWeibull)





#small_sample
power_curve(10,10,len=1000,repl = 10000,from=0.1)
power_curve(10,10,len=1000,repl=500,rdist = rExp,from = 0.1)
###power_curve(10,10,len=1000,repl=10000,rdist = rExp,from=0.045)
power_curve(10,10,len=1000,repl=10000,rdist = rGamma,from=0.1)
power_curve(10,10,len=1000,repl=10000,rdist = rLogis,from=0.1)
power_curve(10,10,len=1000,repl=10000,rdist = rWeibull,from=0.1)





#Large_Sample
warning("Check For Normality")
source("normality_large_sample.R")
a=Chii(n=50,m=50,breaking = 25)

a$shapiro_test
a$chisq.test

ggplot(data=a$data,aes(sample=a$data[,1]))+geom_qq(colour="red")+geom_qq_line(colour="blue")

warning("This also in the file: New...\n
        You can skip to line 79...")
warning("Generating data for large sample....")

power_curve(50,50,len=1000,repl = 10000)
power_curve(50,50,len=1000,repl=10000,rdist = rExp)
power_curve(50,50,len=1000,repl=10000,rdist = rGamma)
power_curve(50,50,len=1000,repl=10000,rdist = rLogis)
power_curve(50,50,len=1000,repl=10000,rdist = rWeibull)


# Drawing the parametric curve F statistics

source("Parametric_plotting.R")
warning("Plotting Fumction")

t=plotting()
t$Small_outer
t$Small
t$Large_outer
t$Large

source("modified_draws.R")
source("Nonpara.R")
source("Capon.R")
source("savage.R")

warning("this also need non_paramatric data, which may take long time.\n Skip to 133
         You can get the generated in the file: New...")


warning("Generating Data")
power_curve(30, 30, len = 1000, rdist = rWeibull, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rNorm, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rExp, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rGamma, stat_rejection = savage_rejection, repl=1000)

power_curve(30, 20, len = 1000, rdist = rWeibull, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rNorm, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rExp, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rGamma, stat_rejection = savage_rejection, repl=1000)

power_curve(20, 30, len = 1000, rdist = rWeibull, stat_rejection = savage_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rNorm, stat_rejection = savage_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rExp, stat_rejection = savage_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rGamma, stat_rejection = savage_rejection, repl=1000)



power_curve(30, 30, len = 1000, rdist = rWeibull, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rNorm, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rExp, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rGamma, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rLogis, stat_rejection = capon_rejection, repl=1000)

power_curve(30, 20, len = 1000, rdist = rWeibull, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rNorm, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rExp, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rGamma, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rLogis, stat_rejection = capon_rejection, repl=1000)

power_curve(20, 30, len = 1000, rdist = rWeibull, stat_rejection = capon_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rNorm, stat_rejection = capon_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rExp, stat_rejection = capon_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rGamma, stat_rejection = capon_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rLogis, stat_rejection = capon_rejection, repl=1000)

warning("Nonparametric Power curve Draw")
warning("plotting nonparametric curve")
plot_power(30, 30, "savage", c("Norm", "Weibull", "Gamma", "Exp"))
plot_power(30,30,"capon",c("Norm", "Weibull", "Gamma", "Exp"))

source('Comparision_Para_NonPara.R')
comp_plot=Comp_Plot()
comp_plot$pc
comp_plot$ps
comp_plot$cs
comp_plot$pcs







