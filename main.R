#####
print("Run this Code....")



Dirname=getwd()
Dirname
subDir <- "Sourojyoti"
file=file.path(Dirname,subDir)
dir.create(file, showWarnings = TRUE)
setwd(file.path(Dirname, subDir))
dir.create("New")

download.file("https://github.com/souro1729/nonparametric_project/raw/master/Souro.zip",destfile = "Souro.zip")
unzip("souro.zip",exdir = file)



warning("Get Power Curve. It takes Lots of time so you can Skip to parametric plotting,\n this data is already stored in  file: New....")
source("modified_draws.R")
source("parametric_rejection.R")





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



warning("this also need non_paramatric data, which may take long time.\n
         You can get the generated in the file: New")
source('Comparision_Para_NonPara.R')
comp_plot=Comp_Plot()
comp_plot$pc
comp_plot$ps
comp_plot$cs
comp_plot$pcs






