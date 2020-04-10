S_Weibull<- read.csv("~/Desktop/GIT/Nonparametric/Small/power -- parametric_rejection (10, 10) -- rWeibull -- 0.10, 1.00, 1000.00 -- repl=10000.csv")
S_Normal<-read.csv("~/Desktop/GIT/Nonparametric/Small/power -- parametric_rejection (10, 10) -- rNorm -- 0.10, 1.00, 1000.00 -- repl=10000.csv")
S_Gamma<-read.csv("~/Desktop/GIT/Nonparametric/Small/power -- parametric_rejection (10, 10) -- rGamma -- 0.10, 1.00, 1000.00 -- repl=10000.csv")
S_Exp<-read.csv("~/Desktop/GIT/Nonparametric/Small/power -- parametric_rejection (10, 10) -- rExp -- 0.10, 1.00, 1000.00 -- repl=10000.csv")
S_Logis<-read.csv("~/Desktop/GIT/Nonparametric/Small/power -- parametric_rejection (10, 10) -- rLogis -- 0.10, 1.00, 1000.00 -- repl=10000.csv")


L_Weibull<- read.csv("~/Desktop/GIT/Nonparametric/New/power -- parametric_rejection (50, 50) -- rWeibull -- 0.20, 1.00, 1000.00 -- repl=10000.csv")
L_Normal<-read.csv("~/Desktop/GIT/Nonparametric/New/power -- parametric_rejection (50, 50) -- rNorm -- 0.20, 1.00, 1000.00 -- repl=10000.csv")
L_Gamma<-read.csv("~/Desktop/GIT/Nonparametric/New/power -- parametric_rejection (50, 50) -- rGamma -- 0.20, 1.00, 1000.00 -- repl=10000.csv")
L_Exp<-read.csv("~/Desktop/GIT/Nonparametric/New/power -- parametric_rejection (50, 50) -- rExp -- 0.20, 1.00, 1000.00 -- repl=10000.csv")
L_Logis<-read.csv("~/Desktop/GIT/Nonparametric/New/power -- parametric_rejection (50, 50) -- rLogis -- 0.20, 1.00, 1000.00 -- repl=10000.csv")


L_Weibull=L_Weibull[,-1]
L_Normal=L_Normal[,-1]
L_Gamma=L_Gamma[,-1]
L_Exp=L_Exp[,-1]
L_Logis=L_Logis[,-1]


theta=seq(0.10,1,len=1000)
theta2=seq(0.20,1,len=1000)

library(lattice)
xyplot(S_Weibull+S_Normal+S_Gamma+S_Exp+S_Logis~theta,xlim = c(1.05,0),type="l",
       auto.key = T,grid=T,outer = F,
       main="Comparison of Power Curve in Parametric Case (F-dis)",
       panel = function(...){
         panel.xyplot(...)
         panel.abline(h=c(0.05,1),col.line = c("blue","red"))
       })




xyplot(L_Weibull+L_Normal+L_Gamma+L_Exp+L_Logis~theta2,xlim = c(1.05,0),type="l",
       auto.key = T,grid=T,outer = F,
       main="Comparison of Power Curve in Parametric\n Case Large Sample (F-dis)",
       panel = function(...){
         panel.xyplot(...)
         panel.abline(h=c(0.05,1),col.line = c("blue","red"))
       })
