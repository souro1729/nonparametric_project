

## First generate the follownig data using "parametric_rejection.R" 
## and select the path where it is saved. 

##For plotting purpose please run "theme_mine.R" before plotting



S_Weibull<- as.vector(read.csv("~/Desktop/GIT/Nonparametric/Small/power -- parametric_rejection (10, 10) -- rWeibull -- 0.10, 1.00, 1000.00 -- repl=10000.csv"))[,-1]
S_Normal<-as.vector(read.csv("~/Desktop/GIT/Nonparametric/Small/power -- parametric_rejection (10, 10) -- rNorm -- 0.10, 1.00, 1000.00 -- repl=10000.csv"))[,-1]
S_Gamma<-as.vector(read.csv("~/Desktop/GIT/Nonparametric/Small/power -- parametric_rejection (10, 10) -- rGamma -- 0.10, 1.00, 1000.00 -- repl=10000.csv"))[,-1]
S_Exp<-as.vector(read.csv("~/Desktop/GIT/Nonparametric/Small/power -- parametric_rejection (10, 10) -- rExp -- 0.10, 1.00, 1000.00 -- repl=10000.csv"))[,-1]
S_Logis<-as.vector(read.csv("~/Desktop/GIT/Nonparametric/Small/power -- parametric_rejection (10, 10) -- rLogis -- 0.10, 1.00, 1000.00 -- repl=10000.csv"))[,-1]


L_Weibull<-as.vector(read.csv("~/Desktop/GIT/Nonparametric/Large/power -- parametric_rejection (50, 50) -- rWeibull -- 0.20, 1.00, 1000.00 -- repl=10000.csv"))[,-1]
L_Normal<-as.vector(read.csv("~/Desktop/GIT/Nonparametric/Large/power -- parametric_rejection (50, 50) -- rNorm -- 0.20, 1.00, 1000.00 -- repl=10000.csv"))[,-1]
L_Gamma<-as.vector(read.csv("~/Desktop/GIT/Nonparametric/Large/power -- parametric_rejection (50, 50) -- rGamma -- 0.20, 1.00, 1000.00 -- repl=10000.csv"))[,-1]
L_Exp<-as.vector(read.csv("~/Desktop/GIT/Nonparametric/Large/power -- parametric_rejection (50, 50) -- rExp -- 0.20, 1.00, 1000.00 -- repl=10000.csv"))[,-1]
L_Logis<-as.vector(read.csv("~/Desktop/GIT/Nonparametric/Large/power -- parametric_rejection (50, 50) -- rLogis -- 0.20, 1.00, 1000.00 -- repl=10000.csv"))[,-1]

theta=seq(0.1,1,len=1000)
theta2=seq(0.2,1,len=1000)

data=data.frame(cbind(L_Normal,L_Gamma,L_Exp,L_Logis,L_Weibull,theta2))
data1=data.frame(cbind(S_Normal,S_Gamma,S_Exp,S_Logis,S_Weibull,theta))
#data
require(tidyr)
require(ggplot2)
econdata1 <- gather(data1, key="measure", value="value", c("S_Normal","S_Gamma","S_Exp","S_Logis","S_Weibull"))

econdata <- gather(data, key="measure", value="value", c("L_Normal","L_Gamma","L_Exp","L_Logis","L_Weibull"))



s1=ggplot(data = econdata1,aes(x=theta,y=value))+geom_line(colour="blue")+
        xlim(1.05,0.05)+
        facet_wrap(~measure)+theme(legend.position="")+
        geom_abline(slope = 0,intercept = 1,colour="red",linetype="dashed")+
        geom_abline(slope = 0,intercept = 0.05,colour="green",linetype="dashed")+
        xlab("sigma")+ylab("Power")+
        labs(title = "Comparison Of Power Curve in\n Parametric Case (F-Statistic)")+
       theme_mine() %+replace% theme(panel.spacing = unit(1, "lines"))
s1




s2=ggplot(data = data1,aes(x=theta))+geom_line(aes(y=S_Exp,colour="Exp"))+
        geom_line(aes(y=S_Gamma,colour="Gamma"))+geom_line(aes(y=S_Logis,colour="Logis"))+
        geom_line(aes(y=S_Normal,colour="Normal"))+geom_line(aes(y=S_Weibull,colour="Weibull"))+
        scale_colour_manual(values =c("Exp"="green","Gamma"="brown",
                                      "Logis"= "blue","Normal"="red","Weibull"="orange"))+
        geom_abline(slope = 0,intercept = 1,colour="black",linetype="dashed")+
        geom_abline(slope = 0,intercept = 0.05,colour="black",linetype="dashed")+xlim(1.05,0.05)+ylab("Power")+xlab("sigma")+
        labs(title = "Comparison Of Power Curve in\n Parametric Case (F-Statistic)",colour="Colour")+
        theme_mine()%+replace%theme(legend.position=c(0.15,0.7))
s2
#Large_outer_true  Large_outer_False
l1=ggplot(data = econdata,aes(x=theta2,y=value))+geom_line(colour="blue")+
        xlim(1.05,0.15)+
        facet_wrap(~measure)+theme(legend.position="")+
        geom_abline(slope = 0,intercept = 1,colour="red",linetype="dashed")+
        geom_abline(slope = 0,intercept = 0.05,colour="green",linetype="dashed")+
        xlab("theta")+ylab("Power")+
        labs(title = "Comparison Of Power Curve in\n Parametric Case Large Sample(F-Statistic)")+
        theme_mine()


l1


l2=ggplot(data = data,aes(x=theta2))+geom_line(aes(y=L_Exp,colour="Exp"))+
        geom_line(aes(y=L_Gamma,colour="Gamma"))+geom_line(aes(y=L_Logis,colour="Logis"))+
        geom_line(aes(y=L_Normal,colour="Normal"))+geom_line(aes(y=L_Weibull,colour="Weibull"))+
        scale_colour_manual(values =c("Exp"="green","Gamma"="brown",
                                      "Logis"= "blue","Normal"="red","Weibull"="orange"))+
        geom_abline(slope = 0,intercept = 1,colour="black",linetype="dashed")+
        geom_abline(slope = 0,intercept = 0.05,colour="black",linetype="dashed")+xlim(1.05,0.15)+ylab("Power")+xlab("theta")+
        labs(title = "Comparison Of Power Curve in\n Parametric Case (F-Statistic)",colour="Colour")+
       theme_mine()%+replace%theme(legend.position=c(0.15,0.7))

l2









# scale_colour_manual(values =c("S_Normal"="#E69F00","S_Gamma"="#56B4E9",
#                               "S_Exp"= "#009E73","S_Logis"="#F0E442","S_Weibull"="#0072B2"))

# library(lattice)
# xyplot(S_Weibull+S_Normal+S_Gamma+S_Exp+S_Logis~theta,xlim = c(1.05,0),type="l",
#        auto.key = T,grid=T,outer = F,
#        main="Comparison of Power Curve in Parametric Case (F-dis)",
#        panel = function(...){
#          panel.xyplot(...)
#          panel.abline(h=c(0.05,1),col.line = c("blue","red"))
#        })
# 
# 
# 
# 
# xyplot(L_Weibull+L_Normal+L_Gamma+L_Exp+L_Logis~theta2,xlim = c(1.05,0),type="l",
#        auto.key = T,grid=T,outer = F,
#        main="Comparison of Power Curve in Parametric\n Case Large Sample (F-dis)",
#        panel = function(...){
#          panel.xyplot(...)
#          panel.abline(h=c(0.05,1),col.line = c("blue","red"))
#        })






