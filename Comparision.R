capon_Exp=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- capon_rejection (30, 30) -- rExp -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

capon_Norm=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- capon_rejection (30, 30) -- rNorm -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

capon_Weib=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- capon_rejection (30, 30) -- rWeibull -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

capon_Gam=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- capon_rejection (30, 30) -- rGamma -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

capon_Log=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- capon_rejection (30, 30) -- rLogis -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]








savage_Exp=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- savage_rejection (30, 30) -- rExp -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

savage_Norm=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- savage_rejection (30, 30) -- rNorm -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

savage_Weib=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- savage_rejection (30, 30) -- rWeibull -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

savage_Gam=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- savage_rejection (30, 30) -- rGamma -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

#savage_Log=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- savage_rejection (30, 30) -- rLogis -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]








para_Exp=as.vector(read.csv("~/Desktop/GIT/Nonparametric/data/power -- parametric_rejection (30, 30) -- rExp -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

para_Log=as.vector(read.csv("~/Desktop/GIT/Nonparametric/data/power -- parametric_rejection (30, 30) -- rLogis -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

para_Weib=as.vector(read.csv("~/Desktop/GIT/Nonparametric/data/power -- parametric_rejection (30, 30) -- rWeibull -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

para_Norm=as.vector(read.csv("~/Desktop/GIT/Nonparametric/data/power -- parametric_rejection (30, 30) -- rNorm -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

para_Gam=as.vector(read.csv("~/Desktop/GIT/Nonparametric/data/power -- parametric_rejection (30, 30) -- rGamma -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]





#para_Gam
theta=seq(5,1,len=1000)

#theta
data=data.frame(cbind(capon_Exp,savage_Exp,para_Exp,capon_Gam,savage_Gam,para_Gam,capon_Log,
           para_Log,capon_Norm,savage_Norm,para_Norm,capon_Weib,savage_Weib,para_Weib,theta))

library(ggplot2)
library(ggpubr)

c_exp=ggplot(data=data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Exp,colour="Capon"))+
  geom_line(aes(y=para_Exp,colour="Para"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Para"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Exponential",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())

c_exp
c_log=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Log,colour="Capon"))+
  geom_line(aes(y=para_Log,colour="Para"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Para"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Logistic",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())

c_normal=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Norm,colour="Capon"))+
  geom_line(aes(y=para_Norm,colour="Para"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Para"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Normal",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())





c_weibull=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Weib,colour="Capon"))+
  geom_line(aes(y=para_Weib,colour="Para"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Para"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Weibull",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())



c_gamma=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Gam,colour="Capon"))+
  geom_line(aes(y=para_Gam,colour="Para"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Para"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Gamma",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())



figure=ggarrange(c_normal,c_log,c_exp,c_weibull,c_gamma,ncol=2,nrow = 3)
annotate_figure(figure,left = text_grob("Power Of The Test Statistic",rot=90),
                bottom = text_grob("theta",size = 16),top =text_grob("Comparision Between F And Capon",face="bold"))
##########################################





c_exp=ggplot(data=data,aes(theta))+ylim(0,1)+geom_line(aes(y=savage_Exp,colour="Savage"))+
  geom_line(aes(y=para_Exp,colour="Para"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Savage"="red","Para"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Exponential",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())

c_exp
# c_log=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=savage_Log,colour="Savage"))+
#   geom_line(aes(y=para_Log,colour="Para"))+xlim(0.95,5.05)+
#   scale_colour_manual(values =c("Savage"="red","Para"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
#   labs(title = "Logistic",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
#   theme(axis.title.y = element_blank(),axis.title.x = element_blank())

c_normal=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=savage_Norm,colour="Savage"))+
  geom_line(aes(y=para_Norm,colour="Para"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Savage"="red","Para"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Normal",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())





c_weibull=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=savage_Weib,colour="Savage"))+
  geom_line(aes(y=para_Weib,colour="Para"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Savage"="red","Para"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Weibull",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())



c_gamma=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=savage_Gam,colour="Savage"))+
  geom_line(aes(y=para_Gam,colour="Para"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Savage"="red","Para"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Gamma",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())



figure=ggarrange(c_normal,c_exp,c_weibull,c_gamma,ncol=2,nrow = 3)
annotate_figure(figure,left = text_grob("Power Of The Test Statistic",rot=90),
                bottom = text_grob("theta",size = 16),top =text_grob("Comparision Between F And Savage",face="bold"))
##################################################



c_exp=ggplot(data=data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Exp,colour="Capon"))+
  geom_line(aes(y=savage_Exp,colour="Savage"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Savage"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Exponential",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())

c_exp
# c_log=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Log,colour="Capon"))+
#   geom_line(aes(y=savage_Log,colour="Savage"))+xlim(0.95,5.05)+
#   scale_colour_manual(values =c("Capon"="red","Savage"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
#   labs(title = "Logistic",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
#   theme(axis.title.y = element_blank(),axis.title.x = element_blank())

c_normal=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Norm,colour="Capon"))+
  geom_line(aes(y=savage_Norm,colour="Savage"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Savage"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Normal",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())





c_weibull=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Weib,colour="Capon"))+
  geom_line(aes(y=savage_Weib,colour="Savage"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Savage"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Weibull",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())



c_gamma=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Gam,colour="Capon"))+
  geom_line(aes(y=savage_Gam,colour="Savage"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Savage"="blue"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Gamma",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())



figure=ggarrange(c_normal,c_exp,c_weibull,c_gamma,ncol=2,nrow = 3)
annotate_figure(figure,left = text_grob("Power Of The Test Statistic",rot=90),
                bottom = text_grob("theta",size = 16),top =text_grob("Comparision Between Savage And Capon",face="bold"))
#################################################







c_exp=ggplot(data=data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Exp,colour="Capon"))+
  geom_line(aes(y=para_Exp,colour="Para"))+geom_line(aes(y=savage_Exp,colour="Savage"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Para"="blue","Savage"="green"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Exponential",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())

c_exp
# c_log=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Log,colour="Capon"))+
#   geom_line(aes(y=para_Log,colour="Para"))+geom_line(aes(y=savage_log,colour="Savage"))+xlim(0.95,5.05)+
#   scale_colour_manual(values =c("Capon"="red","Para"="blue","Savage"="green"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
#   labs(title = "Logistic",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
#   theme(axis.title.y = element_blank(),axis.title.x = element_blank())

c_normal=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Norm,colour="Capon"))+
  geom_line(aes(y=para_Norm,colour="Para"))+geom_line(aes(y=savage_Norm,colour="Savage"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Para"="blue","Savage"="green"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Normal",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())





c_weibull=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Weib,colour="Capon"))+
  geom_line(aes(y=para_Weib,colour="Para"))+geom_line(aes(y=savage_Weib,colour="Savage"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Para"="blue","Savage"="green"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Weibull",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())



c_gamma=ggplot(data,aes(theta))+ylim(0,1)+geom_line(aes(y=capon_Gam,colour="Capon"))+
  geom_line(aes(y=para_Gam,colour="Para"))+geom_line(aes(y=savage_Exp,colour="Savage"))+xlim(0.95,5.05)+
  scale_colour_manual(values =c("Capon"="red","Para"="blue","Savage"="green"))+theme(legend.position=c(0.15,0.9),legend.key.size = unit(0.1,"cm"))+
  labs(title = "Gamma",colour="Colour")+theme(plot.title = element_text(hjust = 0.5,size = 10))+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())



figure=ggarrange(c_normal,c_exp,c_weibull,c_gamma,ncol=2,nrow = 3)
annotate_figure(figure,left = text_grob("Power Of The Test Statistic",rot=90),
                bottom = text_grob("theta",size = 16),top =text_grob("Comparision Between F, Savage And Capon",face="bold"))
##########################################################










