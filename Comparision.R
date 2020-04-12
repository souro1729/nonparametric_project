capon_Exp=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- capon_rejection (30, 30) -- rExp -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

capon_Norm=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- capon_rejection (30, 30) -- rNorm -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

capon_Weib=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- capon_rejection (30, 30) -- rWeibull -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

capon_Gam=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- capon_rejection (30, 30) -- rGamma -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

capon_Log=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- capon_rejection (30, 30) -- rLogis -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]








savage_Exp=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- savage_rejection (30, 30) -- rExp -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

savage_Norm=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- savage_rejection (30, 30) -- rNorm -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

savage_Weib=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- savage_rejection (30, 30) -- rWeibull -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

savage_Gam=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- savage_rejection (30, 30) -- rGamma -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

savage_Log=as.vector(read.csv("~/Desktop/GIT/nonparametric_project/data2/power -- savage_rejection (30, 30) -- rLogis -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]








para_Exp=as.vector(read.csv("~/Desktop/GIT/Nonparametric/data/power -- parametric_rejection (30, 30) -- rExp -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

para_Log=as.vector(read.csv("~/Desktop/GIT/Nonparametric/data/power -- parametric_rejection (30, 30) -- rLogis -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

para_Weib=as.vector(read.csv("~/Desktop/GIT/Nonparametric/data/power -- parametric_rejection (30, 30) -- rWeibull -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

para_Norm=as.vector(read.csv("~/Desktop/GIT/Nonparametric/data/power -- parametric_rejection (30, 30) -- rNorm -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]

para_Gam=as.vector(read.csv("~/Desktop/GIT/Nonparametric/data/power -- parametric_rejection (30, 30) -- rGamma -- 0.20, 1.00, 1000.00 -- repl=1000.csv"))[,-1]






#para_Gam


#theta
data=data.frame(cbind(capon_Exp,savage_Exp,para_Exp,capon_Gam,savage_Gam,para_Gam,capon_Log,savage_Log,
           para_Log,capon_Norm,savage_Norm,para_Norm,capon_Weib,savage_Weib,para_Weib,theta))

Parametric=data.frame(cbind(para_Exp,para_Gam,para_Log,para_Norm,para_Weib))
Capon=data.frame(cbind(capon_Exp,capon_Gam,capon_Log,capon_Norm,capon_Weib))
Savage=data.frame(cbind(savage_Exp,savage_Gam,savage_Log,savage_Norm,savage_Weib))
theta=seq(5,1,len=1000)



library(ggplot2)
library(dplyr)
library(reshape2)




PC=cbind(Parametric,Capon)
encode_pc=gather(cbind(PC,theta),key = "measure",value = "value",c(-"theta"))
Distribution=rep(c("Exponential","Gamma","Logistic","Normal","Weibull"),each=1000,times=2)
Statistic=rep(c("F","Capon"),each=5000)
#theta=rep(seq(1,5,length=1000),times=10)
#df1=melt(df) %>% select(-variable) %>% mutate(Distribution=Distribution,Statistic=
#                                                                     Statistic, theta=theta)




encode_pc["measure"]=Statistic
encode_pc=cbind(encode_pc,Distribution)

ggplot(encode_pc,aes(x=theta,y=value,colour=Statistic))+geom_line()+facet_wrap(~Distribution,ncol=3,scales = "free")+
  labs(title = "Comparison of power graph of\n F and Capon  statistic",y="Power")+
  theme_mine()















PS=cbind(Parametric,Savage,theta)
encode_ps=gather(PS,key = "measure",value = "value",c(-"theta"))
Distribution=rep(c("Exponential","Gamma","Logistic","Normal","Weibull"),each=1000,times=2)
Statistic=rep(c("F","Savage"),each=5000)
# theta=rep(seq(1,5,length=1000),times=10)
#df1=melt(df) %>% select(-variable) %>% mutate(Distribution=Distribution,Statistic=
#                                                                     Statistic, theta=theta)




encode_ps["measure"]=Statistic
encode_ps
encode_ps=cbind(encode_ps,Distribution)
encode_ps

ggplot(encode_ps,aes(x=theta,y=value,colour=Statistic))+geom_line()+facet_wrap(~Distribution,ncol=3,scales = "free")+
  labs(title = "Comparison of power graph of\n F and Savage  statistic",y="Power")+
  theme_mine()











CS=cbind(Capon,Savage,theta)
encode_cs=gather(CS,key = "measure",value = "value",c(-"theta"))
Distribution=rep(c("Exponential","Gamma","Logistic","Normal","Weibull"),each=1000,times=2)
Statistic=rep(c("Capon","Savage"),each=5000)
# theta=rep(seq(1,5,length=1000),times=10)
#df1=melt(df) %>% select(-variable) %>% mutate(Distribution=Distribution,Statistic=
#                                                                     Statistic, theta=theta)




encode_cs["measure"]=Statistic

encode_cs=cbind(encode_cs,Distribution)


ggplot(encode_cs,aes(x=theta,y=value,colour=Statistic))+geom_line()+facet_wrap(~Distribution,ncol=3,scales = "free")+
  labs(title = "Comparison of power graph of\n Capon and Savage  statistic",y="Power")+
  theme_mine()




PCS=cbind(Parametric,Capon,Savage,theta)
encode_pcs=gather(PCS,key = "measure",value = "value",c(-"theta"))
Distribution=rep(c("Exponential","Gamma","Logistic","Normal","Weibull"),each=1000,times=3)
Statistic=rep(c("F","Capon","Savage"),each=5000)
# theta=rep(seq(1,5,length=1000),times=10)
#df1=melt(df) %>% select(-variable) %>% mutate(Distribution=Distribution,Statistic=
#                                                                     Statistic, theta=theta)




encode_pcs["measure"]=Statistic

encode_pcs=cbind(encode_pcs,Distribution)


ggplot(encode_pcs,aes(x=theta,y=value,colour=Statistic))+geom_line()+facet_wrap(~Distribution,ncol=3,scales = "free")+
  labs(title = "Comparison of power graph of \nF, Capon and Savage  statistic",y="Power")+
  theme_mine()




