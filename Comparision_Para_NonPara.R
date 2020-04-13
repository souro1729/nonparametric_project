


## First generate the follownig data using "parametric_rejection.R" 
## and select the path where it is saved. 

##For plotting purpose please run "theme_mine.R" before plotting
Comp_Plot=function(){
Dir_name=file.path(getwd(),"New")
source("theme_mine.R")


capon_Exp=as.vector(read.csv(file.path(Dir_name,"power -- capon_rejection (30, 30) -- rExp -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

capon_Norm=as.vector(read.csv(file.path(Dir_name,"power -- capon_rejection (30, 30) -- rNorm -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

capon_Weib=as.vector(read.csv(file.path(Dir_name,"power -- capon_rejection (30, 30) -- rWeibull -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

capon_Gam=as.vector(read.csv(file.path(Dir_name,"power -- capon_rejection (30, 30) -- rGamma -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

capon_Log=as.vector(read.csv(file.path(Dir_name,"power -- capon_rejection (30, 30) -- rLogis -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]








savage_Exp=as.vector(read.csv(file.path(Dir_name,"power -- savage_rejection (30, 30) -- rExp -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

savage_Norm=as.vector(read.csv(file.path(Dir_name,"power -- savage_rejection (30, 30) -- rNorm -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

savage_Weib=as.vector(read.csv(file.path(Dir_name,"power -- savage_rejection (30, 30) -- rWeibull -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

savage_Gam=as.vector(read.csv(file.path(Dir_name,"power -- savage_rejection (30, 30) -- rGamma -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

savage_Log=as.vector(read.csv(file.path(Dir_name,"power -- savage_rejection (30, 30) -- rLogis -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]








para_Exp=as.vector(read.csv(file.path(Dir_name,"power -- parametric_rejection (30, 30) -- rExp -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

para_Log=as.vector(read.csv(file.path(Dir_name,"power -- parametric_rejection (30, 30) -- rLogis -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

para_Weib=as.vector(read.csv(file.path(Dir_name,"power -- parametric_rejection (30, 30) -- rWeibull -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

para_Norm=as.vector(read.csv(file.path(Dir_name,"power -- parametric_rejection (30, 30) -- rNorm -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]

para_Gam=as.vector(read.csv(file.path(Dir_name,"power -- parametric_rejection (30, 30) -- rGamma -- 0.20, 1.00, 1000.00 -- repl=1000.csv")))[,-1]




require(ggplot2)
require(dplyr)

require(tidyr)
#para_Gam


#theta

Parametric=data.frame(cbind(para_Exp,para_Gam,para_Log,para_Norm,para_Weib))
Capon=data.frame(cbind(capon_Exp,capon_Gam,capon_Log,capon_Norm,capon_Weib))
Savage=data.frame(cbind(savage_Exp,savage_Gam,savage_Log,savage_Norm,savage_Weib))
theta=seq(0.2,1,len=1000)








PC=cbind(Parametric,Capon)
encode_pc=gather(cbind(PC,theta),key = "measure",value = "value",c(-"theta"))
Distribution_pc=rep(c("Exponential","Gamma","Logistic","Normal","Weibull"),each=1000,times=2)
Statistic_pc=rep(c("F","Capon"),each=5000)
#theta=rep(seq(1,5,length=1000),times=10)
#df1=melt(df) %>% select(-variable) %>% mutate(Distribution_pc=Distribution_pc,Statistic_pc=
#                                                                     Statistic_pc, theta=theta)




encode_pc["measure"]=Statistic_pc
encode_pc=cbind(encode_pc,Distribution_pc)

pc=ggplot(encode_pc,aes(x=theta,y=value,colour=Statistic_pc))+geom_line()+facet_wrap(~Distribution_pc,ncol=3)+
  labs(title = "Comparison of power graph of\n F and Capon  statistic",y="Power",color="Statistic")+xlim(1.05,0.15)+
  theme_mine()















PS=cbind(Parametric,Savage,theta)
encode_ps=gather(PS,key = "measure",value = "value",c(-"theta"))
Distribution_ps=rep(c("Exponential","Gamma","Logistic","Normal","Weibull"),each=1000,times=2)
Statistic_ps=rep(c("F","Savage"),each=5000)
# theta=rep(seq(1,5,length=1000),times=10)
#df1=melt(df) %>% select(-variable) %>% mutate(Distribution_ps=Distribution_ps,Statistic_ps=
#                                                                     Statistic_ps, theta=theta)




encode_ps["measure"]=Statistic_ps
encode_ps=cbind(encode_ps,Distribution_ps)


ps=ggplot(encode_ps,aes(x=theta,y=value,colour=Statistic_ps))+geom_line()+facet_wrap(~Distribution_ps,ncol=3)+
  labs(title = "Comparison of power graph of\n F and Savage  statistic",color="Statistic")+xlim(1.05,0.15)+
  theme_mine()











CS=cbind(Capon,Savage,theta)
encode_cs=gather(CS,key = "measure",value = "value",c(-"theta"))
Distribution_cs=rep(c("Exponential","Gamma","Logistic","Normal","Weibull"),each=1000,times=2)
Statistic_cs=rep(c("Capon","Savage"),each=5000)
# theta=rep(seq(1,5,length=1000),times=10)
#df1=melt(df) %>% select(-variable) %>% mutate(Distribution_cs=Distribution_cs,Statistic_cs=
#                                                                     Statistic_cs, theta=theta)




encode_cs["measure"]=Statistic_cs

encode_cs=cbind(encode_cs,Distribution_cs)


cs=ggplot(encode_cs,aes(x=theta,y=value,colour=Statistic_cs))+geom_line()+facet_wrap(~Distribution_cs,ncol=3)+
  labs(title = "Comparison of power graph of\n Capon and Savage  statistic",color="Statistic")+xlim(1.05,0.15)+
  theme_mine()




PCS=cbind(Parametric,Capon,Savage,theta)
encode_pcs=gather(PCS,key = "measure",value = "value",c(-"theta"))
Distribution_pcs=rep(c("Exponential","Gamma","Logistic","Normal","Weibull"),each=1000,times=3)
Statistic_pcs=rep(c("F","Capon","Savage"),each=5000)
# theta=rep(seq(1,5,length=1000),times=10)
#df1=melt(df) %>% select(-variable) %>% mutate(Distribution_pcs=Distribution_pcs,Statistic_pcs=
#                                                                     Statistic_pcs, theta=theta)




encode_pcs["measure"]=Statistic_pcs

encode_pcs=cbind(encode_pcs,Distribution_pcs)


pcs=ggplot(encode_pcs,aes(x=theta,y=value,colour=Statistic_pcs))+geom_line()+facet_wrap(~Distribution_pcs,ncol=3)+
  labs(title = "Comparison of power graph of \nF, Capon and Savage  statistic",color="Statistic")+xlim(1.05,0.15)+
  theme_mine()
mylist=list("pc"=pc,"cs"=cs,"ps"=ps,"pcs"=pcs)
return(mylist)

}