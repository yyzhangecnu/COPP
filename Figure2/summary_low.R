load("SS_low_IPWE.Rdata")
CP_low_IPW <- IPW_CP[[1]]
AL_low_IPW <- IPW_AL[[1]]

CP_low_AIPW <- AIPW_CP[[1]]
AL_low_AIPW <- AIPW_AL[[1]]

load("SS_low_Naive.Rdata")
library(ggplot2)
CP_low_Naive <- unlist(CovProb)
AL_low_Naive <- unlist(AveLeng)
#AL_Oracle <- mean(unlist(ALOrac))

load("SS_low_Sweight.Rdata")
library(ggplot2)
CP_low_Sweight <- unlist(CovProb)
AL_low_Sweight <- unlist(AveLeng)

load("SS_low_Dweight.Rdata")
library(ggplot2)
CP_low_Dweight <- unlist(CovProb)
AL_low_Dweight <- unlist(AveLeng)

load("SS_low_BootSweight.Rdata")
library(ggplot2)
CP_low_BootSweight <- unlist(CovProb)
AL_low_BootSweight <- unlist(AveLeng)

load("SS_low_BootDweight.Rdata")
library(ggplot2)
CP_low_BootDweight <- unlist(CovProb)
AL_low_BootDweight <- unlist(AveLeng)

PlotData_SS_low_CP<- data.frame(CP=c(CP_low_IPW,CP_low_AIPW,CP_low_Naive,CP_low_Sweight,CP_low_Dweight,CP_low_BootSweight,CP_low_BootDweight),Methods=c(rep("IS",100),rep("DR",100),rep("SM",100),rep("COPP",100),rep("COPP-IS",100),rep("COPP-MS",100),rep("COPP-IS-MS",100)))
PlotData_SS_low_CP$Methods <- factor(PlotData_SS_low_CP$Methods,levels=c('IS','DR','SM','COPP','COPP-IS','COPP-MS','COPP-IS-MS'),ordered = TRUE)

#Average Length
PlotData_SS_low_AL<- data.frame(AL=c(AL_low_IPW,AL_low_AIPW,AL_low_Naive,AL_low_Sweight,AL_low_Dweight,AL_low_BootSweight,AL_low_BootDweight),Methods=c(rep("IS",100),rep("DR",100),rep("SM",100),rep("COPP",100),rep("COPP-IS",100),rep("COPP-MS",100),rep("COPP-IS-MS",100)))
PlotData_SS_low_AL$Methods <- factor(PlotData_SS_low_AL$Methods,levels=c('IS','DR','SM','COPP','COPP-IS','COPP-MS','COPP-IS-MS'),ordered = TRUE)


load("TS_low_IPWE.Rdata")
CP_low_IPW <- IPW_CP[[1]]
AL_low_IPW <- IPW_AL[[1]]

CP_low_AIPW <- AIPW_CP[[1]]
AL_low_AIPW <- AIPW_AL[[1]]

load("TS_low_Naive2.Rdata")
library(ggplot2)
CP_low_Naive <- unlist(CovProb)
AL_low_Naive <- unlist(AveLeng)
#AL_Oracle <- mean(unlist(ALOrac))

load("TS_low_Sweight2.Rdata")
library(ggplot2)
CP_low_Sweight <- unlist(CovProb)
AL_low_Sweight <- unlist(AveLeng)

load("TS_low_Dweight2.Rdata")
library(ggplot2)
CP_low_Dweight <- unlist(CovProb)
AL_low_Dweight <- unlist(AveLeng)

load("TS_low_BootSweight2.Rdata")
library(ggplot2)
CP_low_BootSweight <- unlist(CovProb)
AL_low_BootSweight <- unlist(AveLeng)

load("TS_low_BootDweight2.Rdata")
library(ggplot2)
CP_low_BootDweight <- unlist(CovProb)
AL_low_BootDweight <- unlist(AveLeng)

PlotData_TS_low_CP<- data.frame(CP=c(CP_low_IPW,CP_low_AIPW,CP_low_Naive,CP_low_Sweight,CP_low_Dweight,CP_low_BootSweight,CP_low_BootDweight),Methods=c(rep("IS",100),rep("DR",100),rep("SM",100),rep("COPP",100),rep("COPP-IS",100),rep("COPP-MS",100),rep("COPP-IS-MS",100)))
PlotData_TS_low_CP$Methods <- factor(PlotData_TS_low_CP$Methods,levels=c('IS','DR','SM','COPP','COPP-IS','COPP-MS','COPP-IS-MS'),ordered = TRUE)

#Average Length
PlotData_TS_low_AL<- data.frame(AL=c(AL_low_IPW,AL_low_AIPW,AL_low_Naive,AL_low_Sweight,AL_low_Dweight,AL_low_BootSweight,AL_low_BootDweight),Methods=c(rep("IS",100),rep("DR",100),rep("SM",100),rep("COPP",100),rep("COPP-IS",100),rep("COPP-MS",100),rep("COPP-IS-MS",100)))
PlotData_TS_low_AL$Methods <- factor(PlotData_TS_low_AL$Methods,levels=c('IS','DR','SM','COPP','COPP-IS','COPP-MS','COPP-IS-MS'),ordered = TRUE)

PlotData <- list(PlotData_SS_low_CP,PlotData_SS_low_AL,PlotData_TS_low_CP,PlotData_TS_low_AL)


p1 = ggplot(PlotData[[1]],aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+geom_hline(yintercept = 0.9,size=0.8)+theme(legend.position="None")+ggtitle("Low dim, Single Stage")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
p2 = ggplot(PlotData[[2]],aes(x=Methods, y=AL, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+theme(legend.position="None")+ggtitle("Low dim, Single Stage")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
p3 = ggplot(PlotData[[3]],aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+geom_hline(yintercept = 0.9,size=0.8)+theme(legend.position="None")+ggtitle("Low dim, Two Stages")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
p4 = ggplot(PlotData[[4]],aes(x=Methods, y=AL, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+theme(legend.position="None")+ggtitle("Low dim, Two Stages")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
#library(dplyr)
#library(ggpubr)
#ggarrange(p1,p2,p3,p4,nrow=1,ncol=4)
#p4 = ggplot(PlotData[[4]],aes(x=Methods, y=AL, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=20))+theme(legend.position="right")+ggtitle("Two Stages")+theme(plot.title = element_text(hjust = 0.5,size=25))+theme(axis.text = element_text(face="bold"))+scale_x_discrete(labels=function(x) str_wrap(x,width=2))




