load("SDM_H3_Sweight.Rdata")
library(ggplot2)
CP_SDM_H3_Sw <- unlist(CovProb)
AL_SDM_H3_Sw <- unlist(AveLeng)

load("SDM_H3_Dweight.Rdata")
library(ggplot2)
CP_SDM_H3_Dw <- unlist(CovProb)
AL_SDM_H3_Dw <- unlist(AveLeng)

load("SDM_H3_BootSweight.Rdata")
library(ggplot2)
CP_SDM_H3_BSw <- unlist(CovProb)
AL_SDM_H3_BSw <- unlist(AveLeng)

load("SDM_H3_BootDweight.Rdata")
library(ggplot2)
CP_SDM_H3_BDw <- unlist(CovProb)
AL_SDM_H3_BDw <- unlist(AveLeng)

###############################
load("SDM_H4_Sweight.Rdata")
library(ggplot2)
CP_SDM_H4_Sw <- unlist(CovProb)
AL_SDM_H4_Sw <- unlist(AveLeng)

load("SDM_H4_Dweight.Rdata")
library(ggplot2)
CP_SDM_H4_Dw <- unlist(CovProb)
AL_SDM_H4_Dw <- unlist(AveLeng)

load("SDM_H4_BootSweight.Rdata")
library(ggplot2)
CP_SDM_H4_BSw <- unlist(CovProb)
AL_SDM_H4_BSw <- unlist(AveLeng)

load("SDM_H4_BootDweight.Rdata")
library(ggplot2)
CP_SDM_H4_BDw <- unlist(CovProb)
AL_SDM_H4_BDw <- unlist(AveLeng)

################################
load("SDM_H5_Sweight.Rdata")
library(ggplot2)
CP_SDM_H5_Sw <- unlist(CovProb)
AL_SDM_H5_Sw <- unlist(AveLeng)

load("SDM_H5_Dweight.Rdata")
library(ggplot2)
CP_SDM_H5_Dw <- unlist(CovProb)
AL_SDM_H5_Dw <- unlist(AveLeng)

load("SDM_H5_BootSweight.Rdata")
library(ggplot2)
CP_SDM_H5_BSw <- unlist(CovProb)
AL_SDM_H5_BSw <- unlist(AveLeng)

load("SDM_H5_BootDweight.Rdata")
library(ggplot2)
CP_SDM_H5_BDw <- unlist(CovProb)
AL_SDM_H5_BDw <- unlist(AveLeng)

PlotData_SDM_CP<- data.frame(CP=c(CP_SDM_H3_Sw,CP_SDM_H3_Dw,CP_SDM_H3_BSw,CP_SDM_H3_BDw,CP_SDM_H4_Sw,CP_SDM_H4_Dw,CP_SDM_H4_BSw,CP_SDM_H4_BDw,CP_SDM_H5_Sw,CP_SDM_H5_Dw,CP_SDM_H5_BSw,CP_SDM_H5_BDw),Methods=c(rep("H3",100),rep("IS-H3",100),rep("MS-H3",100),rep("ISMS-H3",100),rep("H4",100),rep("IS-H4",100),rep("MS-H4",100),rep("ISMS-H4",100),rep("H5",100),rep("IS-H5",100),rep("MS-H5",100),rep("ISMS-H5",100)))
PlotData_SDM_CP$Methods <- factor(PlotData_SDM_CP$Methods,levels=c('H3','IS-H3','MS-H3','ISMS-H3','H4','IS-H4','MS-H4','ISMS-H4','H5','IS-H5','MS-H5','ISMS-H5'),ordered = TRUE)

#Average Length
PlotData_SDM_AL<- data.frame(AL=c(AL_SDM_H3_Sw,AL_SDM_H3_Dw,AL_SDM_H3_BSw,AL_SDM_H3_BDw,AL_SDM_H4_Sw,AL_SDM_H4_Dw,AL_SDM_H4_BSw,AL_SDM_H4_BDw,AL_SDM_H5_Sw,AL_SDM_H5_Dw,AL_SDM_H5_BSw,AL_SDM_H5_BDw),Methods=c(rep("H3",100),rep("IS-H3",100),rep("MS-H3",100),rep("ISMS-H3",100),rep("H4",100),rep("IS-H4",100),rep("MS-H4",100),rep("ISMS-H4",100),rep("H5",100),rep("IS-H5",100),rep("MS-H5",100),rep("ISMS-H5",100)))
PlotData_SDM_AL$Methods <- factor(PlotData_SDM_AL$Methods,levels=c('H3','IS-H3','MS-H3','ISMS-H3','H4','IS-H4','MS-H4','ISMS-H4','H5','IS-H5','MS-H5','ISMS-H5'),ordered = TRUE)


library(stringr)
p1 = ggplot(PlotData_SDM_CP,aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+geom_hline(yintercept = 0.9,size=0.8)+theme(legend.position="None")+ggtitle("SDM")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
p2 = ggplot(PlotData_SDM_AL,aes(x=Methods, y=AL, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+theme(legend.position="None")+ggtitle("Low dim, Single Stage")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
#library(dplyr)
#library(ggpubr)
#ggarrange(p1,p2,p3,p4,nrow=1,ncol=4)
#p4 = ggplot(PlotData[[4]],aes(x=Methods, y=AL, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=20))+theme(legend.position="right")+ggtitle("Two Stages")+theme(plot.title = element_text(hjust = 0.5,size=25))+theme(axis.text = element_text(face="bold"))+scale_x_discrete(labels=function(x) str_wrap(x,width=2))




