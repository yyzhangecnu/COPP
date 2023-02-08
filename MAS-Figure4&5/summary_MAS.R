load("MAS_Sweight.Rdata")
library(ggplot2)
CP_MAS_Sw <- unlist(CovProb)
AL_MAS_Sw <- unlist(AveLeng)

load("MAS_Dweight.Rdata")
library(ggplot2)
CP_MAS_Dw <- unlist(CovProb)
AL_MAS_Dw <- unlist(AveLeng)

load("MAS_BootSweight.Rdata")
library(ggplot2)
CP_MAS_BSw <- unlist(CovProb)
AL_MAS_BSw <- unlist(AveLeng)

load("MAS_BootDweight.Rdata")
library(ggplot2)
CP_MAS_BDw <- unlist(CovProb)
AL_MAS_BDw <- unlist(AveLeng)

###############################
load("nips_true.Rdata")
library(ggplot2)
CP_nips_true <- unlist(CovProb)
AL_nips_true <- unlist(AveLeng)

load("nips_false.Rdata")
library(ggplot2)
CP_nips_false <- unlist(CovProb)
AL_nips_false <- unlist(AveLeng)


PlotData_MAS_CP<- data.frame(CP=c(CP_MAS_Sw,CP_MAS_Dw,CP_MAS_BSw,CP_MAS_BDw,CP_nips_true,CP_nips_false),Methods=c(rep("COPP",100),rep("COPP-IS",100),rep("COPP-MS",100),rep("COPP-ISMS",100),rep("Tau-TDen",100),rep("Tau-FDen",100)))
PlotData_MAS_CP$Methods <- factor(PlotData_MAS_CP$Methods,levels=c('COPP','COPP-IS','COPP-MS','COPP-ISMS','Tau-TDen',"Tau-FDen"),ordered = TRUE)

#Average Length
PlotData_MAS_AL<- data.frame(AL=c(AL_MAS_Sw,AL_MAS_Dw,AL_MAS_BSw,AL_MAS_BDw,AL_nips_true,AL_nips_false),Methods=c(rep("COPP",100),rep("COPP-IS",100),rep("COPP-MS",100),rep("COPP-ISMS",100),rep("Tau-TDen",100),rep("Tau-FDen",100)))
PlotData_MAS_AL$Methods <- factor(PlotData_MAS_AL$Methods,levels=c('COPP','COPP-IS','COPP-MS','COPP-ISMS','Tau-TDen',"Tau-FDen"),ordered = TRUE)


library(stringr)
p1 = ggplot(PlotData_MAS_CP,aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+geom_hline(yintercept = 0.9,size=0.8)+theme(legend.position="None")+ggtitle("MAS")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
p2 = ggplot(PlotData_MAS_AL,aes(x=Methods, y=AL, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+theme(legend.position="None")+ggtitle("MAS")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
#library(dplyr)
#library(ggpubr)
#ggarrange(p1,p2,p3,p4,nrow=1,ncol=4)
#p4 = ggplot(PlotData[[4]],aes(x=Methods, y=AL, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=20))+theme(legend.position="right")+ggtitle("Two Stages")+theme(plot.title = element_text(hjust = 0.5,size=25))+theme(axis.text = element_text(face="bold"))+scale_x_discrete(labels=function(x) str_wrap(x,width=2))




