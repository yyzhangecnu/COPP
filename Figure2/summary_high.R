
load("SS_high_Naive.Rdata")
library(ggplot2)
CP_high_Naive <- unlist(CovProb)
AL_high_Naive <- unlist(AveLeng)
#AL_Oracle <- mean(unlist(ALOrac))

load("SS_high_Sweight.Rdata")
library(ggplot2)
CP_high_Sweight <- unlist(CovProb)
AL_high_Sweight <- unlist(AveLeng)

load("SS_high_Dweight.Rdata")
library(ggplot2)
CP_high_Dweight <- unlist(CovProb)
AL_high_Dweight <- unlist(AveLeng)

load("SS_high_BootSweight.Rdata")
library(ggplot2)
CP_high_BootSweight <- unlist(CovProb)
AL_high_BootSweight <- unlist(AveLeng)

load("SS_high_BootDweight.Rdata")
library(ggplot2)
CP_high_BootDweight <- unlist(CovProb)
AL_high_BootDweight <- unlist(AveLeng)

PlotData_SS_high_CP<- data.frame(CP=c(CP_high_Naive,CP_high_Sweight,CP_high_Dweight,CP_high_BootSweight,CP_high_BootDweight),Methods=c(rep("SM",100),rep("COPP",100),rep("COPP-IS",100),rep("COPP-MS",100),rep("COPP-IS-MS",100)))
PlotData_SS_high_CP$Methods <- factor(PlotData_SS_high_CP$Methods,levels=c('SM','COPP','COPP-IS','COPP-MS','COPP-IS-MS'),ordered = TRUE)

#Average Length
PlotData_SS_high_AL<- data.frame(AL=c(AL_high_Naive,AL_high_Sweight,AL_high_Dweight,AL_high_BootSweight,AL_high_BootDweight),Methods=c(rep("SM",100),rep("COPP",100),rep("COPP-IS",100),rep("COPP-MS",100),rep("COPP-IS-MS",100)))
PlotData_SS_high_AL$Methods <- factor(PlotData_SS_high_AL$Methods,levels=c('SM','COPP','COPP-IS','COPP-MS','COPP-IS-MS'),ordered = TRUE)



load("TS_high_Naive2.Rdata")
library(ggplot2)
CP_high_Naive <- unlist(CovProb)
AL_high_Naive <- unlist(AveLeng)
#AL_Oracle <- mean(unlist(ALOrac))

load("TS_high_Sweight2.Rdata")
library(ggplot2)
CP_high_Sweight <- unlist(CovProb)
AL_high_Sweight <- unlist(AveLeng)

load("TS_high_Dweight2.Rdata")
library(ggplot2)
CP_high_Dweight <- unlist(CovProb)
AL_high_Dweight <- unlist(AveLeng)

load("TS_high_BootSweight2.Rdata")
library(ggplot2)
CP_high_BootSweight <- unlist(CovProb)
AL_high_BootSweight <- unlist(AveLeng)

load("TS_high_BootDweight2.Rdata")
library(ggplot2)
CP_high_BootDweight <- unlist(CovProb)
AL_high_BootDweight <- unlist(AveLeng)

PlotData_TS_high_CP<- data.frame(CP=c(CP_high_Naive,CP_high_Sweight,CP_high_Dweight,CP_high_BootSweight,CP_high_BootDweight),Methods=c(rep("SM",100),rep("COPP",100),rep("COPP-IS",100),rep("COPP-MS",100),rep("COPP-IS-MS",100)))
PlotData_TS_high_CP$Methods <- factor(PlotData_TS_high_CP$Methods,levels=c('SM','COPP','COPP-IS','COPP-MS','COPP-IS-MS'),ordered = TRUE)

#Average Length
PlotData_TS_high_AL<- data.frame(AL=c(AL_high_Naive,AL_high_Sweight,AL_high_Dweight,AL_high_BootSweight,AL_high_BootDweight),Methods=c(rep("SM",100),rep("COPP",100),rep("COPP-IS",100),rep("COPP-MS",100),rep("COPP-IS-MS",100)))
PlotData_TS_high_AL$Methods <- factor(PlotData_TS_high_AL$Methods,levels=c('SM','COPP','COPP-IS','COPP-MS','COPP-IS-MS'),ordered = TRUE)

PlotData <- list(PlotData_SS_high_CP,PlotData_SS_high_AL,PlotData_TS_high_CP,PlotData_TS_high_AL)


p1 = ggplot(PlotData[[1]],aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+geom_hline(yintercept = 0.9,size=0.8)+theme(legend.position="None")+ggtitle("High dim, Single Stage")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
p2 = ggplot(PlotData[[2]],aes(x=Methods, y=AL, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+theme(legend.position="None")+ggtitle("High dim, Single Stage")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
p3 = ggplot(PlotData[[3]],aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+geom_hline(yintercept = 0.9,size=0.8)+theme(legend.position="None")+ggtitle("High dim, Two Stages")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
p4 = ggplot(PlotData[[4]],aes(x=Methods, y=AL, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=13))+theme(legend.position="None")+ggtitle("High dim, Two Stages")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=5))
#library(dplyr)
#library(ggpubr)
#ggarrange(p1,p2,p3,p4,nrow=1,ncol=4)
#p4 = ggplot(PlotData[[4]],aes(x=Methods, y=AL, color=Methods))+geom_boxplot()+xlab("")+theme(axis.title.y = element_text(size=20))+theme(legend.position="right")+ggtitle("Two Stages")+theme(plot.title = element_text(hjust = 0.5,size=25))+theme(axis.text = element_text(face="bold"))+scale_x_discrete(labels=function(x) str_wrap(x,width=2))

#m <- matrix(c(1,2,3,4,5,5,5,5),2,4,byrow = TRUE)
#layout(mat=m, heights = c(0.4,0.2))

#library(dplyr)
#library(ggpubr)
#ggarrange(p1,p2,p3,p4,nrow=1,ncol=4)

#plot(1,type="n",axes=FALSE,xlab="",ylab="")
#plot_colors <- c("blue","black", "green", "orange", "pink")
#legend(x = "top",inset = 0,
#       legend = c("Fabricated Metal", "Iron and Steel", "Paper","Beverages", "Tobacco"), 
#       col=plot_colors, lwd=5, cex=.5, horiz = TRUE)
# setEPS()
# postscript("LS_AL.eps")
# a2=ggplot(PlotData,aes(x=Methods, y=AL, color=Methods))+geom_boxplot()+xlab("")+theme(legend.position="None")
# a2=a2+ggtitle("high dim, Single stage")+theme(plot.title = element_text(hjust = 0.5))
# a2+scale_color_brewer(palette = "Set1")
# dev.off()
