load("TS_low_Naive2.Rdata")
library(ggplot2)
CP_low_Naive <- unlist(CovProb)
AL_low_Naive <- unlist(AveLeng)
#AL_Oracle <- mean(unlist(ALOrac))

load("TS_low_Sweight2.Rdata")
library(ggplot2)
CP_low_Sweight <- unlist(CovProb)
AL_low_Sweight <- unlist(AveLeng)

PlotData<- data.frame(CP=c(CP_low_Naive,CP_low_Sweight),Methods=c(rep("CP",100),rep("AOP",100)))
PlotData$Methods <- factor(PlotData$Methods,levels=c('CP','AOP'),ordered = TRUE)

setEPS()
postscript("LT.eps")
a1=ggplot(PlotData,aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+geom_hline(yintercept = 0.9,linetype="dashed",size=0.5)+theme(legend.position="None")
a1=a1+ggtitle("Low dim, Two stages")+theme(plot.title = element_text(hjust = 0.5))
a1+scale_color_brewer(palette = "Set1")
dev.off()

load("TS_high_Naive2.Rdata")
library(ggplot2)
CP_high_Naive <- unlist(CovProb)
AL_high_Naive <- unlist(AveLeng)

load("TS_high_Sweight2.Rdata")
library(ggplot2)
CP_high_Sweight <- unlist(CovProb)
AL_high_Sweight <- unlist(AveLeng)

PlotData<- data.frame(CP=c(CP_high_Naive,CP_high_Sweight),Methods=c(rep("CP",100),rep("AOP",100)))
PlotData$Methods <- factor(PlotData$Methods,levels=c('CP','AOP'),ordered = TRUE)

setEPS()
postscript("HT.eps")
a2=ggplot(PlotData,aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+geom_hline(yintercept = 0.9,linetype="dashed",size=0.5)+theme(legend.position="None")
a2=a2+ggtitle("High dim, Two stages")+theme(plot.title = element_text(hjust = 0.5))
a2+scale_color_brewer(palette = "Set1")
dev.off()

load("SS_low_Naive.Rdata")
library(ggplot2)
CP_low_Naive <- unlist(CovProb)
AL_low_Naive <- unlist(AveLeng)
#AL_Oracle <- mean(unlist(ALOrac))

load("SS_low_Sweight.Rdata")
library(ggplot2)
CP_low_Sweight <- unlist(CovProb)
AL_low_Sweight <- unlist(AveLeng)

PlotData<- data.frame(CP=c(CP_low_Naive,CP_low_Sweight),Methods=c(rep("CP",100),rep("AOP",100)))
PlotData$Methods <- factor(PlotData$Methods,levels=c('CP','AOP'),ordered = TRUE)

setEPS()
postscript("LS.eps")
a3=ggplot(PlotData,aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+geom_hline(yintercept = 0.9,linetype="dashed",size=0.5)+theme(legend.position="None")
a3=a3+ggtitle("Low dim, Single stage")+theme(plot.title = element_text(hjust = 0.5))
a3+scale_color_brewer(palette = "Set1")
dev.off()

load("SS_high_Naive.Rdata")
library(ggplot2)
CP_high_Naive <- unlist(CovProb)
AL_high_Naive <- unlist(AveLeng)

load("SS_high_Sweight.Rdata")
library(ggplot2)
CP_high_Sweight <- unlist(CovProb)
AL_high_Sweight <- unlist(AveLeng)

PlotData<- data.frame(CP=c(CP_high_Naive,CP_high_Sweight),Methods=c(rep("CP",100),rep("AOP",100)))
PlotData$Methods <- factor(PlotData$Methods,levels=c('CP','AOP'),ordered = TRUE)

setEPS()
postscript("HS.eps")
a4=ggplot(PlotData,aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+geom_hline(yintercept = 0.9,linetype="dashed",size=0.5)+theme(legend.position="None")
a4=a4+ggtitle("High dim, Single stage")+theme(plot.title = element_text(hjust = 0.5))
a4+scale_color_brewer(palette = "Set1")
dev.off()



