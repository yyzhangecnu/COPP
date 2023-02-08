load("SS_low_AOPM.Rdata")
CP_ran_M1_COPP <- CovProb_ran_M1 
CP_ran_M0_COPP <- CovProb_ran_M0 
CP_det_M1_COPP <- CovProb_det_M1 
CP_det_M0_COPP <- CovProb_det_M0 

load("SS_low_DirectM.Rdata")
CP_ran_M1_Direct <- CovProb_ran_M1 
CP_ran_M0_Direct <- CovProb_ran_M0 
CP_det_M1_Direct <- CovProb_det_M1 
CP_det_M0_Direct <- CovProb_det_M0 


load("SS_low_Sampling.Rdata")
CP_ran_M1_Sample <- CovProb_ran_M1 
CP_ran_M0_Sample <- CovProb_ran_M0 
CP_det_M1_Sample <- CovProb_det_M1 
CP_det_M0_Sample <- CovProb_det_M0 


PlotData_ran_M1 <- data.frame(CP=c(CP_ran_M1_Direct,CP_ran_M1_Sample,CP_ran_M1_COPP),Methods=c(rep("DM",100),rep("SM",100),rep("COPP",100)))
PlotData_ran_M1$Methods <- factor(PlotData_ran_M1$Methods,levels=c('DM','SM','COPP'),ordered = TRUE)

PlotData_det_M1 <- data.frame(CP=c(CP_det_M1_Direct,CP_det_M1_Sample,CP_det_M1_COPP),Methods=c(rep("DM",100),rep("SM",100),rep("COPP",100)))
PlotData_det_M1$Methods <- factor(PlotData_det_M1$Methods,levels=c('DM','SM','COPP'),ordered = TRUE)

PlotData_ran_M0 <- data.frame(CP=c(CP_ran_M0_Direct,CP_ran_M0_Sample,CP_ran_M0_COPP),Methods=c(rep("DM",100),rep("SM",100),rep("COPP",100)))
PlotData_ran_M0$Methods <- factor(PlotData_ran_M0$Methods,levels=c('DM','SM','COPP'),ordered = TRUE)

PlotData_det_M0 <- data.frame(CP=c(CP_det_M0_Direct,CP_det_M0_Sample,CP_det_M0_COPP),Methods=c(rep("DM",100),rep("SM",100),rep("COPP",100)))
PlotData_det_M0$Methods <- factor(PlotData_det_M0$Methods,levels=c('DM','SM','COPP'),ordered = TRUE)


p1 <- ggplot(PlotData_ran_M1,aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+geom_hline(yintercept = 0.9,size=0.8)+theme(axis.title.y = element_text(size=13))+theme(legend.position="None")+ggtitle("Random/True")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+theme(axis.text.x = element_text(size=20))+scale_x_discrete(labels=function(x) str_wrap(x,width=1))
p2 <- ggplot(PlotData_det_M1,aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+geom_hline(yintercept = 0.9,size=0.8)+theme(axis.title.y = element_text(size=13))+theme(legend.position="None")+ggtitle("Deterministic/True")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=1))
p3 <- ggplot(PlotData_ran_M0,aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+geom_hline(yintercept = 0.9,size=0.8)+theme(axis.title.y = element_text(size=13))+theme(legend.position="None")+ggtitle("Random/False")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=1))
p4 <- ggplot(PlotData_det_M0,aes(x=Methods, y=CP, color=Methods))+geom_boxplot()+xlab("")+geom_hline(yintercept = 0.9,size=0.8)+theme(axis.title.y = element_text(size=13))+theme(legend.position="None")+ggtitle("Deterministic/False")+theme(plot.title = element_text(hjust = 0.5,size=13))+theme(axis.text = element_text(face="bold",size=10))+scale_x_discrete(labels=function(x) str_wrap(x,width=1))

save.image("figure1.Rdata")


