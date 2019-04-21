require(dplyr)
require(reshape)
require(ggplot2)

db <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/db_spXsite.csv",row.names=1) 
head(db)
rowSums(db)
Habitat <- c(rep("1.Banana",3),rep("2.Agroforestry",3),rep("3.Dist.Secondary",3),rep("4. Cleared regen.",3),rep("5.Mixed",3),rep("6. Min Dist.",3))
db.a <- as.data.frame(cbind(rownames(db),rowSums(db),Habitat))
names(db.a) <- c("PlotID","Abund","Habitat")
db.a$Abund <- as.numeric(as.character(db.a$Abund))
mean.ab <- tapply(db.a$Abund,list(db.a$Habitat),mean)
sd.ab <- tapply(db.a$Abund,list(db.a$Habitat),sd)

mean.long<-as.data.frame(melt(mean.ab,id.vars="Habitat"))
sd.long <- as.data.frame(melt(sd.ab,id.vars="Habitat"))

limits <- aes(ymax = mean.long[,2] + sd.long[,2], ymin=mean.long[,2] - sd.long[,2])
positions <- c("Banana", "Agroforestry","Secondary","CCR","PCR","SLR")

habs <- c("1.Banana","2.Agroforestry","3.Dist. secondary",	"4.Cleared regen.",	"5.Mixed use",	"6.Min. disturbed")
db.ab <- as.data.frame(cbind(habs,abun))
db.ab[,2] <- as.numeric(as.character(db.ab[,2]))
positions <- habs

require(scales)
require(ggplot2)
#facet_wrap(facets=~group,nrow=1,scale="free_y")+scale_y_continuous(breaks= pretty_breaks())
par(mar=c(5,4,4,2))
ggplot(mean.long,aes(x=indices,y=value,fill=factor(indices)))+
  geom_bar(stat="identity",position=position_dodge(.9))+
  geom_errorbar(limits,position=position_dodge(.9),width=0.25)+
  labs(title = "Mean dung beetle abundance in plots of each habitat type")+ 
  xlab("Habitat")+ylab("Mean abundance +/- SD")+
  #scale_x_discrete(limits = positions,labels=c("Banana","Agroforestry","Dist.Secondary","Cleared regen.","Mixed use","Min. disturbed"))+
  theme(legend.position="none")+
  theme(
    plot.background = element_blank()#to remove background plot
    ,panel.grid.major = element_blank()#to remove grid lines
    ,panel.grid.minor = element_blank()#to remove grid lines
    ,panel.border = element_blank()#to remove border line
  ) + scale_fill_manual(values=c("#FFFF33","#CCFF33","#99CC33","#66CC00","#009900","#006600"))



