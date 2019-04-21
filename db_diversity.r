# calculating dung beetle diversity

# libraries and sources ----
rm(list=ls()) ##Clear memory
setwd("C:/Data/PhD/Analysis/Dungbeetles")

source("C://Data/PhD/Analysis/Butterflies/general/diversity.r")
source("C://Data/PhD/Analysis/Diversity/diversityplots.r")
require(ggplot2)
require(reshape2)
require(BiodiversityR)
require(vegan)
require(plotrix)

## observed hills numbers diversity ----
db <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/db_spXsite.csv",row.names=1) 
db.mat <- t(as.matrix(db))
head(db.mat)

#checking for singletons and doubletons
length(which(rowSums(db.mat)=="1")) # overall species only found once or twice
length(which(rowSums(db.mat)=="2"))
length(rowSums((db.mat)))
length(which(db.mat=="1")) #in each site
length(which(db.mat=="2"))
length(which(db.mat>"2"))

p1 <- db.mat/sum(db.mat) #turned counts into proportions
#qs1 <- c(0,0.5,1,2,3,Inf)
qs1 <- c(0,seq(from = 0.5, to = 2,by=0.1),Inf)# for calculating
#qs2 <- c(0,0.5,1,2,3,6) # for plotting
qs2 <- c(1,seq(from = 2, to = 8,by=0.4),16) # for plotting

alpha<- subcommunity.alpha.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=alpha)
title <- "Alpha diversity without similarity 
Dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/plot_diversity_new_aug16/alpha_nosim_plots.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

# ## ggplot option (gradient cols)
#  ggplot(data=divs.p,
#          aes(x=plotQs, y=value,colour=Habitat, group = plotID )) +
#     geom_point()+ geom_line(size=0.8)+
#     labs(title = "Alpha diversity across disturbance gradient")+ 
#     xlab("Diversity Index (q)")+ylab("Diversity") +
#     scale_x_discrete(labels=c("0","1","2","3","","","Inf"))+# What names you want on x axis
#     theme(legend.position="right",panel.grid=element_blank(), 
#     panel.background=element_rect(fill = "transparent",colour = NA),
#     panel.border=element_blank()) +
#     theme_bw() + scale_color_manual(values=c('darkred','firebrick2','darksalmon','darkslategray3','dodgerblue2','navyblue')) +
#     theme(panel.border=element_blank()) 
#  
#  # base plot (gradient and axis break)
#  #png(file="C://Data/PhD/Outputs/Dungbeetles/Diversity/DB_Alpha_baseplot.png",width=10,height=7.5,units="in",res=180)  
#  
#   cf1 <- c('darkred','firebrick2','darksalmon','darkslategray3','dodgerblue2','navyblue')
#  cf <- c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue')
#  rows <- length(alpha[,1])
#  plot(rep(0,length(qs1))~qs1,type="l",col="white",ylim=c(min(alpha),max(alpha)),xlim=c(0,6),bty='L',xlab=c("q"),
#       ylab=c("Effective number of species"),cex.lab=1.4,cex.axis=1.2,xaxt="n")
#    axis(1, at=c(0,1,2,3,6), labels=c("0","1","2","3",expression(infinity)), las=0)
#    axis.break(axis=1,breakpos=4.5,pos=NA,bgcol="white",breakcol="black",
#               style="zigzag",brw=0.1)
#    for(rw in 1:18){
#      lines(alpha[rw,]~qs2,col=cf[rw],lwd=3)
#      points(alpha[rw,]~qs2,col=cf[rw],pch=16,cex=0.7)
#    }
#    legend("topright",legend= c("1.Banana","2.Agroforestry","3.Disturbed secondary","4.Cleared regenerating","5.Mixed history","6.Minimally disturbed"),
#           cex=1.3,pch=21,pt.bg=(col=cf1),pt.cex=2,bty="n") #Added key later (after submission) to explain subgroups.
#   #dev.off()
#  require(graphics)
#    cf1 <- c('darkred','firebrick2','darksalmon','darkslategray3','dodgerblue2','navyblue')
#    cf <- c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue')
#   

# alpha div plot ----
#    png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rdiversity/alphab_gradient_inf.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
  # tiff("C://Data/PhD/Outputs/Dungbeetles/db chapter figures/alphab_gradient_inf.tiff",width=190,height=150,units="mm",res=1000, pointsize=12)  
  #  gradplot(measure=alpha,y=c("Effective number of species"),colgrad=cf,legcol=cf1)
  #  dev.off()
   
   tiff(file="C://Data/PhD/Outputs/Dungbeetles/Biotropica figures/Fig1_alphaprofile.tiff",width=190,height=110,units="mm",res=1000, pointsize=9)  
   par(mar=c(4.5,4,1,1))
   gradplot(measure=alpha,y=c("Effective number of species"),colgrad=cf,legcol=cf1)
   dev.off()
   
beta_b<- subcommunity.beta.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=beta_b)
title <- "beta bar diversity without similarity 
Dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/plot_diversity_new_aug16/betabar_nosim_plots.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()
png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rdiversity/betabar_gradient.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
gradplot(measure=beta_b,y=c("Effective number of distinct subcommunities"),colgrad=cf,legcol=cf1,legpos="topleft")
dev.off()

beta<- subcommunity.beta(populations=p1,qs=qs1)
divs.p <- prep.div(div=beta)
title <- "beta diversity without similarity 
Dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/plot_diversity_new_aug16/beta_nosim_plots.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()
png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rdiversity/beta_gradient.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
gradplot(measure=beta,y=c("Distinctiveness of subcommunity"),legpos="topleft")
dev.off()

rho<- subcommunity.rho(populations=p1,qs=qs1)
divs.p <- prep.div(div=rho)
title <- "rho diversity without similarity 
Dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/plot_diversity_new_aug16/rho_nosim_plots.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()
png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rdiversity/rho_gradient.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
gradplot(measure=rho, y=c("redundancy of subcommunity"))
dev.off()

rho_b<- subcommunity.rho.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=rho_b)
title <- "rho bar diversity without similarity 
Dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/plot_diversity_new_aug16/rhobar_nosim_plots.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()
png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rdiversity/rhobar_gradient.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
gradplot(measure=rho_b,y=c("Representativeness of subcommunity"))
dev.off()

gamma<- subcommunity.gamma(populations=p1,qs=qs1)
divs.p <- prep.div(div=gamma)
title <- "gamma diversity without similarity 
Dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/plot_diversity_new_aug16/gamma_nosim_plots.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()
png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rdiversity/gamma_gradient.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
gradplot(measure=gamma,y=c("contribution per individual toward metacommunity diversity"))
dev.off()



#export diversity values
# 
# write.csv(alpha,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/alpha.csv")
# write.csv(beta,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/beta.csv")
# write.csv(beta_b,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/beta_b.csv")
# write.csv(rho,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/rho.csv")
# write.csv(rho_b,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/rho_b.csv")
# write.csv(gamma,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/gamma.csv")

###/////////////////////
# mean hill diversities per habitat ----

sab1Plots<- subcommunity.alpha.bar(populations=p1,qs=qs1)
###png(filename="sab1Plots.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
avs <- av.divs.f(sab1Plots,q=qs2)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(rep(0,length(qs2))~qs2,type="l",col="white",ylim=c(min(avs),max(avs)),bty='L',xlab=c("q"),xaxt="n",
     ylab=c("Alpha diversity"),main=c("Subcommunity Alpha bar"),
     sub=c("(not accounting for taxonomic similarity)"))
plotplots(measure=avs,rows=c(1:6),leg.pos="topright")
###dev.off()

beta<- subcommunity.beta.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=beta)
title <- "Beta diversity without similarity 
Dung beetles, all plots across disturbance gradient"
plotdiv(divs.long= divs.p,t1=title)
avs <- av.divs.f(beta,q=qs2)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(rep(0,length(qs2))~qs2,type="l",col="white",ylim=c(min(avs),max(avs)),bty='L',xlab=c("q"),xaxt="n",
     ylab=c("Beta diversity"),main=c("Subcommunity Beta bar"),
     sub=c("(not accounting for taxonomic similarity)"))
plotplots(measure=avs,rows=c(1:6),leg.pos="topleft")



rho<- subcommunity.rho.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=rho)
title <- "Rho diversity without similarity 
Dung beetles, all plots across disturbance gradient"
plotdiv(divs.long= divs.p,t1=title)
avs <- av.divs.f(rho,q=qs2)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(rep(0,length(qs2))~qs2,type="l",col="white",ylim=c(min(avs),max(avs)),bty='L',xlab=c("q"),xaxt="n",
     ylab=c("Rho diversity"),main=c("Subcommunity Rho bar"),
     sub=c("(not accounting for taxonomic similarity)"))
plotplots(measure=avs,rows=c(1:6),leg.pos="topright")

gamma<- subcommunity.gamma(populations=p1,qs=qs1)
divs.p <- prep.div(div=gamma)
title <- "Gamma diversity without similarity 
Dung beetles, all plots across disturbance gradient"
plotdiv(divs.long= divs.p,t1=title)
avs <- av.divs.f(gamma,q=qs2)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(rep(0,length(qs2))~qs2,type="l",col="white",ylim=c(min(avs),max(avs)),bty='L',xlab=c("q"),xaxt="n",
     ylab=c("gamma diversity"),main=c("Subcommunity gamma"),
     sub=c("(not accounting for taxonomic similarity)"))
plotplots(measure=avs,rows=c(1:6),leg.pos="topright")


#Classic divestsity measures----
richness<- diversityresult(x=t(db.mat), index='richness' ,method='s', 
                           sortit=F, digits=2)
as.data.frame(richness)

exShannon<- exp(diversity(t(db.mat), index = "shannon"))
as.data.frame(exShannon)

InvSimpson<- diversity(t(db.mat), index = "invsimpson")
as.data.frame(InvSimpson)

recBerger <- diversityresult(x=t(db.mat), index='Berger' ,method='s', 
                             sortit=F, digits=2)
as.data.frame(recBerger)

plotID<-rownames(richness)
classicdivs <- cbind(plotID,richness,exShannon,InvSimpson,recBerger)

Disturbance <- c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(5,3),rep(6,3))
Habitat <- c(rep("1.Banana",3),rep("2.Agroforestry",3),rep("3.Dist.Secondary",3),rep("4. Cleared regen.",3),rep("5.Mixed",3),rep("6. Min Dist.",3))

require(ggplot2)
require(reshape2)
# reshape data to long format
divs.long<-melt(classicdivs,id.vars="plotID")
divs.longH <- cbind(divs.long,Habitat=rep(Habitat,4))
divs.longH <- cbind(divs.long,Disturbance=rep(Disturbance,4))
#divs.long <- with(divs.long,divs.long[order(plotID), ])
r.long <- divs.longH[divs.longH$variable=="richness",]
Sh.long <- divs.longH[divs.longH$variable=="exShannon",]
Si.long <- divs.longH[divs.longH$variable=="InvSimpson",]
Bp.long <- divs.longH[divs.longH$variable=="Berger",]

positions <- c(plotID)

gradcols <- c("yellow","yellow","yellow","yellow3","yellow3","yellow3",
              "yellowgreen","yellowgreen","yellowgreen","springgreen3","springgreen3","springgreen3",
              "green3","green3","green3","forestgreen","forestgreen","forestgreen")

###png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/db_classic_richness.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
ggplot(r.long,aes(x=plotID,y=value,fill=factor(plotID)))+ # indices and values are the auto column names when you melt the data
  geom_bar(stat="identity",position=position_dodge(.9))+
  labs(title = "Dung beetle richness along a disturbance gradient")+ 
  xlab("Plot")+ylab("Richness") +
  scale_x_discrete(limits = positions,labels=c(plotID))+ # What names you want on x axis
  theme(legend.position="none")+
  theme_bw() + scale_fill_manual(values=gradcols) # colour gradient light green to dark
###dev.off()

###png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/db_classic_shannon.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
ggplot(Sh.long,aes(x=plotID,y=value,fill=factor(plotID)))+ # indices and values are the auto column names when you melt the data
  geom_bar(stat="identity",position=position_dodge(.9))+
  labs(title = "Dung beetle Exp Shannon diversity along a disturbance gradient")+ 
  xlab("Plot")+ylab("Exp Shannon diversity") +
  scale_x_discrete(limits = positions,labels=c(plotID))+ # What names you want on x axis
  theme(legend.position="none")+
  theme_bw() + scale_fill_manual(values=gradcols) # colour gradient light green to dark
###dev.off()

###png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/db_classic_simpson.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
ggplot(Si.long,aes(x=plotID,y=value,fill=factor(plotID)))+ # indices and values are the auto column names when you melt the data
  geom_bar(stat="identity",position=position_dodge(.9))+
  labs(title = "Dung beetle Inv Simpson diversity along a disturbance gradient")+ 
  xlab("Plot")+ylab("Inv Simpson diversity") +
  scale_x_discrete(limits = positions,labels=c(plotID))+ # What names you want on x axis
  theme(legend.position="none")+
  theme_bw() + scale_fill_manual(values=gradcols) # colour gradient light green to dark
###dev.off()

###png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/db_classic_bergerparker.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
ggplot(Bp.long,aes(x=plotID,y=value,fill=factor(plotID)))+ # indices and values are the auto column names when you melt the data
  geom_bar(stat="identity",position=position_dodge(.9))+
  labs(title = "Dung beetle Berger Parker diversity along a disturbance gradient")+ 
  xlab("Plot")+ylab("Berger Parker diversity") +
  scale_x_discrete(limits = positions,labels=c(plotID))+ # What names you want on x axis
  theme(legend.position="none")+
  theme_bw() + scale_fill_manual(values=gradcols) # colour gradient light green to dark
###dev.off()


# MEan classicdiversity per habitat -----

## Richness
mean.r <- tapply(r.long$value,list(r.long$Habitat),mean)
sd.r <- tapply(r.long$value,list(r.long$Habitat),sd)

mean.long<-as.data.frame(melt(mean.r,id.vars="Habitat"))
sd.long <- as.data.frame(melt(sd.r,id.vars="Habitat"))
limits <- aes(ymax = mean.long[,2] + sd.long[,2], ymin=mean.long[,2] - sd.long[,2])
positions <- c(plotID)
gradcols <- c("yellow","yellow3","yellowgreen","springgreen3","green3","forestgreen")

ggplot(mean.long,aes(x=Var1,y=value,fill=factor(Var1)))+
  geom_bar(stat="identity",position=position_dodge(.9))+
  geom_errorbar(limits,position=position_dodge(.9),width=0.25)+
  labs(title = "Mean dung beetle richness in plots of each habitat type")+ 
  xlab("Habitat")+ylab("Mean richness +/- SD")+
  #scale_x_discrete(limits = positions,labels=c("Banana","Agroforestry","Dist.Secondary","Cleared regen.","Mixed use","Min. disturbed"))+
  theme(legend.position="none")+
  theme(
    plot.background = element_blank()#to remove background plot
    ,panel.grid.major = element_blank()#to remove grid lines
    ,panel.grid.minor = element_blank()#to remove grid lines
    ,panel.border = element_blank()#to remove border line
  ) + scale_fill_manual(values=c("#FFFF33","#CCFF33","#99CC33","#66CC00","#009900","#006600"))

## Shannon
mean.sh <- tapply(Sh.long$value,list(Sh.long$Habitat),mean)
sd.sh <- tapply(Sh.long$value,list(Sh.long$Habitat),sd)

mean.long<-as.data.frame(melt(mean.sh,id.vars="Habitat"))
sd.long <- as.data.frame(melt(sd.sh,id.vars="Habitat"))
limits <- aes(ymax = mean.long[,2] + sd.long[,2], ymin=mean.long[,2] - sd.long[,2])
positions <- c(plotID)
gradcols <- c("yellow","yellow3","yellowgreen","springgreen3","green3","forestgreen")

ggplot(mean.long,aes(x=Var1,y=value,fill=factor(Var1)))+
  geom_bar(stat="identity",position=position_dodge(.9))+
  geom_errorbar(limits,position=position_dodge(.9),width=0.25)+
  labs(title = "Mean dung beetle shannon diversity  in plots of each habitat type")+ 
  xlab("Habitat")+ylab("Mean shannon diversity +/- SD")+
  #scale_x_discrete(limits = positions,labels=c("Banana","Agroforestry","Dist.Secondary","Cleared regen.","Mixed use","Min. disturbed"))+
  theme(legend.position="none")+
  theme(
    plot.background = element_blank()#to remove background plot
    ,panel.grid.major = element_blank()#to remove grid lines
    ,panel.grid.minor = element_blank()#to remove grid lines
    ,panel.border = element_blank()#to remove border line
  ) + scale_fill_manual(values=c("#FFFF33","#CCFF33","#99CC33","#66CC00","#009900","#006600"))



###//////////////////////////
##//////////////////////////
# Dung beetle function data -----


fun <- as.data.frame(read.csv("C://Data/PhD/Processed_data/Dungbeetles/Dungbeetlefunctions_1sthalf.csv")) 
head(fun)

# total proportion of seeds dispersed
#fun <- fun[-1,]
tot.rem<- fun$Countremain_Small+fun$Countremain_Med+fun$Countremain_Large
tot.Disp <- 80-tot.rem
fun2 <- cbind(fun,tot.Disp)
head(fun2)
mean.dispersal <- tapply(fun2$tot.Disp,list(fun2$Habitat),mean)
sd.dispersal <- tapply(fun2$tot.Disp,list(fun2$Habitat),mean)
barplot(mean.dispersal,main="number of beads dispersed")

mean.beadw<- tapply(fun2$Weight_.beads,list(fun2$Habitat),mean)
sd.dbeadw<- tapply(fun2$Weight_.beads,list(fun2$Habitat),mean)
barplot(mean.beadw,main="weight of beads remaining")

#Dung removal
mean.dung <- tapply(fun2$Dung_endweight,list(fun2$Habitat),mean)
sd.dung <- tapply(fun2$Dung_endweight,list(fun2$Habitat),mean)
barplot(mean.dung,main="weight or remaining dung")

# Soil excavation
mean.soil <- tapply(fun2$Excavated_soilweight,list(fun2$Habitat),mean)
sd.soil <- tapply(fun2$Excavated_soilweight,list(fun2$Habitat),mean)
barplot(mean.soil, main="weight of excavated soil")


#/////////////////////////////
## Plot line graphs of diversity ------- 
# with diversity index on x axis and diversity value on y, with lines for each plot
#/////////////////////////

# reshape data to long format
divslong<-melt(classicdivs,id.vars="plotID")
habitat <- c("1.Banana","1.Banana","1.Banana","2.Agroforestry","2.Agroforestry","2.Agroforestry","3.Secondary","3.Secondary","3.Secondary","4.Cleared Regen","4.Cleared Regen","4.Cleared Regen","5.Mixed","5.Mixed","5.Mixed","6.MinDist","6.MinDist","6.MinDist")
divs.long <- cbind(divslong,habitat)


##png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/db_classic_diversityprofile_plots.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
ggplot(data=divs.long,
       aes(x=variable, y=value,colour=habitat, group = plotID )) +
  geom_point()+ geom_line(size=0.8)+
  labs(title = "Dung beetle classic diversity profiles of each plot along a disturbance gradient")+ 
  xlab("Diversity Index (q)")+ylab("Diversity") +
  scale_x_discrete(labels=c("Richness (q=0)","Shannon exp (q=1)", "inv Simpsons (q=2)", "Berger Parker (q=Inf)"))+ # What names you want on x axis
  theme(legend.position="right")+
  theme_bw() + scale_color_manual(values=c("goldenrod1","red","magenta",
                                           "black","green","blue"))

##dev.off()


##/////////////////
#### Split dung beetles by functional groups -------------
#///////////////////
require(stringr)

db <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/db_spXsite.csv",row.names=1) 

traits <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/Dungbeetle_traits.csv")

traits <- traits[,-c(9,10,11)]
head(traits)
str(traits)

traits$Species <- str_replace_all(traits$Species,"[[:punct:]]"," ")
traits <- as.data.frame(traits[,1:4])

tdb <- t(db)
head(tdb)
Sp <- c(rownames(tdb))
Species <- str_replace_all(Sp,"[[:punct:]]"," ") # replace '.' with spaces so names are the same in both dfs
tdb <- as.data.frame(cbind(tdb,Species))
head(tdb)


DB_merge <- merge(tdb,traits,by="Species") #join count mat with functions

head(DB_merge)

rollers <- DB_merge[DB_merge$Functional_group=="Telecoprid",] # split into rollers and tunnellers (incl. dwellers)
head(rollers)
rollers <- rollers[,-c(20,21,22)]
str(rollers)
rollers[,2] <- as.numeric(as.character(rollers[,2]))
rollers[,3] <- as.numeric(as.character(rollers[,3]))
rollers[,4] <- as.numeric(as.character(rollers[,4]))
rollers[,5] <- as.numeric(as.character(rollers[,5]))
rollers[,6] <- as.numeric(as.character(rollers[,6]))
rollers[,7] <- as.numeric(as.character(rollers[,7]))
rollers[,8] <- as.numeric(as.character(rollers[,8]))
rollers[,9] <- as.numeric(as.character(rollers[,9]))
rollers[,10] <- as.numeric(as.character(rollers[,10]))
rollers[,11] <- as.numeric(as.character(rollers[,11]))
rollers[,12] <- as.numeric(as.character(rollers[,12]))
rollers[,13] <- as.numeric(as.character(rollers[,13]))
rollers[,14] <- as.numeric(as.character(rollers[,14]))
rollers[,15] <- as.numeric(as.character(rollers[,15]))
rollers[,16] <- as.numeric(as.character(rollers[,16]))
rollers[,17] <- as.numeric(as.character(rollers[,17]))
rollers[,18] <- as.numeric(as.character(rollers[,18]))
rollers[,19] <- as.numeric(as.character(rollers[,19]))
rolls <- rollers$Species
rollers <- as.matrix(rollers[,2:19])
rownames(rollers) <- rolls


tunnels <- DB_merge[DB_merge$Functional_group!="Telecoprid",]
head(tunnels)
tunnels <- tunnels[,-c(20,21,22)] # remove functions columns
tunnels[,2] <- as.numeric(as.character(tunnels[,2]))
tunnels[,3] <- as.numeric(as.character(tunnels[,3]))
tunnels[,4] <- as.numeric(as.character(tunnels[,4]))
tunnels[,5] <- as.numeric(as.character(tunnels[,5]))
tunnels[,6] <- as.numeric(as.character(tunnels[,6]))
tunnels[,7] <- as.numeric(as.character(tunnels[,7]))
tunnels[,8] <- as.numeric(as.character(tunnels[,8]))
tunnels[,9] <- as.numeric(as.character(tunnels[,9]))
tunnels[,10] <- as.numeric(as.character(tunnels[,10]))
tunnels[,11] <- as.numeric(as.character(tunnels[,11]))
tunnels[,12] <- as.numeric(as.character(tunnels[,12]))
tunnels[,13] <- as.numeric(as.character(tunnels[,13]))
tunnels[,14] <- as.numeric(as.character(tunnels[,14]))
tunnels[,15] <- as.numeric(as.character(tunnels[,15]))
tunnels[,16] <- as.numeric(as.character(tunnels[,16]))
tunnels[,17] <- as.numeric(as.character(tunnels[,17]))
tunnels[,18] <- as.numeric(as.character(tunnels[,18]))
tunnels[,19] <- as.numeric(as.character(tunnels[,19]))
tunns <- tunnels$Species
tunnels <- as.matrix(tunnels[,2:19])
rownames(tunnels) <- tunns


#write.csv(rollers,"C://Data/PhD/Processed_data/Dungbeetles/rollers-countmat.csv")
#write.csv(tunnels,"C://Data/PhD/Processed_data/Dungbeetles/tunnels-countmat.csv")

#SIZE 

small <- DB_merge[DB_merge$Size=="Small",] # split into small and large
head(small)
small <- small[,-c(20,21,22)]
str(small)
small[,2] <- as.numeric(as.character(small[,2]))
small[,3] <- as.numeric(as.character(small[,3]))
small[,4] <- as.numeric(as.character(small[,4]))
small[,5] <- as.numeric(as.character(small[,5]))
small[,6] <- as.numeric(as.character(small[,6]))
small[,7] <- as.numeric(as.character(small[,7]))
small[,8] <- as.numeric(as.character(small[,8]))
small[,9] <- as.numeric(as.character(small[,9]))
small[,10] <- as.numeric(as.character(small[,10]))
small[,11] <- as.numeric(as.character(small[,11]))
small[,12] <- as.numeric(as.character(small[,12]))
small[,13] <- as.numeric(as.character(small[,13]))
small[,14] <- as.numeric(as.character(small[,14]))
small[,15] <- as.numeric(as.character(small[,15]))
small[,16] <- as.numeric(as.character(small[,16]))
small[,17] <- as.numeric(as.character(small[,17]))
small[,18] <- as.numeric(as.character(small[,18]))
small[,19] <- as.numeric(as.character(small[,19]))
smallsp <- small$Species
small <- as.matrix(small[,2:19])
rownames(small) <- smallsp
head(small)

large <- DB_merge[DB_merge$Size=="Large",] # split into large and large
head(large)
large <- large[,-c(20,21,22)]
str(large)
large[,2] <- as.numeric(as.character(large[,2]))
large[,3] <- as.numeric(as.character(large[,3]))
large[,4] <- as.numeric(as.character(large[,4]))
large[,5] <- as.numeric(as.character(large[,5]))
large[,6] <- as.numeric(as.character(large[,6]))
large[,7] <- as.numeric(as.character(large[,7]))
large[,8] <- as.numeric(as.character(large[,8]))
large[,9] <- as.numeric(as.character(large[,9]))
large[,10] <- as.numeric(as.character(large[,10]))
large[,11] <- as.numeric(as.character(large[,11]))
large[,12] <- as.numeric(as.character(large[,12]))
large[,13] <- as.numeric(as.character(large[,13]))
large[,14] <- as.numeric(as.character(large[,14]))
large[,15] <- as.numeric(as.character(large[,15]))
large[,16] <- as.numeric(as.character(large[,16]))
large[,17] <- as.numeric(as.character(large[,17]))
large[,18] <- as.numeric(as.character(large[,18]))
large[,19] <- as.numeric(as.character(large[,19]))
largesp <- large$Species
large <- as.matrix(large[,2:19])
rownames(large) <- largesp
head(large)

#write.csv(small,"C://Data/PhD/Processed_data/Dungbeetles/small-countmat.csv")
#write.csv(large,"C://Data/PhD/Processed_data/Dungbeetles/large-countmat.csv")


##//////////
# calc. roller diversity Hills ----

db.mat <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/rollers-countmat.csv")
head(db.mat)

p1 <- db.mat/sum(db.mat) #turned counts into proportions
qs1 <- c(0,1,2,3,Inf) # for calculating
qs2 <- c(0,1,2,3,6) # for plotting

alpha<- subcommunity.alpha.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=alpha)
title <- "Alpha diversity without similarity 
Roller dung beetles all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/alpha_nosim_plots_rollers.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

beta_b<- subcommunity.beta.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=beta_b)
title <- "beta bar diversity without similarity 
Roller dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/betabar_nosim_plots_rollers.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

beta<- subcommunity.beta(populations=p1,qs=qs1)
divs.p <- prep.div(div=beta)
title <- "beta diversity without similarity 
Roller dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/beta_nosim_plots_rollers.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

rho<- subcommunity.rho(populations=p1,qs=qs1)
divs.p <- prep.div(div=rho)
title <- "rho diversity without similarity 
Roller dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/rho_nosim_plots_rollers.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

rho_b<- subcommunity.rho.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=rho_b)
title <- "rho bar diversity without similarity 
Roller dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/rhobar_nosim_plots_rollers.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

gamma<- subcommunity.gamma(populations=p1,qs=qs1)
divs.p <- prep.div(div=gamma)
title <- "gamma diversity without similarity 
Roller dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/gamma_nosim_plots_rollers.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()


# export diversity values
# 
# write.csv(alpha,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/alpha_rollers.csv")
# write.csv(beta,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/beta_rollers.csv")
# write.csv(beta_b,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/beta_b_rollers.csv")
# write.csv(rho,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/rho_rollers.csv")
# write.csv(rho_b,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/rho_b_rollers.csv")
# write.csv(gamma,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/gamma_rollers.csv")
# 
# 
# ###//////////////
# calc. tunneler diversity hills nos----

db.mat <- tunnels
head(db.mat)

p1 <- db.mat/sum(db.mat) #turned counts into proportions
qs1 <- c(0,1,2,3,Inf) # for calculating
qs2 <- c(0,1,2,3,6) # for plotting

alpha<- subcommunity.alpha.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=alpha)
title <- "Alpha diversity without similarity 
Tunneler dung beetles all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/alpha_nosim_plots_tunnels.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

beta_b<- subcommunity.beta.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=beta_b)
title <- "beta bar diversity without similarity 
Tunneler dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/betabar_nosim_plots_tunnels.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

beta<- subcommunity.beta(populations=p1,qs=qs1)
divs.p <- prep.div(div=beta)
title <- "beta diversity without similarity 
Tunneler dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/beta_nosim_plots_tunnels.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

rho<- subcommunity.rho(populations=p1,qs=qs1)
divs.p <- prep.div(div=rho)
title <- "rho diversity without similarity 
Tunneler dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/rho_nosim_plots_tunnels.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

rho_b<- subcommunity.rho.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=rho_b)
title <- "rho bar diversity without similarity 
Tunneler dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/rhobar_nosim_plots_tunnels.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

gamma<- subcommunity.gamma(populations=p1,qs=qs1)
divs.p <- prep.div(div=gamma)
title <- "gamma diversity without similarity 
Tunneler dung beetles, all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/gamma_nosim_plots_tunnels.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()


# export diversity values

# write.csv(alpha,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/alpha_tunnels.csv")
# write.csv(beta,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/beta_tunnels.csv")
# write.csv(beta_b,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/beta_b_tunnels.csv")
# write.csv(rho,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/rho_tunnels.csv")
# write.csv(rho_b,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/rho_b_tunnels.csv")
# write.csv(gamma,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/gamma_tunnels.csv")


#///////////////////
#//////////////////
#///////////////////////

##//////////
# SIZE alpha  diversity Hills ----

#SMALL
db.mat <- small#read.csv("C://Data/PhD/Processed_data/Dungbeetles/small-countmat.csv")
head(db.mat)

p1 <- db.mat/sum(db.mat) #turned counts into proportions
qs1 <- c(0,1,2,3,Inf) # for calculating
qs2 <- c(0,1,2,3,6) # for plotting

alpha<- subcommunity.alpha.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=alpha)
title <- "Alpha diversity without similarity 
Small dung beetles all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/alpha_nosim_plots_rollers.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

#write.csv(alpha,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/alpha_small.csv")

#LARGE
db.mat <- large#read.csv("C://Data/PhD/Processed_data/Dungbeetles/large-countmat.csv")
head(db.mat)

p1 <- db.mat/sum(db.mat) #turned counts into proportions
qs1 <- c(0,1,2,3,Inf) # for calculating
qs2 <- c(0,1,2,3,6) # for plotting

alpha<- subcommunity.alpha.bar(populations=p1,qs=qs1)
divs.p <- prep.div(div=alpha)
title <- "Alpha diversity without similarity 
Large dung beetles all plots across disturbance gradient"
#png(filename="C://Data/PhD/Outputs/Dungbeetles/Diversity/rollers_tunnelers/alpha_nosim_plots_rollers.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
plotdiv(divs.long= divs.p,t1=title)
#dev.off()

#write.csv(alpha,"C://Data/PhD/Processed_data/Dungbeetles/Diversity/alpha_large.csv")


#  iNExt - diversity estimates (whole group) ----

# install iNEXT package from CRAN
# install.packages("iNEXT")
# install.packages("httr")
# 
# ## install the latest version from github
# install.packages('devtools')
# library(devtools)
# install_github('JohnsonHsieh/iNEXT')
# 
# ##/////////////
#### iNEXT diversity estimates

library(iNEXT)
library(ggplot2)

db <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/db_spXsite.csv",row.names=1) 
db.mat <- t(as.matrix(db))
head(db.mat)

str(db.mat)

m <- c(50, 100, 200,300,400,500,600) # series of sample sizes for extrapolation (if size=NULL, just goes to doubel sample size, 
# or otherwise can specify a signle number. for q=0 only reliable up to double sample size, but for 
# q=1 and 2, can be much higher without problems.)
#c(0,1,2,3,Inf)
#calculate up to 600 individuals for q=1-3, but only double sample size for q=0. qINF just estimates at 1, so not useful
iN123 <- iNEXT(db.mat, q=c(0,0.5,1,2,Inf), datatype="abundance", size=NULL, endpoint=300, knots=80, se=TRUE, conf=0.95, nboot=50)

#iN0 <- iNEXT(db.mat, q=0, datatype="abundance", size=NULL, endpoint=300, knots=40, se=TRUE, conf=0.95, nboot=50)
# 
# asy.q123 <- as.data.frame(iN123$AsyEst)
# asy.q0 <- as.data.frame(iN0$AsyEst)

# for many sites, observed samples are already at 90-95% completeness,
# or even higher. Therefore, 95% completeness was chosen at the 
# point at which to compare diversity estimates across sites. 

#extract the dataframes for each site
estq123_BAA <- as.data.frame(iN123$iNextEst$`1.BA-A`)
estq123_BAB <- as.data.frame(iN123$iNextEst$`1.BA-B`)
estq123_BAC <- as.data.frame(iN123$iNextEst$`1.BA-C`)
estq123_AFA <- as.data.frame(iN123$iNextEst$`2.AF-A`)
estq123_AFB <- as.data.frame(iN123$iNextEst$`2.AF-B`)
estq123_AFC <- as.data.frame(iN123$iNextEst$`2.AF-C`)
estq123_SFA <- as.data.frame(iN123$iNextEst$`3.SF-A`)
estq123_SFB <- as.data.frame(iN123$iNextEst$`3.SF-B`)
estq123_SFC <- as.data.frame(iN123$iNextEst$`3.SF-C`)
estq123_CCRA <- as.data.frame(iN123$iNextEst$`4.CCR-A`)
estq123_CCRB <- as.data.frame(iN123$iNextEst$`4.CCR-B`)
estq123_CCRC <- as.data.frame(iN123$iNextEst$`4.CCR-C`)
estq123_MXDA <- as.data.frame(iN123$iNextEst$`5.MXD-A`)
estq123_MXDB <- as.data.frame(iN123$iNextEst$`5.MXD-B`)
estq123_MXDC <- as.data.frame(iN123$iNextEst$`5.MXD-C`)
estq123_MINA <- as.data.frame(iN123$iNextEst$`6.MinD-A`)
estq123_MINB <- as.data.frame(iN123$iNextEst$`6.MinD-B`)
estq123_MINC <- as.data.frame(iN123$iNextEst$`6.MinD-C`)

r <- which(estq123_BAA$m==300) #select the end rows for equal sample size estimates
BAA <- estq123_BAA[r,]
r <- which(estq123_BAB$m==300) #select the end rows for equal sample size estimates
BAB <- estq123_BAB[r,]
r <- which(estq123_BAC$m==300) #select the end rows for equal sample size estimates
BAC <- estq123_BAC[r,]
r <- which(estq123_AFA$m==300) #select the end rows for equal sample size estimates
AFA <- estq123_AFA[r,]
r <- which(estq123_AFB$m==300) #select the end rows for equal sample size estimates
AFB<- estq123_AFB[r,]
r <- which(estq123_AFC$m==300) #select the end rows for equal sample size estimates
AFC<- estq123_AFC[r,]
r <- which(estq123_SFA$m==300) #select the end rows for equal sample size estimates
SFA <- estq123_SFA[r,]
r <- which(estq123_SFB$m==300) #select the end rows for equal sample size estimates
SFB<- estq123_SFB[r,]
r <- which(estq123_SFC$m==300) #select the end rows for equal sample size estimates
SFC<- estq123_SFC[r,]
r <- which(estq123_CCRA$m==300) #select the end rows for equal sample size estimates
CCRA <- estq123_CCRA[r,]
r <- which(estq123_CCRB$m==300) #select the end rows for equal sample size estimates
CCRB <- estq123_CCRB[r,]
r <- which(estq123_CCRC$m==300) #select the end rows for equal sample size estimates
CCRC<- estq123_CCRC[r,]
r <- which(estq123_MXDA$m==300) #select the end rows for equal sample size estimates
MXDA <- estq123_MXDA[r,]
r <- which(estq123_MXDB$m==300) #select the end rows for equal sample size estimates
MXDB <- estq123_MXDB[r,]
r <- which(estq123_MXDC$m==300) #select the end rows for equal sample size estimates
MXDC<- estq123_MXDC[r,]
r <- which(estq123_MINA$m==300) #select the end rows for equal sample size estimates
MINA <- estq123_MINA[r,]
r <- which(estq123_MINB$m==300) #select the end rows for equal sample size estimates
MINB <- estq123_MINB[r,]
r <- which(estq123_MINC$m==300) #select the end rows for equal sample size estimates
MINC<- estq123_MINC[r,]

Estqs <- rbind(BAA,BAB,BAC,AFA,AFB,AFC,SFA,SFB,SFC,CCRA,CCRB,CCRC,MXDA,MXDB,MXDC,MINA,MINB,MINC)
Site <- c(rep("BA-A",4),rep("BA-B",4),rep("BA-C",4),rep("AF-A",4),rep("AF-B",4),rep("AF-C",4),rep("SF-A",4),rep("SF-B",4),rep("SF-C",4),rep("CCR-A",4),rep("CCR-B",4),rep("CCR-C",4),rep("MXD-A",4),rep("MXD-B",4),rep("MXD-C",4),rep("MIN-A",4),rep("MIN-B",4),rep("MIN-C",4))
Ests <- cbind(Estqs,Site)
Site <- c("BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")


Est0 <- Ests[Ests$order==0,4]
Est1 <- Ests[Ests$order==1,4]
Est2 <- Ests[Ests$order==2,4]
EstIn <- Ests[Ests$order==Inf,4]
Est_eqSS <- as.data.frame(cbind(Site=as.character(Site),Est0_ss=as.numeric(Est0),Est1_ss=as.numeric(Est1),Est2_ss=as.numeric(Est2),EstInf_ss=as.numeric(EstIn)))
# 
write.csv(Est_eqSS,"C://Data/PhD/Processed_data/Dungbeetles/db_inext_Ests_EqSS.csv")

# for q=0.5 only
Site <- c("BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")
Estqs <- rbind(BAA,BAB,BAC,AFA,AFB,AFC,SFA,SFB,SFC,CCRA,CCRB,CCRC,MXDA,MXDB,MXDC,MINA,MINB,MINC)
Ests <- cbind(Estqs,Site)
Est05 <- Ests[Ests$order==0.5,c(4,10)]
#write.csv(Est05,"C://Data/PhD/Processed_data/Dungbeetles/db_inext_Ests_EqSSq05.csv")

# Estimates for double sample size q0 ----
# estq0_BAA <- as.data.frame(iN0$iNextEst$`1.BA-A`)
# estq0_BAB <- as.data.frame(iN0$iNextEst$`1.BA-B`)
# estq0_BAC <- as.data.frame(iN0$iNextEst$`1.BA-C`)
# estq0_AFA <- as.data.frame(iN0$iNextEst$`2.AF-A`)
# estq0_AFB <- as.data.frame(iN0$iNextEst$`2.AF-B`)
# estq0_AFC <- as.data.frame(iN0$iNextEst$`2.AF-C`)
# estq0_SFA <- as.data.frame(iN0$iNextEst$`3.SF-A`)
# estq0_SFB <- as.data.frame(iN0$iNextEst$`3.SF-B`)
# estq0_SFC <- as.data.frame(iN0$iNextEst$`3.SF-C`)
# estq0_CCRA <- as.data.frame(iN0$iNextEst$`4.CCR-A`)
# estq0_CCRB <- as.data.frame(iN0$iNextEst$`4.CCR-B`)
# estq0_CCRC <- as.data.frame(iN0$iNextEst$`4.CCR-C`)
# estq0_MXDA <- as.data.frame(iN0$iNextEst$`5.MXD-A`)
# estq0_MXDB <- as.data.frame(iN0$iNextEst$`5.MXD-B`)
# estq0_MXDC <- as.data.frame(iN0$iNextEst$`5.MXD-C`)
# estq0_MINA <- as.data.frame(iN0$iNextEst$`6.MinD-A`)
# estq0_MINB <- as.data.frame(iN0$iNextEst$`6.MinD-B`)
# estq0_MINC <- as.data.frame(iN0$iNextEst$`6.MinD-C`)
# 

# Ests for 95% SC----
# #find the row where completeness is closest to 0.95
# com <- 0.95 # sampling completeness level
# 
# #q=1
# q1BAA <- estq123_BAA[estq123_BAA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1BAA$SC - com)) # which row has desired SC
# BAA <- q1BAA[r,] # select for q=1 which has desired SC
# q1BAB <- estq123_BAB[estq123_BAB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1BAB$SC - com)) # which row has desired SC
# BAB <- q1BAB[r,] # select for q=1 which has desired SC
# q1BAC <- estq123_BAC[estq123_BAC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1BAC$SC - com)) # which row has desired SC
# BAC <- q1BAC[r,] # select for q=1 which has desired SC
# q1AFA <- estq123_AFA[estq123_AFA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1AFA$SC - com)) # which row has desired SC
# AFA <- q1AFA[r,] # select for q=1 which has desired SC
# q1AFB <- estq123_AFB[estq123_AFB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1AFB$SC - com)) # which row has desired SC
# AFB <- q1AFB[r,] # select for q=1 which has desired SC
# q1AFC <- estq123_AFC[estq123_AFC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1AFC$SC - com)) # which row has desired SC
# AFC <- q1AFC[r,] # select for q=1 which has desired SC
# q1SFA <- estq123_SFA[estq123_SFA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1SFA$SC - com)) # which row has desired SC
# SFA <- q1SFA[r,] # select for q=1 which has desired SC
# q1SFB <- estq123_SFB[estq123_SFB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1SFB$SC - com)) # which row has desired SC
# SFB <- q1SFB[r,] # select for q=1 which has desired SC
# q1SFC <- estq123_SFC[estq123_SFC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1SFC$SC - com)) # which row has desired SC
# SFC <- q1SFC[r,] # select for q=1 which has desired SC
# q1CCRA <- estq123_CCRA[estq123_CCRA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1CCRA$SC - com)) # which row has desired SC
# CCRA <- q1CCRA[r,] # select for q=1 which has desired SC
# q1CCRB <- estq123_CCRB[estq123_CCRB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1CCRB$SC - com)) # which row has desired SC
# CCRB <- q1CCRB[r,] # select for q=1 which has desired SC
# q1CCRC <- estq123_CCRC[estq123_CCRC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1CCRC$SC - com)) # which row has desired SC
# CCRC <- q1CCRC[r,] # select for q=1 which has desired SC
# q1MXDA <- estq123_MXDA[estq123_MXDA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MXDA$SC - com)) # which row has desired SC
# MXDA <- q1MXDA[r,] # select for q=1 which has desired SC
# q1MXDB <- estq123_MXDB[estq123_MXDB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MXDB$SC - com)) # which row has desired SC
# MXDB <- q1MXDB[r,] # select for q=1 which has desired SC
# q1MXDC <- estq123_MXDC[estq123_MXDC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MXDC$SC - com)) # which row has desired SC
# MXDC <- q1MXDC[r,] # select for q=1 which has desired SC
# q1MINA <- estq123_MINA[estq123_MINA$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MINA$SC - com)) # which row has desired SC
# MINA <- q1MINA[r,] # select for q=1 which has desired SC
# q1MINB <- estq123_MINB[estq123_MINB$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MINB$SC - com)) # which row has desired SC
# MINB <- q1MINB[r,] # select for q=1 which has desired SC
# q1MINC <- estq123_MINC[estq123_MINC$order==1,] # select only rows for q=1
# r <- which.min(abs(q1MINC$SC - com)) # which row has desired SC
# MINC <- q1MINC[r,] # select for q=1 which has desired SC
# 
# q1est <- rbind(BAA,BAB,BAC,AFA,AFB,AFC,SFA,SFB,SFC,CCRA,CCRB,CCRC,MXDA,MXDB,MXDC,MINA,MINB,MINC)
# Site <- c("BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")
# Eq1 <- cbind(q1est,Site)
# 
# #q=2
# q2BAA <- estq123_BAA[estq123_BAA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2BAA$SC - com)) # which row has desired SC
# BAA <- q2BAA[r,] # select for q=2 which has desired SC
# q2BAB <- estq123_BAB[estq123_BAB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2BAB$SC - com)) # which row has desired SC
# BAB <- q2BAB[r,] # select for q=2 which has desired SC
# q2BAC <- estq123_BAC[estq123_BAC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2BAC$SC - com)) # which row has desired SC
# BAC <- q2BAC[r,] # select for q=2 which has desired SC
# q2AFA <- estq123_AFA[estq123_AFA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2AFA$SC - com)) # which row has desired SC
# AFA <- q2AFA[r,] # select for q=2 which has desired SC
# q2AFB <- estq123_AFB[estq123_AFB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2AFB$SC - com)) # which row has desired SC
# AFB <- q2AFB[r,] # select for q=2 which has desired SC
# q2AFC <- estq123_AFC[estq123_AFC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2AFC$SC - com)) # which row has desired SC
# AFC <- q2AFC[r,] # select for q=2 which has desired SC
# q2SFA <- estq123_SFA[estq123_SFA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2SFA$SC - com)) # which row has desired SC
# SFA <- q2SFA[r,] # select for q=2 which has desired SC
# q2SFB <- estq123_SFB[estq123_SFB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2SFB$SC - com)) # which row has desired SC
# SFB <- q2SFB[r,] # select for q=2 which has desired SC
# q2SFC <- estq123_SFC[estq123_SFC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2SFC$SC - com)) # which row has desired SC
# SFC <- q2SFC[r,] # select for q=2 which has desired SC
# q2CCRA <- estq123_CCRA[estq123_CCRA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2CCRA$SC - com)) # which row has desired SC
# CCRA <- q2CCRA[r,] # select for q=2 which has desired SC
# q2CCRB <- estq123_CCRB[estq123_CCRB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2CCRB$SC - com)) # which row has desired SC
# CCRB <- q2CCRB[r,] # select for q=2 which has desired SC
# q2CCRC <- estq123_CCRC[estq123_CCRC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2CCRC$SC - com)) # which row has desired SC
# CCRC <- q2CCRC[r,] # select for q=2 which has desired SC
# q2MXDA <- estq123_MXDA[estq123_MXDA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MXDA$SC - com)) # which row has desired SC
# MXDA <- q2MXDA[r,] # select for q=2 which has desired SC
# q2MXDB <- estq123_MXDB[estq123_MXDB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MXDB$SC - com)) # which row has desired SC
# MXDB <- q2MXDB[r,] # select for q=2 which has desired SC
# q2MXDC <- estq123_MXDC[estq123_MXDC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MXDC$SC - com)) # which row has desired SC
# MXDC <- q2MXDC[r,] # select for q=2 which has desired SC
# q2MINA <- estq123_MINA[estq123_MINA$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MINA$SC - com)) # which row has desired SC
# MINA <- q2MINA[r,] # select for q=2 which has desired SC
# q2MINB <- estq123_MINB[estq123_MINB$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MINB$SC - com)) # which row has desired SC
# MINB <- q2MINB[r,] # select for q=2 which has desired SC
# q2MINC <- estq123_MINC[estq123_MINC$order==2,] # select only rows for q=2
# r <- which.min(abs(q2MINC$SC - com)) # which row has desired SC
# MINC <- q2MINC[r,] # select for q=2 which has desired SC
# 
# q2est <- rbind(BAA,BAB,BAC,AFA,AFB,AFC,SFA,SFB,SFC,CCRA,CCRB,CCRC,MXDA,MXDB,MXDC,MINA,MINB,MINC)
# Eq2 <- cbind(q2est,Site)
# 
# #q=3
# q3BAA <- estq123_BAA[estq123_BAA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3BAA$SC - com)) # which row has desired SC
# BAA <- q3BAA[r,] # select for q=3 which has desired SC
# q3BAB <- estq123_BAB[estq123_BAB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3BAB$SC - com)) # which row has desired SC
# BAB <- q3BAB[r,] # select for q=3 which has desired SC
# q3BAC <- estq123_BAC[estq123_BAC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3BAC$SC - com)) # which row has desired SC
# BAC <- q3BAC[r,] # select for q=3 which has desired SC
# q3AFA <- estq123_AFA[estq123_AFA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3AFA$SC - com)) # which row has desired SC
# AFA <- q3AFA[r,] # select for q=3 which has desired SC
# q3AFB <- estq123_AFB[estq123_AFB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3AFB$SC - com)) # which row has desired SC
# AFB <- q3AFB[r,] # select for q=3 which has desired SC
# q3AFC <- estq123_AFC[estq123_AFC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3AFC$SC - com)) # which row has desired SC
# AFC <- q3AFC[r,] # select for q=3 which has desired SC
# q3SFA <- estq123_SFA[estq123_SFA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3SFA$SC - com)) # which row has desired SC
# SFA <- q3SFA[r,] # select for q=3 which has desired SC
# q3SFB <- estq123_SFB[estq123_SFB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3SFB$SC - com)) # which row has desired SC
# SFB <- q3SFB[r,] # select for q=3 which has desired SC
# q3SFC <- estq123_SFC[estq123_SFC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3SFC$SC - com)) # which row has desired SC
# SFC <- q3SFC[r,] # select for q=3 which has desired SC
# q3CCRA <- estq123_CCRA[estq123_CCRA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3CCRA$SC - com)) # which row has desired SC
# CCRA <- q3CCRA[r,] # select for q=3 which has desired SC
# q3CCRB <- estq123_CCRB[estq123_CCRB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3CCRB$SC - com)) # which row has desired SC
# CCRB <- q3CCRB[r,] # select for q=3 which has desired SC
# q3CCRC <- estq123_CCRC[estq123_CCRC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3CCRC$SC - com)) # which row has desired SC
# CCRC <- q3CCRC[r,] # select for q=3 which has desired SC
# q3MXDA <- estq123_MXDA[estq123_MXDA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MXDA$SC - com)) # which row has desired SC
# MXDA <- q3MXDA[r,] # select for q=3 which has desired SC
# q3MXDB <- estq123_MXDB[estq123_MXDB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MXDB$SC - com)) # which row has desired SC
# MXDB <- q3MXDB[r,] # select for q=3 which has desired SC
# q3MXDC <- estq123_MXDC[estq123_MXDC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MXDC$SC - com)) # which row has desired SC
# MXDC <- q3MXDC[r,] # select for q=3 which has desired SC
# q3MINA <- estq123_MINA[estq123_MINA$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MINA$SC - com)) # which row has desired SC
# MINA <- q3MINA[r,] # select for q=3 which has desired SC
# q3MINB <- estq123_MINB[estq123_MINB$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MINB$SC - com)) # which row has desired SC
# MINB <- q3MINB[r,] # select for q=3 which has desired SC
# q3MINC <- estq123_MINC[estq123_MINC$order==3,] # select only rows for q=3
# r <- which.min(abs(q3MINC$SC - com)) # which row has desired SC
# MINC <- q3MINC[r,] # select for q=3 which has desired SC
# 
# q3est <- rbind(BAA,BAB,BAC,AFA,AFB,AFC,SFA,SFB,SFC,CCRA,CCRB,CCRC,MXDA,MXDB,MXDC,MINA,MINB,MINC)
# Eq3 <- cbind(q3est,Site)
# 
# 
# #q=0
# r <- which.min(abs(estq0_BAA$SC - com)) # which row has desired SC
# BAA <- estq0_BAA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0BAB$SC - com)) # which row has desired SC
# BAB <- estq0_BAB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_BAC$SC - com)) # which row has desired SC
# BAC <- estq0_BAC[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_AFA$SC - com)) # which row has desired SC
# AFA <- estq0_AFA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_AFB$SC - com)) # which row has desired SC
# AFB <- estq0_AFB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_AFC$SC - com)) # which row has desired SC
# AFC <- estq0_AFC[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_SFA$SC - com)) # which row has desired SC
# SFA <- estq0_SFA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_SFB$SC - com)) # which row has desired SC
# SFB <- estq0_SFB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_SFC$SC - com)) # which row has desired SC
# SFC <- estq0_SFC[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_CCRA$SC - com)) # which row has desired SC
# CCRA <- estq0_CCRA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_CCRB$SC - com)) # which row has desired SC
# CCRB <- estq0_CCRB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_CCRC$SC - com)) # which row has desired SC
# CCRC <- estq0_CCRC[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MXDA$SC - com)) # which row has desired SC
# MXDA <- estq0_MXDA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MXDB$SC - com)) # which row has desired SC
# MXDB <- estq0_MXDB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MXDC$SC - com)) # which row has desired SC
# MXDC <- estq0_MXDC[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MINA$SC - com)) # which row has desired SC
# MINA <- estq0_MINA[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MINB$SC - com)) # which row has desired SC
# MINB <- estq0_MINB[r,] # select for q=0 which has desired SC
# r <- which.min(abs(estq0_MINC$SC - com)) # which row has desired SC
# MINC <- estq0_MINC[r,] # select for q=0 which has desired SC
# 
# q0est <- rbind(BAA,BAB,BAC,AFA,AFB,AFC,SFA,SFB,SFC,CCRA,CCRB,CCRC,MXDA,MXDB,MXDC,MINA,MINB,MINC)
# Eq0 <- cbind(q0est,Site)

# 95% CI
colnames(Eq0)[4] <- "e95q0"
colnames(Eq1)[4] <- "e95q1"
colnames(Eq2)[4] <- "e95q2"
colnames(Eq3)[4] <- "e95q3"

## Asymptotic diversity estimates ----
r <- which(asy.q123$Diversity=="Shannon diversity") #select Shannon asymtotic estimates
as.q1 <- asy.q123[r,c(3:7)] #extract shannon rows
colnames(as.q1)[1] <- "obs.q1"
colnames(as.q1)[2] <- "asEst.q1"

r <- which(asy.q123$Diversity=="Simpson diversity") #select Shannon asymtotic estimates
as.q2 <- asy.q123[r,c(3:7)] #extract shannon rows
colnames(as.q2)[1] <- "obs.q2"
colnames(as.q2)[2] <- "asEst.q2"

## export diversity estimates ----

#Est.qs <- cbind(Site=Eq0$Site,Eq0[,c(1,2,4,5,6,7)],Eq1[,c(1,2,4,5,6,7)],Eq2[,c(1,2,4,5,6,7)],Eq3[,c(1,2,4,5,6,7)],as.q1,as.q2)

#write.csv(Est.qs,"C://Data/PhD/Processed_data/Dungbeetles/db_estqs_inext.csv")
write.csv(Est_eqSS,"C://Data/PhD/Processed_data/Dungbeetles/db_inext_Ests_EqSS.csv")

###///////////
# Sample size diversity estimate curves
###/////////
# Sample-size-based R/E curves, separating by "site""
#png(file="C://Data/PhD/Outputs/Dungbeetles/Diversity/iNEXT/DB_sample_estDiv_site.png",width=40,height=7.5,units="in",res=180)  
ggiNEXT(iN, type=1, facet.var="site")
#dev.off()

# Sample-size-based R/E curves, separating by "order"
gs <- ggiNEXT(iN123, type=1, facet.var="order")
#png(file="C://Data/PhD/Outputs/Dungbeetles/Diversity/iNEXT/DB_sample_estDiv_q.png",width=30,height=20,units="in",res=180)  
gs + scale_color_manual(values=c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue')) +
  scale_fill_manual(values=c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue'))+
  scale_shape_manual(values=c(rep(20,18))) 

#dev.off()


###//////
# Sample completeness 
###
# Sample completeness curves, separating by "site""
#png(file="C://Data/PhD/Outputs/Dungbeetles/Diversity/iNEXT/DB_samplecompletness300_site.png",width=60,height=5,units="in",res=180)  
ggiNEXT(iN0, type=2, facet.var="none")
#dev.off()

# Sample completeness curves, separating by "order"
gc <- ggiNEXT(iN0, type=2, facet.var="none")
tiff("C://Data/PhD/Outputs/Dungbeetles/db chapter figures/DB_samplecompletness_q0_nobg.tiff",width=190,height=130,units="mm",res=1000, pointsize=10)  
#png(file="C://Data/PhD/Outputs/Dungbeetles/Diversity/iNEXT/DB_samplecompletness_q0_nobg.png",width=15,height=10,units="in",res=400)  
gc + scale_color_manual(values=c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue')) +
  scale_fill_manual(values=c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue'))+
  scale_shape_manual(values=c(rep(20,18)))+
  theme_bw(base_size = 18) +
    guides(shape=FALSE,lty = guide_legend(order = 2))+
  theme(legend.position=c(0.6,0.3),legend.direction = "horizontal",
        legend.title=element_blank(),legend.text=element_text(size=9),
        axis.text=element_text(size=9,colour = "black"),axis.title=element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
dev.off()
###//////////
# Coverage based diversity estimate curves
####
# Coverage-based diversity estimate curves, separating by "site""
#png(file="C://Data/PhD/Outputs/Dungbeetles/Diversity/iNEXT/DB_cover_estDiv_site.png",width=60,height=10,units="in",res=180)  
ggiNEXT(iN, type=3, facet.var="site")
#dev.off()

# Coverage-based diversity estimate curves, separating by "order"
gcd <- ggiNEXT(iN123, type=3, facet.var="order")
#png(file="C://Data/PhD/Outputs/Dungbeetles/Diversity/iNEXT/DB_cover_estDiv_q.png",width=20,height=10,units="in",res=180)  
gcd + scale_color_manual(values=c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue')) +
          scale_fill_manual(values=c('darkred','darkred','darkred','firebrick2','firebrick2','firebrick2','darksalmon','darksalmon','darksalmon','darkslategray3','darkslategray3','darkslategray3','dodgerblue2','dodgerblue2','dodgerblue2','navyblue','navyblue','navyblue'))+
          scale_shape_manual(values=c(rep(20,18))) 
         
#dev.off()

## ///////////////
## Roller iNext ests ----



roll.mat <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/rollers-countmat.csv",row.names = 1)
head(roll.mat)
str(roll.mat)
colSums(roll.mat)
edit(roll.mat)
#calculate up to 600 individuals for q=1-3, but only double sample size for q=0. qINF just estimates at 1, so not useful
iN123 <- iNEXT(roll.mat, q=c(1,2,3), datatype="abundance", size=NULL, endpoint=600, knots=80, se=TRUE, conf=0.95, nboot=50)

iN0 <- iNEXT(roll.mat, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95, nboot=50)

asy.q123 <- as.data.frame(iN123$AsyEst)
asy.q0 <- as.data.frame(iN0$AsyEst)

##########################

#iNEXT resampling plots -----
inr0 <- read.csv("C://Data/PhD/Outputs/Dungbeetles/DBq0ests_rsampled.csv")
inr1 <- read.csv("C://Data/PhD/Outputs/Dungbeetles/DBq1ests_rsampled.csv")
inr2 <- read.csv("C://Data/PhD/Outputs/Dungbeetles/DBq2ests_rsampled.csv")
inrI <- read.csv("C://Data/PhD/Outputs/Dungbeetles/DBqInfests_rsampled.csv")

tiff(file="C://Data/PhD/Outputs/Dungbeetles/Biotropica figures/Fig2_alphaests_inextresampling.tiff",width=190,height=70,units="mm",res=1000, pointsize=12)  
par(mfrow=c(1,3))
par(mar=c(5.1,5.1,4.1,2.1))
plot(inr0$meandiffs~inr0$rankdiff,1,pch=21,cex=1,cex.axis=1,cex.lab=1.2,col='black',bty="n",ylab="Diversity estimates:
     Less disturbed > More disturbed",xlab="Difference in disturbance rank",ylim=c(0,1),xlim=c(1,5))
abline(lm(inr0$meandiffs~inr0$rankdiff))
mtext(side = 3, line = 0,adj=0,"a",font=2,cex=1.2)
plot(inr1$meandiffs~inr1$rankdiff,1,pch=21,cex=1,cex.axis=1,cex.lab=1.2,col='black',bty="n",ylab="Diversity estimates:
     Less disturbed > More disturbed",xlab="Difference in disturbance rank",ylim=c(0,1),xlim=c(1,5))
abline(lm(inr1$meandiffs~inr1$rankdiff))
mtext(side = 3, line = 0,adj=0,"b",font=2,cex=1.2)
plot(inr2$meandiffs~inr2$rankdiff,1,pch=21,cex=1,cex.axis=1,cex.lab=1.2,col='black',bty="n",ylab="Diversity estimates:
     Less disturbed > More disturbed",xlab="Difference in disturbance rank",ylim=c(0,1),xlim=c(1,5))
abline(lm(inr2$meandiffs~inr2$rankdiff))
mtext(side = 3, line = 0,adj=0,"c",font=2,cex=1.2)
dev.off()
