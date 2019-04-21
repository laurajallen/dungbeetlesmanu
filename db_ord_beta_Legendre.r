rm(list=ls()) ##Clear memory
setwd("C:/Data/PhD/Analysis/Dungbeetles")

# for ordinations
library(vegan)
library(ape)

# + for beta diversity, LCBD etc and plotting
library(adespatial)
require(vegan)  
require(adegraphics)
library(ade4)
require(RColorBrewer)

#Ordination

########################
# complee db dataset
dung  <- read.csv("C:/Data/PhD/Processed_data/Dungbeetles/db_spXsite.csv",row.names=1) #column 1 has row names
summary(dung) # check smmary makes sense
rownames(dung) # check site names are recognised

# Hellinger transformation
dung.hel = decostand(dung, "hel") # Hellinger transformation 
dung.DHell = dist(dung.hel) # Compute the Hellinger distance 
# Principal coordinate analysis.    
res = cmdscale(dung.DHell, 5, eig=TRUE) # Calculate PCoA for Hellinger transformed  spider data distance matrix, with 5 axes calculated and eigenvalues cqlculated
limits = apply(res$points, 2, range)  # identify limits for axes  
xlim = c(limits[1,1], limits[2,1]) + c(-0.2,0)    # set axes limits
ylim = c(limits[1,2], limits[2,2])    
x = res$points[,1]    # select first two axes of PCoA for plotting
y = res$points[,2]    
plot(x, y, xlim=xlim, ylim=ylim, asp=1, xlab="Axis 1", ylab="Axis 2") 

# add names of the objects to the graphs
names = rownames(dung)    
text(x, y, labels= names, pos=2, cex=1, offset=1) 

# PCoA can also be computed using the pcoa() function of the ape package. 
# The biplot.pcoa() function of that package produces nicer ordination plots than those produced above. 
dung.pcoa <- pcoa(dung.DHell) # PCoA of dung beetle hellinger distances
head(dung)
# top 10 species for beta div
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
dung6 <- (dung)[,c(sp6)]
biplot.pcoa(dung.pcoa,plot.axes = c(1,2),dir.axis1=-1, dir.axis2=1, rn=NULL,dung6,cex=0.6) #plot biplot of dung beetle PCoA, 
?biplot.pcoa




# Now dung in PCA, with hellinger tranformation

dung.transf = decostand(dung,"hel")
rda.out <- rda(dung.transf) # default scale option =F 
# the rda (redundancy analysis) function calculates the PCA and can also be used for canonical RDA
# scale = F means the data should be centred by columns but not standardised

summary(rda.out,scaling=1) # shows the two axes of the PCA for each site
# scaling = 1 preserves Euclidean distance among objects
# scaling = 2 preserves correlation between descriptors

#PCA w scaling 1
summary(rda.out, scaling=1) # Eigenvalues, species scores, site scores 
biplot(rda.out, scaling=1) # Function biplot.rda() of vegan is used  
biplot(rda.out, scaling=1,display=c("sites","species"))

# PCA w scaling 2
summary(rda.out, scaling=2) # Eigenvalues, species scores, site scores 
biplot(rda.out, scaling=2,display=c("sites","species")) # Function biplot.rda() of vegan is used 
rda.sp6 <- (summary(rda.out)$species)[sp6,]



#custom pca plot
dung.transf = decostand(dung,"hel")
rda.out <- rda(dung.transf) 
# pc1=0.24119 pc2=0.08143
scrs <- scores(rda.out,display = c("sites", "species"), scaling =1) #extract sepecies and site scores from rda summary
summary(rda.out)

#png(file="C://Data/PhD/Outputs/Dungbeetles/Legendre/PCA_community_comp.png",width=10,height=7.5,units="in",res=180)  
tiff(file="C://Data/PhD/Outputs/Dungbeetles/db chapter figures/PCA_community_comp-col.tiff",width=140,height=140,units="mm",res=1000,pointsize=12)  

xlim <- with(scrs, range((scrs$species[,1])/1.8, scrs$sites[,1]))
ylim <- with(scrs, range((scrs$species[,2])/1.8, scrs$sites[,2]))
par(mar=c(1,1,1,1))
plot.new()
plot.window(xlim = xlim, ylim = ylim, asp = 1)
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")

colvec <- c('#d73027','#d73027','#d73027','#fc8d59','#fc8d59','#fc8d59','#fee090','#fee090','#fee090','#e0f3f8','#e0f3f8','#e0f3f8','#91bfdb','#91bfdb','#91bfdb','#4575b4','#4575b4','#4575b4')
#colvec <- c('#b2182b','#b2182b','#b2182b','#ef8a62','#ef8a62','#ef8a62','#CC3366','#fddbc7','#fddbc7','#d1e5f0','#d1e5f0','#d1e5f0','#67a9cf','#67a9cf','#67a9cf','#2166ac','#2166ac','#2166ac')
#colvec <- c("orange","orange","orange","magenta","magenta","magenta","red2","red2","red2", "black","black","black","mediumblue","mediumblue","mediumblue","green4","green4","green4")
with(dung, points(scrs$sites, col="black", bg= colvec,
                      pch = 21,cex=4))

legend("bottomleft",bty="n",pch=21,pt.cex=2,cex=1,pt.bg=c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4'),legend=c("1.Banana","2.Agroforestry","3.Disturbed secondary","4.Cleared regenerating","5.Mixed history","6.Minimally disturbed"))
#sites <- c("1.BAA","1.BAB","1.BAC","2.AFA","2.AFB","2.AFC","3.SFA","3.SFB","3.SFC","4.CCRA","4.CCRB","4.CCRC","5.MXDA","5.MXDB","5.MXDC","6.MINA","6.MINB","6.MINC")
#text(scrs$sites,labels=sites,pos=4)
text(-0.01,0.34,"PC2",srt=90,font=2)
text(-0.52,-0.01,"PC1",font=2)
dev.off()

#bw friendly version
tiff(file="C://Data/PhD/Outputs/Dungbeetles/db chapter figures/PCA_community_comp-bw.tiff",width=190,height=190,units="mm",res=1000,pointsize=12)  

xlim <- with(scrs, range((scrs$species[,1])/1.8, scrs$sites[,1]))
ylim <- with(scrs, range((scrs$species[,2])/1.8, scrs$sites[,2]))
par(mar=c(1,1,1,1))
plot.new()
plot.window(xlim = xlim, ylim = ylim, asp = 1)
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")

symvec <- c(rep(0,3),rep(4,3),rep(2,3),rep(1,3),rep(3,3),rep(6,3))
colvec <- c('#d73027','#d73027','#d73027','#fc8d59','#fc8d59','#fc8d59','#fee090','#fee090','#fee090','#e0f3f8','#e0f3f8','#e0f3f8','#91bfdb','#91bfdb','#91bfdb','#4575b4','#4575b4','#4575b4')
#colvec <- c('#b2182b','#b2182b','#b2182b','#ef8a62','#ef8a62','#ef8a62','#CC3366','#fddbc7','#fddbc7','#d1e5f0','#d1e5f0','#d1e5f0','#67a9cf','#67a9cf','#67a9cf','#2166ac','#2166ac','#2166ac')
#colvec <- c("orange","orange","orange","magenta","magenta","magenta","red2","red2","red2", "black","black","black","mediumblue","mediumblue","mediumblue","green4","green4","green4")
with(dung, points(scrs$sites, col="black", bg= "grey",
                  pch = symvec,cex=4,lwd=1.5))

legend("bottomleft",bty="n",pch=c(0,4,2,1,3,6),pt.cex=2,cex=1.5,pt.bg="black",legend=c("1.Banana","2.Agroforestry","3.Disturbed secondary","4.Cleared regenerating","5.Mixed history","6.Minimally disturbed"))
#sites <- c("1.BAA","1.BAB","1.BAC","2.AFA","2.AFB","2.AFC","3.SFA","3.SFB","3.SFC","4.CCRA","4.CCRB","4.CCRC","5.MXDA","5.MXDB","5.MXDC","6.MINA","6.MINB","6.MINC")
#text(scrs$sites,labels=sites,pos=4)
text(-0.01,0.34,"PC2",srt=90,font=2)
text(-0.52,-0.01,"PC1",font=2)
dev.off()

#bw friendly version - with numbers instead of symbols
tiff(file="C://Data/PhD/Outputs/Dungbeetles/Legendre/PCA_community_comp-bwnum.tiff",width=190,height=190,units="mm",res=1000,pointsize=14)  
xlim <- with(scrs, range((scrs$species[,1])/1.8, scrs$sites[,1]))
ylim <- with(scrs, range((scrs$species[,2])/1.8, scrs$sites[,2]))
par(mar=c(1,1,1,1))
plot.new()
plot.window(xlim = xlim, ylim = ylim, asp = 1)
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")
symvec <- c(rep("1",3),rep("2",3),rep("3",3),rep("4",3),rep("5",3),rep("6",3))
with(dung, points(scrs$sites, col="black", bg= "grey",
                  pch = symvec,cex=2,lwd=1.5))
text(-0.01,0.34,"PC2",srt=90,font=2)
text(-0.52,-0.01,"PC1",font=2)
dev.off()

#sitenames <- c("BAA","BAB","BAC","AFA","AFB","AFC","SFA","SFB","SFB","CCA","CCB","CCC","MXA","MXB","MXC","MDA","MDB","MDC")
#rownames(scrs$sites)
#labsp6 <- c("C.virens","O.haematopus","C.monilifer","E.hypocrita","E.nigrovirens","O.rubrescens")
#with(scrs, text((scrs$species[sp6,]/2), labels = labsp6,
#               col = "darkcyan", cex = 0.8))
#with(scrs, text((scrs$sites+0.02), labels = sitenames,
#               col = "black", cex = 1))
##/////////////
# Beta diversity partitioning, LCBD/SCBD ----
##/////////////
oldparamadeg <- adegpar() #save original graphical settings before making changes
# full dungbeetle dataset
dung  <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/db_spXsite.csv",row.names=1) #column 1 has row names
head(dung)
res = beta.div(dung, "hellinger", nperm=999)

# Plot a map of the LCDB indices (coordinates converted so that min x and min y are each 0, 
# and the others are relatvive distnces frm these in m)
dung.xy  <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/sites_xycoords_plotting.csv",row.names=1) #column 1 has row names
#png(filename="C://Data/PhD/Outputs/Dungbeetles/LCBD_DB.png", type="cairo",units="in", width=10, height=8, pointsize=12,res=96)
adegpar(paxes.draw = TRUE, psu.ce = 3,pback.col = "lightgrey",pgrid=list(text.col = "white", text.cex = 1))
g <- adegraphics::s.value(dung.xy, res$LCBD, symbol="circle", col = c("black", "blue"))
addtext(g,dung.xy[,1],dung.xy[,2],rownames(dung.xy),plabels.col = "black",plabels.cex = 0.7)
#dev.off()
#there are 2 packages with a function of the same name, so need to specify the package

### Example using the abundance data and the percentage difference dissimilarity - needs to be square-rooted
# -  doesn't give the species SCBD, because these are lost when dissimilarity matrix calculated
#res = beta.div(dung, "%diff", sqrt.D=T, nperm=999, clock=TRUE)

# Plot a map of the LCDB indices, with significantly different sites coloured red
signif = which(res$p.LCBD <= 0.05)  # Which are the significant LCDB indices?
nonsignif = which(res$p.LCBD > 0.05)  # Which are the non-significant LCDB indices?
g1 <- adegraphics::s.value(dung.xy[signif,], res$LCBD[signif], ppoint.alpha = 0.5, plegend.drawKey = FALSE,
                           symbol = "circle", col = c("white", "red"), main="Map of mite LCBD (red = significant indices)")
g2 <- adegraphics::s.value(dung.xy[nonsignif,], res$LCBD[nonsignif], ppoint.alpha = 0.5,
                           symbol = "circle", col = c("white", "blue"))
g2+g1
addtext(g1,dung.xy[signif,1],dung.xy[signif,2],rownames(dung.xy[signif,]),plabels.col = "black",plabels.cex = 0.7)
addtext(g2,dung.xy[nonsignif,1],dung.xy[nonsignif,2],rownames(dung.xy[nonsignif,]),plabels.col = "black",plabels.cex = 0.7)

g3+g4

# Top SCBD ----
# plot of top 10 SCBD species - those which contribute most to Beta diversity
par(las=2) # make label text perpendicular to axis
par(mar=c(5,12,4,2)) # increase y-axis margin.
top10SCBD<- sort(res$SCBD,decreasing=T)[1:10]
barplot(rev(sort(res$SCBD,decreasing=T)[1:10]),horiz=T,
        main="Species with highest 
        contributions to beta diversity",
        xlab="SCBD value")
top10SCBD<- sort(res$SCBD,decreasing=T)[1:10]
sp6 <- names(top6SCBD)
sp10<- names(top10SCBD)



# BEta diversity paritioning: richness vs replacement
# Compute and partition a matrix of %difference indices (quantitative form of S?rensen index)
db.transf = decostand(dung,"hel")
outJ = beta.div.comp(db.transf, coef="J", quant=F) # presence absence?
out2 = beta.div.comp(db.transf, coef="S", quant=T) # takes abundance
out2$part # Most of beta diversity in this dataset is due to species replacement (63% replacement, 37% richness difference
out2$Note
#R he 5 values are:
# 0.31093880 BDtotal (total beta diversity) = sum(D.ij)/(n*(n-1)) (Legendre & De C?ceres 2013). This is equal to sum(d.ij^2)/(n*(n-1)) where d.ij = sqrt(D.ij). The dissimilarities are square-rooted because the Jaccard, S?rensen, Ruzicka and %difference indices are not Euclidean.
# 0.26719930 Total replacement diversity.
# 0.04373949 Total richness difference diversity (or nestedness).
# 0.85933086 Total replacement diversity/Total beta diversity.
# 0.14066914 Total richness difference diversity (or nestedness)/Total beta diversity.
# *^^ calculated on 05/06/17 


dung  <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/db_spXsite.csv",row.names=1) #column 1 has row names
head(dung)
res = beta.div(dung, "hellinger", nperm=999)

# Plot a map of the LCDB indices (coordinates converted so that min x and min y are each 0, 
# and the others are relatvive distnces frm these in m)
dung.xy  <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/sites_xycoords_plotting.csv",row.names=1) #column 1 has row names

g1 <- adegraphics::s.value(dung.xy, res$LCBD, symbol="circle", col = c("white","brown"), main="Map of dungbeetle LCBD")
addtext(g1,dung.xy[,1],dung.xy[,2],rownames(dung.xy),plabels.col = "black",plabels.cex = 0.7)



###################################
#### Abundance of top SCBD species ----
##################################
head(dung)

db.sp <- t(dung)
head(db.sp)

BA <- db.sp[,1]+db.sp[,2]+db.sp[,3]
AF <- db.sp[,4]+db.sp[,5]+db.sp[,6]
SF <- db.sp[,7]+db.sp[,8]+db.sp[,9]
CC <- db.sp[,10]+db.sp[,11]+db.sp[,12]
MX <- db.sp[,13]+db.sp[,14]+db.sp[,15]
MD <- db.sp[,16]+db.sp[,17]+db.sp[,18]

dbsp <-cbind.data.frame(BA,AF,SF,CC,MX,MD)

names(dbsp) <- c("1.Banana","2.Agroforestry","3.Secondary","4.Cleared regen.","5.Mixed","6.Minimal dist.")

dbsp <- as.matrix(dbsp)

rows10<- which(rownames(dbsp) %in% sp10)

db10 <- dbsp[rows10,] # select the rows with the species with top SCBD
par(mar=c(5,4,4,2)) # increase y-axis margin.

rownames(db10)
sp10names <- c("Canthon monilifer","Canthon virens","Deltochilum carinatum","Dichotomius batesi","Dichotomius conicollis","Eurysternus hypocrita","Eurysternus nigrovirens", "Onthophagus haemotopus","Onthophagus rubrescens","Onthophagus xanthomerus")
cols10 <- c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')

bw10names <- c("Canthon monilifer (a)","Canthon virens (b)","Deltochilum carinatum (c)","Dichotomius batesi (d)","Dichotomius conicollis (e)","Eurysternus hypocrita (f)","Eurysternus nigrovirens (g)", "Onthophagus haemotopus (h)","Onthophagus rubrescens (i)","Onthophagus xanthomerus (j)")

new10 <- brewer.pal(10,"Paired")
blue10 <- brewer.pal(10,"BrBG")
#png(file="C://Data/PhD/Outputs/Dungbeetles/Legendre/SCBD10_abund.png",width=10,height=7.5,units="in",res=180)  
tiff(file="C://Data/PhD/Outputs/Dungbeetles/db chapter figures/SCBD10_abund.tiff",width=190,height=100,units="mm",res=1000, pointsize=9)  
bp <- barplot(db10,beside=T,ylab="No. of Individuals",xlab="Disturbance Rank",cex.axis=1.2,cex.lab=1.5,ylim=c(0,560),col=blue10,names.arg = c("1","2","3","4","5","6"))
legend(0,560,legend=bw10names,text.font=3,pch=21,pt.cex=1.5,cex=1,col=c("black"),pt.bg=blue10,bty="n")
text(bp,y=5, labels=c("a","b","c","d","e","f","g","h","i","j"),pos=1)
dev.off()

# greyscale graph
gray10 <- c("gray100","gray88","gray73","gray60","gray47","gray33","gray27","gray20","gray16","gray1")
bw10names <- c("Canthon monilifer (a)","Canthon virens (b)","Deltochilum carinatum (c)","Dichotomius batesi (d)","Dichotomius conicollis (e)","Eurysternus hypocrita (f)","Eurysternus nigrovirens (g)", "Onthophagus haemotopus (h)","Onthophagus rubrescens (i)","Onthophagus xanthomerus (j)")

tiff(file="C://Data/PhD/Outputs/Dungbeetles/Biotropica figures/Fig4_SCBD10_abund.tiff",width=190,height=100,units="mm",res=1000, pointsize=12)  
par(mar=c(3.8,4,0.1,0.1))
bp <- barplot(db10,beside=T,ylab="No. of Individuals",xlab="Disturbance Rank",ylim=c(-50,570),cex.axis=1.2,cex.lab=1.2,col=blue10,names.arg = c("1","2","3","4","5","6"))
legend(0,580,legend=bw10names,text.font=3,pch=21,pt.cex=1.3,cex=0.8,col=c("black"),pt.bg=blue10,bty="n")
text(bp,y=5, labels=c("a","b","c","d","e","f","g","h","i","j"),pos=1,font=3)
dev.off()

# plot abundance of all species at all habitats
#fake rownames 
rows <- cbind(dbsp,"zeros"=rep(0,length(dbsp[,1])))
#png(file="C://Data/PhD/Outputs/Dungbeetles/Abundance_all.png",width=14,height=25,units="in",res=300)  
par(mfrow=c(7,1))
par(mar=c(2,3,2.1,2.1))
barplot(dbsp[,1],main="1. Banana",names.arg=c(1:56))
barplot(dbsp[,2],main="2. Agroforestry",names.arg=c(1:56))
barplot(dbsp[,3],main="3. Secondary",names.arg=c(1:56))
barplot(dbsp[,4],main="4. Cleared Regenerating",names.arg=c(1:56))
barplot(dbsp[,5],main="5. Mixed Use",names.arg=c(1:56))
barplot(dbsp[,6],main="6. Minimally disturbed",names.arg=c(1:56))
par(mar=c(12,3,0,2.1))
barplot(rows[,7],axes=NULL,las=2)
dev.off()



## RDA for dung beetles ----
#custom pca plot
## read in environemtal variables
env  <- read.csv("C:/Data/PhD/Processed_data/Site_data/sites_elev_dist_coords.csv",row.names=1) #column 1 has row names
dung  <- read.csv("C:/Data/PhD/Processed_data/Dungbeetles/db_spXsite.csv",row.names=1) #column 1 has row names

## filter only variables of interest:
sel <- c(1,3,10) # distance to river, elevation and disturbance rank
env <- env[,sel]
env <- as.matrix(env)
dung <- as.matrix(dung)
#envCS = scale(env, center=TRUE, scale=TRUE)
dung.transf = decostand(dung,"hel")
rda.out <- rda(dung.transf,env) ## same results whether or not yu scale env variables in advance

#summary(summary(rda.out)) # STRUCTURE OF SUMMARY FILE
summary(rda.out) #summary of rda.out

plot(rda.out, display=c("sp","sites","bp")) 
scrs <- scores(rda.out,display = c("sp","sites","bp")) #extract sepecies and site scores from rda summary
png(file="C://Data/PhD/Outputs/Dungbeetles/Biotropica figures/Fig3_DB_RDA_community.png",width=190,height=180,units="mm",res=1000, pointsize=12)
xlim <- c(-1,1)#with(scrs, range((scrs$sp[,1])/1.8, scrs$sites[,1]))
ylim <- c(-1.5,1.5)#with(scrs, range((scrs$sp[,2])/1.8, scrs$sites[,2]))
plot.new()
plot.window(xlim = xlim, ylim = ylim, asp = 1)
axis(side=1, at=c(-1.5,-1,-0.5,0.5,1,1.5), lty="solid", pos=c(0,0),padj=-1,cex.axis=1)
axis(side=2, at=c(-1.5,-1,-0.5,0.5,1,1.5), lty="solid", pos=c(0,0),padj=1,cex.axis=1)
#abline(h = 0, lty = "dotted")
#abline(v = 0, lty = "dotted")
colvec <- c('#d73027','#d73027','#d73027','#fc8d59','#fc8d59','#fc8d59','#fee090','#fee090','#fee090','#e0f3f8','#e0f3f8','#e0f3f8','#91bfdb','#91bfdb','#91bfdb','#4575b4','#4575b4','#4575b4')
points(scrs$sites, col="black", bg= colvec, pch = 21,cex=4)
# plot arrows for each environental variable
arrows(x0=0, y0=0, scrs$biplot[1,1], scrs$biplot[1,2], code=2, lwd=2, col="black")
arrows(x0=0, y0=0, scrs$biplot[2,1], scrs$biplot[2,2], code=2, lwd=2, col="black")
arrows(x0=0, y0=0, scrs$biplot[3,1], scrs$biplot[3,2], code=2, lwd=2, col="black")
text(x=scrs$biplot[1,1]+0.3, y=scrs$biplot[1,2]+0.1,labels = c("Distance to river"))
text(x=scrs$biplot[2,1]+0.1, y=scrs$biplot[2,2]+0.1,labels = c("Elevation"))
text(x=scrs$biplot[3,1]+0.3, y=scrs$biplot[3,2]+0.1,labels = c("Disturbance rank"))
text(x=-1.66, y=0,labels = c("RDA1"))
text(x=0.05, y=-1.4,labels = c("RDA2"),srt=90)
legend("bottomleft",bty="n",pch=21,pt.cex=1.5,cex=1,pt.bg=c('#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4'),legend=c("1.Banana (most disturbed)","2.Agroforestry","3.Disturbed secondary","4.Cleared regenerating","5.Mixed history","6.Minimally disturbed"))
dev.off()
#max(scrs$sites)


