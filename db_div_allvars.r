# DB data - combining all datasets for models
# Nov 2016

rm(list=ls())

# libraries ----
library(ape) # for moran's i 
 require(dplyr)
 library(tidyr)
 library(stringr)

## source functions ----
source("C://Data/PhD/Analysis/Dungbeetles/correlation_morans_functions.r")

# Preparing dataframe ----

# #
weather <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_weather.csv")
veg <- read.csv("C://Data/PhD/Processed_data/Vegetation/veg_pca_out.csv")
alpha <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/Diversity/alpha.csv")
site_data <- read.csv("C://Data/PhD/Processed_data/Site_data/sites_elev_dist_coords.csv")
rich <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/ESoutput/DB_ES_RichEst_replacement.csv")
abund <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_Abund_groups.csv")
soil <- read.csv("C://Data/PhD/Processed_data/Soil/Soil_data.csv")
Estqs <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/db_estqs_inext.csv") #old asymptotic 95SC estimates don't use

head(abund)
abund <- abund[,-1]
head(weather)
head(veg)
head(alpha)
head(site_data)
head(rich)
head(soil)
head(Estqs)
#
 str(rich) #get rid of some of the excess columns of richness output
 rem_rich <- c(8,9,10,11,12,13,14,15)#cols of richness ests to remove
 rich <- rich[,-rem_rich]
#
 # Edited alpha file in Excel to separate Site_rank into rank and site columns
 # Also edited richness output to replace spaces with underscores and remove % and () from colnames
colnames(weather)[1] <- "Site"
 colnames(veg)[1] <- "Site"
 colnames(rich)[1] <- "Site"
 colnames(alpha)[1] <- "Site"

 newalphasite <- str_split_fixed(alpha$Site, ".",3) # split Site column, to get site values without Rank. infront
 alpha$Site <- newalphasite[,3]
#
 alpha$Site <- as.factor(alpha$Site)
 levels(alpha$Site)
 levels(alpha$Site)[levels(alpha$Site)=="MinD-B"] <- "MIN-B"
 levels(alpha$Site)[levels(alpha$Site)=="MinD-A"] <- "MIN-A"
 levels(alpha$Site)[levels(alpha$Site)=="MinD-C"] <- "MIN-C"

 levels(weather$Site)[levels(weather$Site)=="T6-850"] <- "MIN-B"
 levels(weather$Site)[levels(weather$Site)=="T7-1350"] <- "MIN-A"
 levels(weather$Site)[levels(weather$Site)=="CH-400"] <- "MIN-C"
 levels(weather$Site)[levels(weather$Site)=="T1-650"] <- "CCR-B"
 levels(weather$Site)[levels(weather$Site)=="T2-800"] <- "CCR-A"
 levels(weather$Site)[levels(weather$Site)=="T5-250"] <- "CCR-C"
 levels(weather$Site)[levels(weather$Site)=="T2-2150"] <- "MXD-A"
 levels(weather$Site)[levels(weather$Site)=="T10-100"] <- "MXD-B"
 levels(weather$Site)[levels(weather$Site)=="T3-1800"] <- "MXD-C"

#
#
 DB_merge1 <- merge(alpha,site_data,by="Site")
 DB_merge2 <- merge(DB_merge1,veg,by="Site")
 DB_merge3 <- merge(DB_merge2,weather,by="Site")
 DB_merge4 <- merge(DB_merge3,abund,by="Site")
 DB_merge5 <- merge(DB_merge4,soil,by="Site")
 DB_merge6 <- merge(DB_merge5,Estqs,by="Site")
 DB_allvars <- merge(DB_merge6,rich,by="Site")

head(DB_allvars)
write.csv(DB_allvars,"C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars.csv")


####//////////////////
## Analysis of biodiversity patterns ----
#///////////////////////
# Read in Data ----

DB <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars_EqSSests.csv")

 head(DB,n=1)
 str(DB)
 
DB$Rank <- as.numeric(DB$Rank)
DB$q0 <- as.numeric(DB$q0)
DB$River_dist <- as.numeric(DB$River_dist)
DB$Road_dist <- as.numeric(DB$Road_dist)
DB$Elevation <- as.numeric(DB$Elevation)
DB$UTM_Coords_E <- as.numeric(DB$UTM_Coords_E)
DB$UTM_Coords_S <- as.numeric(DB$UTM_Coords_S)
DB$Rain_min <- as.numeric(DB$Rain_min)
DB$Hum_max <- as.numeric(DB$Hum_max)
DB$Hum_min <- as.numeric(DB$Hum_min)
DB$S_est <- as.numeric(DB$S_est)
DB$Site <- as.factor(as.character(DB$Site))

head(DB)

# 

####
####
# Models ----

# before model, check for collinearity between variables, so there's no need to test all of them.

# Correlation tests ----
#weather
cor.test(DB$Hum19mean,DB$Temp19mean, method = "spearman",exact=F)
cor.test(DB$Hum19mean,DB$Rain_mean, method = "spearman",exact=F)
cor.test(DB$Rain_mean,DB$Temp19mean, method = "spearman",exact=F)
## Temp, humidity and rainfall are alll correlated - so probably should just use one of them

cor.test(DB$Rank,DB$Road_dist, method = "spearman",exact=F)
cor.test(DB$Rank,DB$River_dist, method = "spearman",exact=F)
cor.test(DB$Rank,DB$Elevation, method = "spearman",exact=F)
# Road and elevation are correlated with rank -  but road is a genuine indicator of distirbance intensity, 
# and the elevational range is only ~200m, so very unlikely to influence the dung beetle community

# plotting weather vars to see which is more likely to 
# be influential - only using one because highly correlated.
plot(q0~Rain_mean,data=DB)
plot(q0~Temp19mean,data=DB)
plot(q0~Hum19mean,data=DB)

# strongest correlation from rain so will use that to control for weather effects
cor.test(DB$Rain_mean,DB$q0,method = "spearman",exact=F)
cor.test(DB$Hum19mean,DB$q0,method = "spearman",exact=F)
cor.test(DB$Temp19mean,DB$q0,method = "spearman",exact=F)



## Checklist of variables to consider:
# rain              - Rain_mean
# temp              - Temp19mean
# humidity          - Hum19mean
# distance to river - River_dist
# elevation         - Elevation 
# distance to road  - Road_dist
# rank              - Rank
####


## using spearman rank correlation test instead of glm, because disturbance rank may not be perfectly linear 
# (i.e. diff between 1 and 2 may be more than diff between 2 and 3, so rank correlation is better suited to this)

# Use the correlation test to check for a relationship between diversity and disturbance
# rank correlation (more general, so less likely to affected by incorrect assumptions about the shape (linearity) of the disturbance rank variable)


## results table----
## run through correlation and morans tests for each variable against disturbance rank
respnames <- c("Observed q0","Observed q1","Observed q2","Observed qInf","Estimated q0","Estimated q1","Estimated q2","Total Abundance","Vegetation structure PC1")
resps <- list(DB$q0,DB$q1,DB$q2,DB$qInf,DB$e95q0,DB$e95q1,DB$e95q2,DB$Abund_total,DB$veg_pc1)
corresults <- as.data.frame(cbind("Response"=respnames,"S"=numeric(length(resps)),"p"=numeric(length(resps)),"rho"=numeric(length(resps)),"LCI"=numeric(length(resps)),"UCI"=numeric(length(resps))))
morresults <- as.data.frame(cbind("Response"=respnames,"Obs"=numeric(length(resps)),"Exp"=numeric(length(resps)),"sd"=numeric(length(resps)),"p"=numeric(length(resps)),"Obs-Exp"=numeric(length(resps))))
for(c in 2:6){ ## convert columns to numeric
  corresults[,c] <- as.numeric(as.character((corresults[,c])))
  morresults[,c] <- as.numeric(as.character((morresults[,c])))
}
str(morresults)

r <- 9
for(r in 1:length(respnames)){
  response <- c(resps[[r]])
  cr <- docor(DB$Rank, response) 
  corresults[r,] <- cbind("response"=respnames[r],cr)
  if(r<9){
    m0 <- lm(log(response)~DB$Rank)
    mr <- morans(m0) 
    morresults[r,] <- cbind("response"=respnames[r],mr)}
  else{
    m0 <- lm(response~DB$Rank)
    mr <- morans(m0) 
    morresults[r,] <- cbind("response"=respnames[r],mr)}
}
#write.csv(morresults,"C://Data/PhD/Outputs/Dungbeetles/moransi_240419.csv")
#write.csv(corresults,"C://Data/PhD/Outputs/Dungbeetles/diversitycorrelations_240419.csv")

## vegetation~ rank plot ----

#png(file="C://Data/PhD/Outputs/Vegetation/veqpc1-rank.png",width=10,height=7.5,units="in",res=250)  
#tiff("C://Data/PhD/Outputs/Vegetation/veqpc1-rank-nojitter_abline.tiff",width=140,height=140,units="mm",res=500, pointsize=12)  
#tiff(file="C://Data/PhD/Outputs/Orchidbees/Figures/Ecolind/Fig1-1column-vegpc1-rank.tiff",width=90,height=100,units="mm",res=1000, pointsize=9)  
#tiff(file="C://Data/PhD/Outputs/Vegetation/vegpc1-rank.tiff",width=90,height=100,units="mm",res=1000, pointsize=9)  
#tiff(file="C://Data/PhD/Outputs/Dungbeetles/Biotropica figures/FigS7-vegpc1-rank.tiff",width=90,height=100,units="mm",res=1000, pointsize=9)  
par(mfrow=c(1,1),mar=c(4,4,1,1))
#plot(DB$veg_pc1~DB$Rank,bty="n",pch=21,cex=1.5,lwd=3,cex.axis=1.2,cex.lab=1.2,col='black',data=DB,ylab="Vegetation Structure PC1 Scores",xlab="Disturbance Rank")
plot(DB$veg_pc1~DB$Rank,bty="n",pch=21,cex=1.8,lwd=1,cex.axis=1,cex.lab=1,col='black',data=DB,ylim=c(-1.5,1.6),ylab="Vegetation Structure PC1",xlab="Disturbance Rank")
abline(lm(DB$veg_pc1~DB$Rank),lwd=1)
dev.off()


####
### Estimated diversity ----


## DB diversity permutation test ----

# check for a continuous pattern across q=0,1,2,and inf by permuting the
# results at each value of q, and then combining the p values.

head(DB)

#DB$Rank,  DB$q0, DB$q0.5, DB$q1, DB$q2, DB$qInf

# mini function to make cortest shorter
co <- function(ia,ib){
res <- cor.test(ia, ib, method = "spearman",exact=F)
cbind(res$p.value)}

co(DB$Rank,DB$q0.5)

# then re-run with re-ordered ranking, 1000 times.
perms <- c(NULL)
for(s in 1:10000){
rs <- sample(rep(1:6,3))
cr0 <- co(rs,DB$q0)
cr05 <- co(rs,DB$q0.5)
cr1 <- co(rs,DB$q1)
cr2 <- co(rs,DB$q2)
crInf <- co(rs,DB$qInf)# correlation result
logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2)+log(crInf) #combined p vals
run <- cbind(s,cr0,cr05,cr1,cr2,crInf,logcr)
perms <- rbind(perms,run)
}
head(perms)
perms <- as.data.frame(perms)
names(perms) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_qInf","p_logcr")

# compare permuted results with real correlation
cr0 <- co(DB$Rank,DB$q0)
cr05 <- co(DB$Rank,DB$q0.5)
cr1 <- co(DB$Rank,DB$q1)
cr2 <- co(DB$Rank,DB$q2)
crInf <- co(DB$Rank,DB$qInf)# correlation result
logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2)+log(crInf)
real<- as.data.frame(cbind(0,cr0,cr05,cr1,cr2,crInf,logcr)) #Run 0 is the real values
names(real) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_qInf","p_logcr")
comb <- rbind(perms,real)
head(comb)
tail(comb)

# check if real (Run 0) is in bottom 5% of pvals for each
pq0 <- rank(comb$p_q0)[10001]/length(comb[,1]) #divide the rank of the real p val by the total number to ge the p value of the probability of getting that result by chance
pq05 <- rank(comb$p_q05)[10001]/length(comb[,1])
pq1 <- rank(comb$p_q1)[10001]/length(comb[,1])
pq2 <- rank(comb$p_q2)[10001]/length(comb[,1])
pqI <- rank(comb$p_qInf)[10001]/length(comb[,1])
pqlog <- rank(comb$p_logcr)[10001]/length(comb[,1])

pvals_perm <- cbind(pq0,pq05,pq1,pq2,pqI,pqlog)
pvals_perm


#///////////
# Permutation - Same for estimated diversity-----
#  re-run with re-ordered ranking, 1000 times.
perms <- c(NULL)
for(s in 1:10000){
  rs <- sample(rep(1:6,3))
  cr0 <- co(rs,DB$Est0_ss)
  cr05 <- co(rs,DB$Est05_ss)
  cr1 <- co(rs,DB$Est1_ss)
  cr2 <- co(rs,DB$Est2_ss)
  #crInf <- co(rs,DB$EstInf_ss)# correlation result
  logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2)#+log(crInf) #combined p vals
  run <- cbind(s,cr0,cr05,cr1,cr2,logcr)
  perms <- rbind(perms,run)
}
head(perms)
perms <- as.data.frame(perms)
names(perms) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_logcr")

# compare permuted results with real correlation
cr0 <- co(DB$Rank,DB$Est0_ss)
cr05 <- co(DB$Rank,DB$Est05_ss)
cr1 <- co(DB$Rank,DB$Est1_ss)
cr2 <- co(DB$Rank,DB$Est2_ss)
#crInf <- co(DB$Rank,DB$EstInf_ss)# correlation result
logcr <- log(cr0)+log(cr05)+log(cr1)+log(cr2)
real<- as.data.frame(cbind(0,cr0,cr05,cr1,cr2,logcr)) #Run 0 is the real values
names(real) <- c("Run","p_q0","p_q05","p_q1","p_q2","p_logcr")
comb <- rbind(perms,real)
head(comb)
tail(comb)

# check if real (Run 0) is in bottom 5% of pvals for each
pq0 <- rank(comb$p_q0)[10001]/length(comb[,1]) #divide the rank of the real p val by the total number to ge the p value of the probability of getting that result by chance
pq05 <- rank(comb$p_q05)[10001]/length(comb[,1])
pq1 <- rank(comb$p_q1)[10001]/length(comb[,1])
pq2 <- rank(comb$p_q2)[10001]/length(comb[,1])
#pqI <- rank(comb$p_qInf)[10001]/length(comb[,1])
pqlog <- rank(comb$p_logcr)[10001]/length(comb[,1])

pvals_perm <- cbind(pq0,pq05,pq1,pq2,pqlog)
pvals_perm
