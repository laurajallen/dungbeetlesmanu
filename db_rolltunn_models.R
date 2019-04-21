###////////////////////////////////////////////
  ###
##////////////////////////////////////////////////
# Dung beetle diversity - split by rollers/tunnelers and small/large

  ## Import data and combine datframes ----
#DB data ROLLERS
#
 rm(list=ls())
require(dplyr)
library(tidyr)
library(stringr)
#
weather <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_weather.csv")
veg <- read.csv("C://Data/PhD/Processed_data/Vegetation/veg_pca_out.csv")
alpha <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/Diversity/alpha_tunnels.csv")
site_data <- read.csv("C://Data/PhD/Processed_data/Site_data/sites_elev_dist_coords.csv")
rich <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/ESoutput/Tunnels_ES_output.csv")
abund <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_Abund_groups.csv")
#
head(abund)
abund <- abund[,-1]
head(weather)
head(veg)
head(alpha)
head(site_data)
 head(rich)
#
 str(rich) #get rid of some of the excess columns of richness output
 rem_rich <- c(8,9,10,11,12,13,14,15)#cols of richness ests to remove
 rich <- rich[,-rem_rich]
#
#Edited alpha file in Excel to separate Site_rank into rank and site columns
#Also edited richness output to replace spaces with underscores and remove % and () from colnames
colnames(weather)[1] <- "Site"
colnames(veg)[1] <- "Site"
colnames(rich)[1] <- "Site"
#
alpha[,3] <- as.factor(alpha[,3])
levels(alpha[,3])
levels(alpha$Site)[levels(alpha$Site)=="MinD-B"] <- "MIN-B"
levels(alpha$Site)[levels(alpha$Site)=="MinD-A"] <- "MIN-A"
levels(alpha$Site)[levels(alpha$Site)=="MinD-C"] <- "MIN-C"
#
#
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
#
DB_merge1 <- merge(alpha,site_data,by="Site")
DB_merge2 <- merge(DB_merge1,veg,by="Site")
DB_merge3 <- merge(DB_merge2,weather,by="Site")
DB_merge4 <- merge(DB_merge3,abund,by="Site")
DB_allvars <- merge(DB_merge4,rich,by="Site")
#
#write.csv(DB_allvars,"C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars-TUNNELS.csv")

#////////////////////////////////
# ROLLERS - read in combined dataframe ----
DBroll <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars-ROLLERS.csv")

head(DBroll,n=1)
str(DBroll)

DBroll$Rank <- as.numeric(DBroll$Rank)
DBroll$q0 <- as.numeric(DBroll$q0)
DBroll$River_dist <- as.numeric(DBroll$River_dist)
DBroll$Road_dist <- as.numeric(DBroll$Road_dist)
DBroll$Elevation <- as.numeric(DBroll$Elevation)
DBroll$UTM_Coords_E <- as.numeric(DBroll$UTM_Coords_E)
DBroll$UTM_Coords_S <- as.numeric(DBroll$UTM_Coords_S)
DBroll$Rain_min <- as.numeric(DBroll$Rain_min)
DBroll$Hum_max <- as.numeric(DBroll$Hum_max)
DBroll$Hum_min <- as.numeric(DBroll$Hum_min)
DBroll$Sest <- as.numeric(DBroll$Sest)

head(DBroll)

# models - correlation and lm(div~rank) ----

### q=0 ----
cor.test(DBroll$Rank, DBroll$q0, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBroll$Rank), replace = TRUE)
    cor(DBroll$Rank[boot.i], DBroll$q0[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(q0)~Rank,data=DBroll)
logLik(m0)
summary(m0)

m1 <- lm(log(q0)~Rank+Elevation,data=DBroll) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(q0)~Rank+River_dist,data=DBroll) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(q0)~Rank+Road_dist,data=DBroll) 
anova(m0,m3) 
logLik(m3)

m4 <- lm(log(q0)~Rank+Rain_mean,data=DBroll) 
anova(m0,m4)
logLik(m4)

#Moran's i test for spatial autocorrelation
library (ape)
resids <- m0$residuals
q0moran <- cbind.data.frame(DBroll$Site,resids,DBroll$UTM_Coords_S,DBroll$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DBroll$UTM_Coords_E,DBroll$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(q2moran$resids, DB.dists.inv)

## q=1 ----
cor.test(DBroll$Rank, DBroll$q1, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBroll$Rank), replace = TRUE)
    cor(DBroll$Rank[boot.i], DBroll$q1[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(q1)~Rank,data=DBroll)
logLik(m0)
summary(m0)

m1 <- lm(log(q1)~Rank+Elevation,data=DBroll) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(q1)~Rank+River_dist,data=DBroll) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(q1)~Rank+Road_dist,data=DBroll) 
anova(m0,m3) 
logLik(m3)

m4 <- lm(log(q1)~Rank+Rain_mean,data=DBroll) 
anova(m0,m4)
logLik(m4)

#Moran's i test for spatial autocorrelation
resids <- m0$residuals
q1moran <- cbind.data.frame(DBroll$Site,resids,DBroll$UTM_Coords_S,DBroll$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DBroll$UTM_Coords_E,DBroll$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(q1moran$resids, DB.dists.inv)


## q=2 ----
cor.test(DBroll$Rank, DBroll$q2, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBroll$Rank), replace = TRUE)
    cor(DBroll$Rank[boot.i], DBroll$q2[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(q2)~Rank,data=DBroll)
logLik(m0)
summary(m0)

m1 <- lm(log(q2)~Rank+Elevation,data=DBroll) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(q2)~Rank+River_dist,data=DBroll) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(q2)~Rank+Rain_mean,data=DBroll) 
anova(m0,m3)
logLik(m3)

#Moran's i test for spatial autocorrelation
resids <- m0$residuals
q2moran <- cbind.data.frame(DBroll$Site,resids,DBroll$UTM_Coords_S,DBroll$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DBroll$UTM_Coords_E,DBroll$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(q2moran$resids, DB.dists.inv)


## q=3 ----
cor.test(DBroll$Rank, DBroll$q3, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBroll$Rank), replace = TRUE)
    cor(DBroll$Rank[boot.i], DBroll$q3[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(q3)~Rank,data=DBroll)
logLik(m0)
summary(m0)

m1 <- lm(log(q3)~Rank+Elevation,data=DBroll) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(q3)~Rank+River_dist,data=DBroll) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(q3)~Rank+Rain_mean,data=DBroll) 
anova(m0,m3)
logLik(m3)

#Moran's i test for spatial autocorrelation
resids <- m0$residuals
q3moran <- cbind.data.frame(DBroll$Site,resids,DBroll$UTM_Coords_S,DBroll$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DBroll$UTM_Coords_E,DBroll$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(q3moran$resids, DB.dists.inv)


## q=Inf----
cor.test(DBroll$Rank, DBroll$qInf, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBroll$Rank), replace = TRUE)
    cor(DBroll$Rank[boot.i], DBroll$qInf[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(qInf)~Rank,data=DBroll)
logLik(m0)
summary(m0)

m1 <- lm(log(qInf)~Rank+Elevation,data=DBroll) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(qInf)~Rank+River_dist,data=DBroll) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(qInf)~Rank+Rain_mean,data=DBroll) 
anova(m0,m3)
logLik(m3)

#Moran's i test for spatial autocorrelation
resids <- m0$residuals
qInfmoran <- cbind.data.frame(DBroll$Site,resids,DBroll$UTM_Coords_S,DBroll$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DBroll$UTM_Coords_E,DBroll$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(qInfmoran$resids, DB.dists.inv)

#////////////////////////////////
# TUNNELLERS - read in combined dataframe ----
DBtun <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars-TUNNELS.csv")

head(DBtun)
str(DBtun)

DBtun$Rank <- as.numeric(DBtun$Rank)
DBtun$q0 <- as.numeric(DBtun$q0)
DBtun$River_dist <- as.numeric(DBtun$River_dist)
DBtun$Road_dist <- as.numeric(DBtun$Road_dist)
DBtun$Elevation <- as.numeric(DBtun$Elevation)
DBtun$UTM_Coords_E <- as.numeric(DBtun$UTM_Coords_E)
DBtun$UTM_Coords_S <- as.numeric(DBtun$UTM_Coords_S)
DBtun$Rain_min <- as.numeric(DBtun$Rain_min)
DBtun$Hum_max <- as.numeric(DBtun$Hum_max)
DBtun$Hum_min <- as.numeric(DBtun$Hum_min)
DBtun$Sest <- as.numeric(DBtun$Sest)

head(DBtun)
#mmorans function
morans <- function(model){
  resids <- model$residuals 
  resdf <- cbind.data.frame(DBtun$Site,resids,DBtun$UTM_Coords_S,DBtun$UTM_Coords_E)
  DBtun.dists <- as.matrix(dist(cbind(DBtun$UTM_Coords_E,DBtun$UTM_Coords_S))) # create distance matrix of site coordinates
  DBtun.dists.inv <- 1/DBtun.dists # inverse dist matrix
  diag(DBtun.dists.inv) <- 0 # replace diagonal with 0s
  DBtun.dists.inv[1:5, 1:5] # view
  as.data.frame(Moran.I(resdf$resids, DBtun.dists.inv))}


# models - correlation and lm(div~rank) ----
require(ape)
### q=0 ----
res <- cor.test(DBtun$Rank, DBtun$q0, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBtun$Rank), replace = TRUE)
    cor(DBtun$Rank[boot.i], DBtun$q0[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(q0)~Rank,data=DBtun)

m1 <- lm(log(q0)~Rank+Elevation,data=DBtun) 
anova(m0,m1) 

m2 <- lm(log(q0)~Rank+River_dist,data=DBtun) 
anova(m0,m2) 

m3 <- lm(log(q0)~Rank+Rain_mean,data=DBtun) 
anova(m0,m3) 

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)
#Moran's i test for spatial autocorrelation
morans(m0)

## q=1 ----
res <- cor.test(DBtun$Rank, DBtun$q1, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBtun$Rank), replace = TRUE)
    cor(DBtun$Rank[boot.i], DBtun$q1[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(q1)~Rank,data=DBtun)
logLik(m0)


m1 <- lm(log(q1)~Rank+Elevation,data=DBtun) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(q1)~Rank+River_dist,data=DBtun) 
anova(m0,m2) 
logLik(m2)
summary(m2)

m3 <- lm(log(q1)~Rank+River_dist+Rain_mean,data=DBtun) 
anova(m2,m3)
logLik(m3)

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m2)
morans(m2)

#Moran's i test for spatial autocorrelation
resids <- m2$residuals
q1moran <- cbind.data.frame(DBtun$Site,resids,DBtun$UTM_Coords_S,DBtun$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DBtun$UTM_Coords_E,DBtun$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(q1moran$resids, DB.dists.inv)


## q=2 ----
res <- cor.test(DBtun$Rank, DBtun$q2, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBtun$Rank), replace = TRUE)
    cor(DBtun$Rank[boot.i], DBtun$q2[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(q2)~Rank,data=DBtun)
logLik(m0)


m1 <- lm(log(q2)~Rank+Elevation,data=DBtun) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(q2)~Rank+River_dist,data=DBtun) 
anova(m0,m2) 
logLik(m2)
summary(m2)

m3 <- lm(log(q2)~Rank+River_dist+Rain_mean,data=DBtun) 
anova(m2,m3)
logLik(m3)

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m2)
morans(m2)

#Moran's i test for spatial autocorrelation
resids <- m2$residuals
q2moran <- cbind.data.frame(DBtun$Site,resids,DBtun$UTM_Coords_S,DBtun$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DBtun$UTM_Coords_E,DBtun$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(q2moran$resids, DB.dists.inv)


## q=3 ----
res <- cor.test(DBtun$Rank, DBtun$q3, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBtun$Rank), replace = TRUE)
    cor(DBtun$Rank[boot.i], DBtun$q3[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(q3)~Rank,data=DBtun)
logLik(m0)
summary(m0)

m1 <- lm(log(q3)~Rank+Elevation,data=DBtun) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(q3)~Rank+River_dist,data=DBtun) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(q3)~Rank+River_dist+Rain_mean,data=DBtun) 
anova(m2,m3)
logLik(m3)


rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m2)
morans(m2)

#Moran's i test for spatial autocorrelation
resids <- m2$residuals
q3moran <- cbind.data.frame(DBtun$Site,resids,DBtun$UTM_Coords_S,DBtun$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DBtun$UTM_Coords_E,DBtun$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(q3moran$resids, DB.dists.inv)


## q=Inf----
res <- cor.test(DBtun$Rank, DBtun$qInf, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBtun$Rank), replace = TRUE)
    cor(DBtun$Rank[boot.i], DBtun$qInf[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))



#////////////////////////////////////////////////
# Abundance ----
#////////////////////////////////////////////////
# Rollers abundance ----
rm(list=ls())
rollers <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/rollers-countmat.csv",row.names = 1)
head(rollers)
names(rollers) <- c("1.BA-A","1.BA-B","1.BA-C","2.AF-A","2.AF-B","2.AF-C","3.SF-A","3.SF-B","3.SF-C","4.CCR-A","4.CCR-B","4.CCR-C","5.MXD-A","5.MXD-B","5.MXD-C","6.MIN-A","6.MIN-B","6.MIN-C")
ab.roll <- colSums(rollers)
barplot(ab.roll,main="abundance of rollers")

tunnels <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/tunnels-countmat.csv",row.names = 1)
head(tunnels)
names(tunnels) <- c("1.BA-A","1.BA-B","1.BA-C","2.AF-A","2.AF-B","2.AF-C","3.SF-A","3.SF-B","3.SF-C","4.CCR-A","4.CCR-B","4.CCR-C","5.MXD-A","5.MXD-B","5.MXD-C","6.MIN-A","6.MIN-B","6.MIN-C")
ab.tunn <- colSums(tunnels)
barplot(ab.tunn,main="abundance of tunnelers")

sites <- c("1.BA-A","1.BA-B","1.BA-C","2.AF-A","2.AF-B","2.AF-C","3.SF-A","3.SF-B","3.SF-C","4.CCR-A","4.CCR-B","4.CCR-C","5.MXD-A","5.MXD-B","5.MXD-C","6.MIN-A","6.MIN-B","6.MIN-C")

## count how many species of roll/tunn (rows occupied) in each site.
spr <- NULL
spt <- NULL
for(c in 1:18){
spr1 <- length(which(rollers[,c]>0))
spr <- rbind(spr,spr1)
spt1 <- length(which(tunnels[,c]>0))
spt <- rbind(spt,spt1)}

distrank <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6)
sprt <- cbind(sites,sp_roll=spr,sp_tunn=spt,distrank)


#tiff(file="C://Data/PhD/Outputs/Dungbeetles/Biotropica figures/FigS3_rolltun_abundrich.tiff",width=190,height=190,units="mm",res=600, pointsize=14)  
par(mar=c(4.5,4,0.5,0.5),mfrow=c(2,1))
barplot(t(as.matrix(cbind(ab.roll,ab.tunn))), ylab="Abundance", beside=T, xaxt="n", las=2, col=c("cyan4","lightgoldenrod"), ylim=c(0,350))
legend("topright",legend=c("Rollers","Tunnellers"), fill=c("cyan4","lightgoldenrod"),bty="n")
#dev.off()
barplot(t(as.matrix(cbind(spr,spt))), ylab="Species richness", beside=T, names.arg=sites, las=2, col=c("cyan4","lightgoldenrod"), ylim=c(0,25))
dev.off()


## correlations for roll.tunn and sizes ----
cor.test(as.numeric(spt),distrank, method = "spearman",exact=F) 
  
  # bootstrapped 95% CI
  quantile(
    replicate(10000, {
      boot.i <- sample(length(distrank), replace = TRUE)
      cor(distrank[boot.i], spt[boot.i], method = "spearman")
    }), 
    c(0.025, 0.975))
##########
#combined datframe
DBroll <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars-ROLLERS.csv")

head(DBroll2)#,n=1)
str(DBroll)
Roll <- DBroll[order(DBroll$Site_rank),]
Tunn <- DBtun[order(DBtun$Site_rank),]
#Roll <- cbind(DBroll2,Abund_roll=ab.roll, Abund_tun= ab.tun) # ab.tun calculated further down in script

#tiff(file="C://Data/PhD/Outputs/Dungbeetles/Biotropica figures/FigS3_rolltun_abundrich.tiff",width=190,height=100,units="mm",res=300, pointsize=14)  
par(mar=c(4.5,4,1,0.5))
barplot(t(as.matrix(cbind(Roll$Abund_roll,Roll$Abund_tunn))), ylab="No. of individuals", beside=T, names.arg=Roll$Site_rank, las=2, col=c("cyan4","lightgoldenrod"), ylim=c(0,350))
legend("topright",legend=c("Rollers","Tunnellers"), fill=c("cyan4","lightgoldenrod"),bty="n")
dev.off()
text(xlab=Roll$Site)

par(mar=c(4.5,4,1,0.5))
barplot(t(as.matrix(cbind(Roll$q0,Tunn$q0))), ylab="No. of individuals", beside=T, names.arg=Roll$Site_rank, las=2, col=c("cyan4","lightgoldenrod"), ylim=c(0,350))
legend("topright",legend=c("Rollers","Tunnellers"), fill=c("cyan4","lightgoldenrod"),bty="n")

#t.test(Roll$Abund_roll[1:9],Roll$Abund_roll[10:18]) # 3 worst habitats v 3 best

# correlation
cor.test(Roll$Rank.x, Roll$Abund_roll, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(Roll$Rank), replace = TRUE)
    cor(Roll$Rank[boot.i], Roll$Abund_roll[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
# correlation not significant, but I think there is a 
# preference of the rollers for the worse habitats - chisq or t-test propbs needed.

#////////////////////////////////////////////////
# Tunneler abundance ----
rm(list=ls())
tunnels <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/tunnels-countmat.csv",row.names = 1)

head(tunnels)
names(tunnels) <- c("BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")

ab.tun <- colSums(tunnels)
barplot(ab.tun,main="abundance of tunnellers")

#combined datframe
DBtun <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars-TUNNELS.csv")

head(DBtun,n=1)
str(DBtun)
Tun <- cbind(DBtun,Abund_tun=ab.tun)

plot(Tun$Abund_tun~Tun$Rank)

t.test(Tun$Abund_tun[1:9],Tun$Abund_tun[10:18]) # 3 worst habitats v 3 best

# correlation
cor.test(Tun$Rank, Tun$Abund_tun, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(Tun$Rank), replace = TRUE)
    cor(Tun$Rank[boot.i], Tun$Abund_tun[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
# correlation not significant, but I think there is a 
# preference of the tunnellers for the better habitats - chisq or t-test propbs needed.




#//////////////////////
#/////////////////////

## Beetle SIZES ----

# Data SMALL/LARGE ----
# 
rm(list=ls()) 
require(dplyr)
library(tidyr)
library(stringr)

weather <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_weather.csv")
veg <- read.csv("C://Data/PhD/Processed_data/Vegetation/veg_pca_out.csv")
alpha_small <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/Diversity/alpha_small.csv")
alpha_large<- read.csv("C://Data/PhD/Processed_data/Dungbeetles/Diversity/alpha_large.csv")
site_data <- read.csv("C://Data/PhD/Processed_data/Site_data/sites_elev_dist_coords.csv")

#abundance small
small <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/small-countmat.csv",row.names = 1)
head(small)
names(small) <- c("BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")
ab.small <- colSums(small)

#abundance large
large <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/large-countmat.csv",row.names = 1)
names(large) <- c("BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")
ab.large <- colSums(large)

head(weather)
head(veg)
head(alpha_small)
head(alpha_large)
head(site_data)
names(alpha_small) <- c("Rank","Site","s_q0","s_q1","s_q2","s_q3","s_qInf")
names(alpha_large) <- c("Rank","Site","l_q0","l_q1","l_q2","l_q3","l_qInf")


# Edited alpha file in Excel to separate Site_rank into rank and site columns
colnames(weather)[1] <- "Site"
colnames(veg)[1] <- "Site"

#correct site names
alpha_small[,2] <- as.factor(alpha_small[,2])
levels(alpha_small[,2])
levels(alpha_small$Site)[levels(alpha_small$Site)=="MinD-B"] <- "MIN-B"
levels(alpha_small$Site)[levels(alpha_small$Site)=="MinD-A"] <- "MIN-A"
levels(alpha_small$Site)[levels(alpha_small$Site)=="MinD-C"] <- "MIN-C"

alpha_large[,2] <- as.factor(alpha_large[,2])
levels(alpha_large[,2])
levels(alpha_large$Site)[levels(alpha_large$Site)=="MinD-B"] <- "MIN-B"
levels(alpha_large$Site)[levels(alpha_large$Site)=="MinD-A"] <- "MIN-A"
levels(alpha_large$Site)[levels(alpha_large$Site)=="MinD-C"] <- "MIN-C"

levels(weather$Site)[levels(weather$Site)=="T6-850"] <- "MIN-B"
levels(weather$Site)[levels(weather$Site)=="T7-1350"] <- "MIN-A"
levels(weather$Site)[levels(weather$Site)=="CH-400"] <- "MIN-C"
levels(weather$Site)[levels(weather$Site)=="T1-650"] <- "CCR-B"
levels(weather$Site)[levels(weather$Site)=="T2-800"] <- "CCR-A"
levels(weather$Site)[levels(weather$Site)=="T5-250"] <- "CCR-C"
levels(weather$Site)[levels(weather$Site)=="T2-2150"] <- "MXD-A"
levels(weather$Site)[levels(weather$Site)=="T10-100"] <- "MXD-B"
levels(weather$Site)[levels(weather$Site)=="T3-1800"] <- "MXD-C"



DB_merge0 <- merge(alpha_small,alpha_large,by="Site")
DB_merge1 <- merge(DB_merge0,site_data,by="Site")
DB_merge2 <- merge(DB_merge1,veg,by="Site")
DB_merge3 <- merge(DB_merge2,weather,by="Site")
DB_allvars <- cbind(DB_merge3,ab.small,ab.large)
head(DB_allvars)

#write.csv(DB_allvars,"C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars-SIZES.csv")


## Large/small abundance and richness
large <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/large-countmat.csv",row.names = 1)
head(large)
names(large) <- c("1.BA-A","1.BA-B","1.BA-C","2.AF-A","2.AF-B","2.AF-C","3.SF-A","3.SF-B","3.SF-C","4.CCR-A","4.CCR-B","4.CCR-C","5.MXD-A","5.MXD-B","5.MXD-C","6.MIN-A","6.MIN-B","6.MIN-C")
ab.large <- colSums(large)
barplot(ab.large,main="abundance of large beetle")

small <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/small-countmat.csv",row.names = 1)
head(small)
names(small) <- c("1.BA-A","1.BA-B","1.BA-C","2.AF-A","2.AF-B","2.AF-C","3.SF-A","3.SF-B","3.SF-C","4.CCR-A","4.CCR-B","4.CCR-C","5.MXD-A","5.MXD-B","5.MXD-C","6.MIN-A","6.MIN-B","6.MIN-C")
ab.small <- colSums(small)
barplot(ab.small,main="abundance of small beetles")

sites <- c("1.BA-A","1.BA-B","1.BA-C","2.AF-A","2.AF-B","2.AF-C","3.SF-A","3.SF-B","3.SF-C","4.CCR-A","4.CCR-B","4.CCR-C","5.MXD-A","5.MXD-B","5.MXD-C","6.MIN-A","6.MIN-B","6.MIN-C")

## count how many species of roll/tunn (rows occupied) in each site.
spl <- NULL
sps <- NULL
for(c in 1:18){
  spl1 <- length(which(large[,c]>0))
  spl <- rbind(spl,spl1)
  sps1 <- length(which(small[,c]>0))
  sps <- rbind(sps,sps1)}

spls <- cbind(sites,sp_large=spl,sp_small=sps)

#tiff(file="C://Data/PhD/Outputs/Dungbeetles/Biotropica figures/FigS4_largesmall_abundrich.tiff",width=190,height=190,units="mm",res=600, pointsize=14)  
par(mar=c(4.5,4,0.5,0.5),mfrow=c(2,1))
barplot(t(as.matrix(cbind(ab.large,ab.small))), ylab="Abundance", beside=T, xaxt="n", las=2, col=c("cyan4","lightgoldenrod"), ylim=c(0,350))
legend("topright",legend=c("Large","Small"), fill=c("cyan4","lightgoldenrod"),bty="n")
barplot(t(as.matrix(cbind(spl,sps))), ylab="Species richness", beside=T, names.arg=sites, las=2, col=c("cyan4","lightgoldenrod"), ylim=c(0,25))
dev.off()

## correlations for roll.tunn and sizes ----
cor.test(as.numeric(sps),distrank, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(distrank), replace = TRUE)
    cor(distrank[boot.i], sps[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# SIZE models - correlation and lm(div~rank) ----

size <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars-SIZES.csv")

# small----
### q=0 ----
cor.test(size$Rank, size$s_q0, method = "spearman",exact=F) 

### q=1 ----
cor.test(size$Rank, size$s_q1, method = "spearman",exact=F) 

### q=2 ----
cor.test(size$Rank, size$s_q2, method = "spearman",exact=F) 

### q=3 ----
cor.test(size$Rank, size$s_q3, method = "spearman",exact=F) 

### q=Inf ----
cor.test(size$Rank, size$s_qInf, method = "spearman",exact=F) 

### abundance ----
cor.test(size$Rank, size$ab.small, method = "spearman",exact=F) 

# large----
### q=0 ----
res <- cor.test(size$Rank, size$l_q0, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(size$Rank), replace = TRUE)
    cor(size$Rank[boot.i], size$l_q0[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(l_q0)~Rank,data=size)
logLik(m0)
summary(m0)

m1 <- lm(log(l_q0)~Rank+Elevation,data=size) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(l_q0)~Rank+River_dist,data=size) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(l_q0)~Rank+Rain_mean,data=size) 
anova(m0,m3) 
logLik(m3)

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

resids <- m0$residuals
q1moran <- cbind.data.frame(size$Site,resids,size$UTM_Coords_S,size$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(size$UTM_Coords_E,size$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Mor <- Moran.I(q1moran$resids, DB.dists.inv)
cbind(Mor$observed,Mor$expected,Mor$sd,Mor$p.value)

### q=1 ----
res <- cor.test(size$Rank, size$l_q1, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)


# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(size$Rank), replace = TRUE)
    cor(size$Rank[boot.i], size$l_q1[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(l_q1)~Rank,data=size)
logLik(m0)
summary(m0)

m1 <- lm(log(l_q1)~Rank+Elevation,data=size) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(l_q1)~Rank+River_dist,data=size) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(l_q1)~Rank+Rain_mean,data=size) 
anova(m0,m3) 
logLik(m3)

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)


resids <- m0$residuals
q1moran <- cbind.data.frame(size$Site,resids,size$UTM_Coords_S,size$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(size$UTM_Coords_E,size$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Mor <- Moran.I(q1moran$resids, DB.dists.inv)
cbind(Mor$observed,Mor$expected,Mor$sd,Mor$p.value)

### q=2 ----
res <- cor.test(size$Rank, size$l_q2, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(size$Rank), replace = TRUE)
    cor(size$Rank[boot.i], size$l_q2[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

### q=3 ----
res <- cor.test(size$Rank, size$l_q3, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(size$Rank), replace = TRUE)
    cor(size$Rank[boot.i], size$l_q3[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

### q=Inf ----
res <- cor.test(size$Rank, size$l_qInf, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(size$Rank), replace = TRUE)
    cor(size$Rank[boot.i], size$l_qInf[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

### abundance ----
cor.test(size$Rank, size$ab.large, method = "spearman",exact=F) 


###/////////\\\\\\\\\\\//////////////\\\\\\\\\\\\\
# ## old models ----
# 
# m1<- glm(q1~Temp19mean+Temp19sd +Hum19mean +Hum19sd +Rain_mean+ Rain_sd +Temp_max +Temp_min 
#          +Rain_max +Rain_min +Hum_max +Hum_min, family=gaussian)
# summary(m1)
# 
# m2<- glm(q1~Temp19mean+Temp19sd +Hum19mean +Hum19sd +Rain_mean+ Rain_sd +Temp_max +Temp_min 
#          +Rain_max +Rain_min +Hum_max)
# summary(m2)
# 
# m3<- glm(q1~Temp19mean+Temp19sd +Hum19mean +Hum19sd +Rain_mean+ Rain_sd +Temp_max +Temp_min 
#          +Rain_max +Rain_min)
# m4<- glm(q1~Temp19mean+Temp19sd +Hum19mean +Hum19sd +Rain_mean+ Rain_sd +Temp_max +Temp_min)
# m5<- glm(q1~Temp19mean+Temp19sd +Hum19mean +Hum19sd +Rain_mean+ Rain_sd +Temp_max)
# m6<- glm(q1~Temp19mean+Temp19sd +Hum19mean +Hum19sd +Rain_mean+ Rain_sd)
# m7<- glm(q1~Temp19mean+Temp19sd +Hum19mean +Hum19sd +Rain_mean)
# m8<- glm(q1~Temp19mean+Temp19sd +Hum19mean +Hum19sd)
# m9<- glm(q1~Temp19mean+Temp19sd +Hum19mean)
# m10<- glm(q1~Temp19mean+Temp19sd)
# m11<- glm(q1~Temp19mean+Rain_mean)
# m12<- glm(q1~Temp19mean)
# m13<- glm(q1~Rain_mean)
# m14<- glm(q1~Temp19mean+Rain_sd)
# m15<- glm(q1~Temp19mean+Rain_max)
# m16<- glm(q1~Rain_sd)
# m17<- glm(q1~Rain_max)
# 
# Ldiff <- logLik(m9)-logLik(m10)  
# 1-pchisq(2*(Ldiff),1) # Rain (or anything else) didn't improve model - closest was Rain_mean at p=0.56
# 
# 
# /////////////////
# /////////////////#
# # Now q1 of Rollers ~ environmental vars
# 
# m0<- glm(q1~1) # null model.
# m1 <- glm(q1~Rank)
# summary(m1) # Est=0.46 p=0.006
# 
# m2 <- glm(q1~Rank+Elevation)
# Ldiff <- logLik(m2)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # Elevation not significant
# 
# m3 <- glm(q1~Rank+River_dist)
# Ldiff <- logLik(m3)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # Not significant
# 
# m4 <- glm(q1~Rank+Road_dist)
# Ldiff <- logLik(m4)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # Almost significant
# 
# m5 <- glm(q1~Rank+Road_dist+Rain_mean)
# Ldiff <- logLik(m5)-logLik(m4)  
# 1-pchisq(2*(Ldiff),1) # Rain not significant
# 
# #Best model (tentative inclusion of road): 
# m1 <- glm(q1~Rank)
# summary(m1)
# plot(q1~Rank,main="rollers q1 diversity")
# abline(m1)
# 
# # checking for colinearity
# require(stats)
# predictors <- stack(DB,select=c(Rank, Road_dist))
# multicol <- cor(getValues(predictors), use = "pairwise.complete.obs", method = "spearman")
# pairs(predictors)
# 
# ////////////////////////////////////////////////
# # Rollers abundance
# ////////////////////////////////////////////////
# rollers <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/rollers-countmat.csv",row.names = 1)
# 
# head(rollers)
# names(rollers) <- c("BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")
# 
# ab.roll <- colSums(rollers)
# ab.r <- rbind(rollers,"Abund"=ab.roll)
# tail(ab.r)
# 
# barplot(ab.roll,main="abundance of rollers")
# 
# m1 <- glm(Abund_roll~Rank)
# summary(m1)
# plot(Abund_roll~Rank)
# abline(m1)
# 
# m2 <- glm(Abund_tunn~Rank)
# summary(m2)
# plot(Abund_tunn~Rank)
# abline(m2)
# 
# m3 <- glm(Abund_total~Rank)
# summary(m3)
# plot(Abund_total~Rank)
# abline(m3)
# max(Abund_tunn)
# png(file="C://Data/PhD/Outputs/Dungbeetles/Diversity/Roller_Abund.png",width=5,height=3.25,units="in",res=180)  
# plot(Abund_roll~Rank,pch=21,bg="mediumseagreen",cex.main=0.9,cex=1.5,bty="n", cex.axis=0.7,cex.lab=0.8,
#      xlim=c(1,6),ylim=c(0,200),main="Rollers",ylab="Abundance",xlab="Disturbance rank")
# m4<- glm(Abund_roll~Rank,family=gaussian)
# abline(m4,col="mediumseagreen",lwd=2)
# dev.off()
# 
# png(file="C://Data/PhD/Outputs/Dungbeetles/Diversity/Tunnel_Abund.png",width=5,height=3.25,units="in",res=180)  
# plot(Abund_tunn~Rank,pch=21,bg="mediumseagreen",cex.main=0.9,cex=1.5,bty="n", cex.axis=0.7,cex.lab=0.8,
#      xlim=c(1,6),ylim=c(0,350),main="Tunnelers",ylab="Abundance",xlab="Disturbance rank")
# m4<- glm(Abund_tunn~Rank,family=gaussian)
# abline(m4,col="mediumseagreen",lwd=3)
# dev.off()
# 
# 
# sum(Abund_total)
# DB
# ////////////////////////////////////////////////
# # Tunnelers abundance
# ////////////////////////////////////////////////
# tunnels <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/tunnels-countmat.csv",row.names = 1)
# 
# head(tunnels)
# names(tunnels) <- c("BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")
# 
# ab.tun <- colSums(tunnels)
# ab.t <- rbind(tunnels,"Abund"=ab.tun)
# tail(ab.t)
# 
# barplot(ab.tun,main="abundance of tunnelers")
# 
# ab_total <- ab.roll+ab.tun
# barplot(ab_total,main="Overall abundance")
# 
# Abund <- as.data.frame(cbind(Site=names(tunnels),Abund_roll=ab.roll,Abund_tunn=ab.tun,Abund_total=ab_total))
# sum(Abund_total)
# 
# #write.csv(Abund,"C://Data/PhD/Processed_data/Dungbeetles/DB_Abund_groups.csv")
# ////////////////////////////////////////////////
# # Dung beetle diversity - split by rollers/tunnelers
# 
# # DB data TUNNELERS
# 
# rm(list=ls()) 
# require(dplyr)
# library(tidyr)
# library(stringr)
# 
# weather <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_weather.csv")
# veg <- read.csv("C://Data/PhD/Processed_data/Vegetation/veg_pca_out.csv")
# alpha <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/Diversity/alpha_tunnels.csv")
# site_data <- read.csv("C://Data/PhD/Processed_data/Site_data/sites_elev_dist_coords.csv")
# rich <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/ESoutput/Tunnels_ES_output.csv")
# abund <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_Abund_groups.csv")
# 
# head(abund)
# abund <- abund[,-1]
# head(weather)
# head(veg)
# head(alpha)
# head(site_data)
# head(rich)
# 
# # 
# str(rich) #get rid of some of the excess columns of richness output
# rem_rich <- c(8,9,10,11,12,13,14,15)#cols of richness ests to remove
# rich <- rich[,-rem_rich]
# 
# # Edited alpha file in Excel to separate Site_rank into rank and site columns
# # Also edited richness output to replace spaces with underscores and remove % and () from colnames
# colnames(weather)[1] <- "Site"
# colnames(veg)[1] <- "Site"
# colnames(rich)[1] <- "Site"
# 
# 
# alpha[,3] <- as.factor(alpha[,3])
# levels(alpha[,3])
# levels(alpha$Site)[levels(alpha$Site)=="MinD-B"] <- "MIN-B"
# levels(alpha$Site)[levels(alpha$Site)=="MinD-A"] <- "MIN-A"
# levels(alpha$Site)[levels(alpha$Site)=="MinD-C"] <- "MIN-C"
# 
# levels(weather$Site)[levels(weather$Site)=="T6-850"] <- "MIN-B"
# levels(weather$Site)[levels(weather$Site)=="T7-1350"] <- "MIN-A"
# levels(weather$Site)[levels(weather$Site)=="CH-400"] <- "MIN-C"
# levels(weather$Site)[levels(weather$Site)=="T1-650"] <- "CCR-B"
# levels(weather$Site)[levels(weather$Site)=="T2-800"] <- "CCR-A"
# levels(weather$Site)[levels(weather$Site)=="T5-250"] <- "CCR-C"
# levels(weather$Site)[levels(weather$Site)=="T2-2150"] <- "MXD-A"
# levels(weather$Site)[levels(weather$Site)=="T10-100"] <- "MXD-B"
# levels(weather$Site)[levels(weather$Site)=="T3-1800"] <- "MXD-C"
# 
# 
# 
# DB_merge1 <- merge(alpha,site_data,by="Site")
# DB_merge2 <- merge(DB_merge1,veg,by="Site")
# DB_merge3 <- merge(DB_merge2,weather,by="Site")
# DB_merge4 <- merge(DB_merge3,abund,by="Site")
# DB_allvars <- merge(DB_merge4,rich,by="Site")
# 
# #write.csv(DB_allvars,"C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars-TUNNELS.csv")
# 
# ////////////////////////////////
# # Models 
# ////////////////////////////###
# 
# DB <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars-TUNNELS.csv")
# 
# head(DB)
# str(DB)
# 
# DB$Rank <- as.numeric(DB$Rank)
# DB$q0 <- as.numeric(DB$q0)
# DB$River_dist <- as.numeric(DB$River_dist)
# DB$Road_dist <- as.numeric(DB$Road_dist)
# DB$Elevation <- as.numeric(DB$Elevation)
# DB$UTM_Coords_E <- as.numeric(DB$UTM_Coords_E)
# DB$UTM_Coords_S <- as.numeric(DB$UTM_Coords_S)
# DB$Rain_min <- as.numeric(DB$Rain_min)
# DB$Hum_max <- as.numeric(DB$Hum_max)
# DB$Hum_min <- as.numeric(DB$Hum_min)
# DB$Sest <- as.numeric(DB$Sest)
# 
# attach(DB)
# 
# m1 <- glm(Abund_roll~Rank)
# summary(m1)
# plot(Abund_tunn~Rank)
# abline(m1)
# 
# m1 <- glm(Abund_tunn~Rank)
# summary(m1)
# 
# # First test which of weather variables are most influential:
# 
# m1<- glm(q1~Temp19mean+Temp19sd +Hum19mean +Hum19sd +Rain_mean+ Rain_sd +Temp_max +Temp_min 
#          +Rain_max +Rain_min +Hum_max +Hum_min, family=gaussian)
# summary(m1)
# 
# 
# m2<- glm(q1~Temp19mean+Temp19sd +Hum19mean +Hum19sd +Rain_mean+ Rain_sd +Temp_max +Temp_min 
#          +Rain_max +Rain_min +Hum_max)
# summary(m2)
# 
# m3<- glm(q1~Temp19mean+Temp19sd +Hum19mean +Hum19sd +Rain_mean+ Rain_sd +Temp_max +Temp_min 
#          +Rain_max +Rain_min)
# 
# 
# # 
# m1 <- glm(q1~Rank)
# summary(m1) #Est=0.7 p=0.006
# plot(q1~Rank,main="Tunnelers diversity")
# abline(m1)
# 
