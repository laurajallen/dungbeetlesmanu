# DB data - combining all datasets for models
# Nov 2016

rm(list=ls())

# libraries ---
library(ape) # for moran's i 
 require(dplyr)
 library(tidyr)
 library(stringr)

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


####
####

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
# add equal sample size diversity estimates to the main data
# Est05<- read.csv("C://Data/PhD/Processed_data/Dungbeetles/db_inext_Ests_EqSSq05.csv")
# Est05 <- Est05[,2:3]
# names(Est05) <- c("Est05_ss","Site")
# DB <- merge(DB,Est05,by="Site")
#Ests_EqSS <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/db_inext_Ests_EqSS.csv")
#DB <- merge(DB,Ests_EqSS,by="Site")
#write.csv(DB,"C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars_EqSSests.csv")

# 

####
####
# Models ----

# before model, check for collinearity between variables, so there's no need to test all of them.

# Correlation tests ----
#weather
cor.test(DB$Temp19mean, DB$Temp19sd, method = "spearman",exact=F) 
cor.test(DB$Temp19mean, DB$Temp_max, method = "spearman",exact=F)
cor.test(DB$Temp19mean, DB$Temp_min, method = "spearman",exact=F) # all temperature variables are correlated - so just using mean

cor.test(DB$Rain_mean,DB$Rain_max, method = "spearman",exact=F)
cor.test(DB$Rain_mean,DB$Rain_sd, method = "spearman",exact=F) #rain all correlated, so using mean
cor.test(DB$Rain_mean,DB$Rain_min, method = "spearman",exact=F) # rain min= 0 so, not useful

cor.test(DB$Hum19mean,DB$Hum_max, method = "spearman",exact=F)
cor.test(DB$Hum19mean,DB$Hum_min, method = "spearman",exact=F)
cor.test(DB$Hum19mean,DB$Hum19sd, method = "spearman",exact=F)
cor.test(DB$Hum19sd,DB$Hum_max, method = "spearman",exact=F) # humidity all correlated, so only using mean

cor.test(DB$Hum19mean,DB$Temp19mean, method = "spearman",exact=F)
cor.test(DB$Hum19mean,DB$Rain_mean, method = "spearman",exact=F)
cor.test(DB$Rain_mean,DB$Temp19mean, method = "spearman",exact=F)
## Temp, humidity and rainfall are alll correlated - so probably should just use one of them

cor.test(DB$Rank,DB$Road_dist, method = "spearman",exact=F)
cor.test(DB$Rank,DB$River_dist, method = "spearman",exact=F)
cor.test(DB$Rank,DB$Elevation, method = "spearman",exact=F)
# Road and elevation are correlated with rank -  is this a problem?

cor.test(DB$Rain_mean,DB$q0,method = "spearman",exact=F)
cor.test(DB$Hum19mean,DB$q0,method = "spearman",exact=F)



# ploting weatehr vars to see which is more likely to 
# be influential - only using one because highly correlated.
plot(q0~Rain_mean,data=DB)
plot(q0~Temp19mean,data=DB)
plot(q0~Hum19mean,data=DB)

# strongest correlation from rain
cor.test(DB$Rain_mean,DB$q0,method = "spearman",exact=F)
cor.test(DB$Hum19mean,DB$q0,method = "spearman",exact=F)
cor.test(DB$Temp19mean,DB$q0,method = "spearman",exact=F)


# Moran's i test for spatial autocorrelation
morans <- function(model){
  resids <- model$residuals 
  resdf <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
  DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
  DB.dists.inv <- 1/DB.dists # inverse dist matrix
  diag(DB.dists.inv) <- 0 # replace diagonal with 0s
  DB.dists.inv[1:5, 1:5] # view
  as.data.frame(Moran.I(resdf$resids, DB.dists.inv))}

## Checklist of variables to test:
# rain              - Rain_mean
# temp              - Temp19mean
# humidity          - Hum19mean
# distance to river - River_dist
# elevation         - Elevation 
# distance to road  - Road_dist
# rank              - Rank
####

# lm(diversity~rank) ----
## Paul recommended using spearman rank correlation test instead of glm
# CORTEST FUNTION ----
#create correlation test function incl. nice output and bootstrap
docor <- function(ia,ib){
  res <- cor.test(ia, ib, method = "spearman",exact=F) 
  ci <- quantile(
    replicate(10000, {
      boot.i <- sample(length(ia), replace = TRUE)
      cor(ia[boot.i], ib[boot.i], method = "spearman")
    }), 
    c(0.025, 0.975))
  cor <- cbind(res$statistic,res$p.value,res$estimate,ci[1],ci[2])
  colnames(cor) <- c("S","p","rho","LCI","UCI")
  print(cor)
}
docor(DB$Rank,DB$Est1_ss)
# I plan to use the correlation test to shpw a relationship, and back up with glms to rule out effetc of other vars.
# rank correlation (more general, so less likely to be the wrong model)
res <-cor.test(DB$Rank, DB$q0, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
cbind(res$statistic,res$p.value,res$estimate)
# spearman calc uses ties corrected formula I believe.
# moderately strong correlation between shannon diversity and rank, statistically significant

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$q0[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
### q=0 ----
m0 <- lm(log(q0)~Rank,data=DB)
logLik(m0)

m1 <- lm(log(q0)~Rank+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1) # for lm model is fitted using max likelihood (AIC), and default return of logLik is REML=F, so this is fine. For lmer, model default is fitted using REML, so need to specify this as F instead to get true max likelihood.

m2 <- lm(log(q0)~Rank+River_dist,data=DB) 
anova(m0,m2) #0.9502
logLik(m2)

m3 <- lm(log(q0)~Rank+Rain_mean,data=DB) 
anova(m0,m3) # 0.6395

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

####

####
####

### q=1 ----
res <- cor.test(DB$Rank, DB$q1, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$q1[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))



m0 <- lm(log(q1)~Rank,data=DB)
logLik(m0)
summary(m0)

m1 <- lm(log(q1)~Rank+Elevation,data=DB) 
anova(m0,m1) #0.8111
logLik(m1) # for lm model is fitted using max likelihood (AIC), and default return of logLik is REML=F, so this is fine. For lmer, model default is fitted using REML, so need to specify this as F instead to get true max likelihood.

m2 <- lm(log(q1)~Rank+River_dist,data=DB) 
anova(m0,m2) #0.09172
logLik(m2)

m3 <- lm(log(q1)~Rank+Rain_mean,data=DB) 
anova(m0,m3) # 0.2766
logLik(m3)


### best:
rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

####

###
### q=2 ----
res <- cor.test(DB$Rank, DB$q2, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$q2[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(q2)~Rank,data=DB)
logLik(m0)

m1 <- lm(log(q2)~Rank+Elevation,data=DB) 
anova(m0,m1) #0.8111
logLik(m1) # for lm model is fitted using max likelihood (AIC), and default return of logLik is REML=F, so this is fine. For lmer, model default is fitted using REML, so need to specify this as F instead to get true max likelihood.

m2 <- lm(log(q2)~Rank+River_dist,data=DB) 
anova(m0,m2) #0.09172
logLik(m2)


m3 <- lm(log(q2)~Rank+Rain_mean,data=DB) 
anova(m0,m3) # 0.2766

### best:
rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

### q=3 ----
res <- cor.test(DB$Rank, DB$q3, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$q3[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(q3)~Rank,data=DB)
logLik(m0)

m1 <- lm(log(q3)~Rank+Elevation,data=DB) 
anova(m0,m1) #0.8111
logLik(m1) # for lm model is fitted using max likelihood (AIC), and default return of logLik is REML=F, so this is fine. For lmer, model default is fitted using REML, so need to specify this as F instead to get true max likelihood.

m2 <- lm(log(q3)~Rank+River_dist,data=DB) 
anova(m0,m2) #0.09172
logLik(m2)

m3 <- lm(log(q3)~Rank+Rain_mean,data=DB)
anova(m0,m3) #0.1171
logLik(m3)


### best:
rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)



### q=Inf ----
res <- cor.test(DB$Rank, DB$qInf, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$qInf[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


m0 <- lm(log(qInf)~Rank,data=DB)
logLik(m0)

m1 <- lm(log(qInf)~Rank+Elevation,data=DB) 
anova(m0,m1) #0.8111
logLik(m1) # for lm model is fitted using max likelihood (AIC), and default return of logLik is REML=F, so this is fine. For lmer, model default is fitted using REML, so need to specify this as F instead to get true max likelihood.

m2 <- lm(log(qInf)~Rank+River_dist,data=DB) 
anova(m0,m2) #0.09172
logLik(m2)

m3 <- lm(log(qInf)~Rank+Rain_mean,data=DB) 
anova(m0,m3) #0.1171
logLik(m3)

### best:
rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

####
### Vegetation ----
# Are vegetation featurs correlated with disturbance rank?
cor.test(DB$Rank, veg_pc1, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$veg_pc1[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# LMs
m0 <- lm(veg_pc1~Rank,data=DB)
logLik(m0)
summary(m0)
# Unsurprisingly, yes. V. low p, high Estimate and big decrease in residual deviance

m1 <- lm(veg_pc1~Rank+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(veg_pc1~Rank+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(veg_pc1~Rank+Road_dist,data=DB) 
anova(m0,m3) 
logLik(m3)

m4 <- lm(veg_pc1~Rank+Rain_mean,data=DB) 
anova(m0,m4) 
logLik(m4)

# Moran's i test for spatial autocorrelation
resids <- m0$residuals
vegmoran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(vegmoran$resids, DB.dists.inv)

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

#  q0 est ----
# (95% sample completeness)
res <- cor.test(DB$Rank, DB$e95q0, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$e95q0[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# LMs
m0 <- lm(log(e95q0)~Rank,data=DB)
logLik(m0)
summary(m0)

m1 <- lm(log(e95q0)~Rank+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(e95q0)~Rank+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(e95q0)~Rank+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

m4 <- lm(log(e95q0)~Rank+Rain_mean,data=DB) 
anova(m0,m4) 
logLik(m4)

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

#  q1 est ----
# (at asymptote - changed to 95%SC)
res <- cor.test(DB$Rank, DB$e95q1, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$e95q1[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# LMs
m0 <- lm(log(e95q1)~Rank,data=DB)
logLik(m0)
summary(m0)

m1 <- lm(log(e95q1)~Rank+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(e95q1)~Rank+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(e95q1)~Rank+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

m4 <- lm(log(e95q1)~Rank+Rain_mean,data=DB) 
anova(m0,m4) 
logLik(m4)

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

#  q2 est ----
# (at asymptote)
res <- cor.test(DB$Rank, DB$e95q2, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$q2[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# LMs
m0 <- lm(log(e95q2)~Rank,data=DB)
logLik(m0)
summary(m0)

m1 <- lm(log(e95q2)~Rank+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(e95q2)~Rank+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(e95q2)~Rank+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

m4 <- lm(log(e95q2)~Rank+Rain_mean,data=DB) 
anova(m0,m4) 
logLik(m4)

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

#  q3 est ----
# (at 95% completeness)
res <- cor.test(DB$Rank, DB$e95q3, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$e95q3[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# LMs
m0 <- lm(log(e95q3)~Rank,data=DB)
logLik(m0)
summary(m0)

m1 <- lm(log(e95q3)~Rank+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1) 

m2 <- lm(log(e95q3)~Rank+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(e95q3)~Rank+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

m4 <- lm(log(e95q3)~Rank+Rain_mean,data=DB) 
anova(m0,m4) 
logLik(m4)

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

## Estimated diversity at equal sample sizes (300) ----
DB <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars_EqSSests.csv")
docor(DB$Est0_ss,DB$Rank)
docor(DB$Est1_ss,DB$Rank)
docor(DB$Est2_ss,DB$Rank)

# q0

m0 <- lm(log(Est0_ss)~Rank,data=DB)

m1 <- lm(log(Est0_ss)~Rank+Elevation,data=DB) 
anova(m0,m1) 

m2 <- lm(log(Est0_ss)~Rank+River_dist,data=DB) 
anova(m0,m2) 

m3 <- lm(log(Est0_ss)~Rank+Rain_mean,data=DB) 
anova(m0,m3) 

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

# q1

m0 <- lm(log(Est1_ss)~Rank,data=DB)

m1 <- lm(log(Est1_ss)~Rank+Elevation,data=DB) 
anova(m0,m1) 

m2 <- lm(log(Est1_ss)~Rank+River_dist,data=DB) 
anova(m0,m2) 

m3 <- lm(log(Est1_ss)~Rank+Rain_mean,data=DB) 
anova(m0,m3) 

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

# q2

m0 <- lm(log(Est2_ss)~Rank,data=DB)

m1 <- lm(log(Est2_ss)~Rank+Elevation,data=DB) 
anova(m0,m1) 

m2 <- lm(log(Est2_ss)~Rank+River_dist,data=DB) 
anova(m0,m2) 

m3 <- lm(log(Est2_ss)~Rank+Rain_mean,data=DB) 
anova(m0,m3) 

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)
morans(m0)

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
### Old code I don't want to delete just yet ----
# #Diversity ~ environment
# 
# ###
# ## q=1
# ###
# 
# # model q1 against environmental variables
# 
# # tested model fit (tried gaussian, gamma and inverse gaussian)
# m0 <- glm(log(q1)~1, family=gaussian)
# m1<- glm(log(q1)~Rank, family=gaussian)
# #Gamma(link="identity")) # inverse.gaussian(link = "1/mu^2"))
# 
# 
# #1-pchisq(m1$deviance,m1$df.residual) # goodness of fit. >0.05 or chosen sig. level,
# # means that there is no evidence contradicting the model assumption of
# # the distribution chosen (so all ok if p>0.05). Gaussian comes out with 
# # v low p value, but both gamma and invgaussian come out high.
# 
# ### Testing all posisble variables
# 
# #
# m2<- glm(log(q1)~Rank+River_dist, family=gaussian)
# summary(m2)
# anova(m2,m1)
# Ldiff <- logLik(m2)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.09581511 (df=4) - reject River_dist
# 
# #
# m3<- glm(log(q1)~Rank+Road_dist, family=gaussian)
# summary(m3)
# anova(m3,m1)
# Ldiff <- logLik(m3)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 'log Lik.' 0.01960956 (df=4) - Road_dist improves model
# 
# cor.test(Rank, Road_dist, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
# # BUT road_dist highly correl;ated with rank, so will not include it (the most disturbed sites are cloesest to the road)
# 
# #
# m4<- glm(log(q1)~Rank+Elevation, family=gaussian)
# summary(m4)
# anova(m4,m1)
# Ldiff <- logLik(m4)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 'log Lik.'0.976748 (df=4) - reject elevation
# 
# 
# #
# m5<- glm(log(q1)~Rank+Rain_mean, family=gaussian)
# summary(m5)
# anova(m5,m1)
# Ldiff <- logLik(m5)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 'log Lik.'0.2952313 (df=4) - reject rain
# 
# #
# m6<- glm(log(q1)~Rank+Elevation+Rank*Elevation, family=gaussian)
# summary(m6)
# anova(m6,m4)
# Ldiff <- logLik(m6)-logLik(m4) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 'log Lik.'0.6360697 (df=5) - reject interaction rank*elevation
# 
# #
# m7<- glm(log(q1)~Rank+River_dist+Rank*River_dist, family=gaussian)
# summary(m7)
# anova(m7,m2)
# Ldiff <- logLik(m7)-logLik(m2) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 'log Lik.'0.6297465 (df=5) - reject interaction rank*river_dist
# 
# #
# m8<- glm(log(q1)~Rank+River_dist+Elevation, family=gaussian)
# summary(m8)
# #
# m9<- glm(log(q1)~Rank+River_dist+Elevation+River_dist*Elevation, family=gaussian)
# summary(m9)
# anova(m9,m8)
# Ldiff <- logLik(m9)-logLik(m8) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 'log Lik.' 0.3990317 (df=6) - reject interaction elevation*river_dist
# 
# 
# #
# m10<- glm(log(q1)~Rank+Rain_mean+Elevation, family=gaussian)
# summary(m10)
# #
# m11<- glm(log(q1)~Rank+Rain_mean+Elevation+Rain_mean*Elevation, family=gaussian)
# summary(m11)
# anova(m11,m10)
# Ldiff <- logLik(m11)-logLik(m10) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 'log Lik.'  0.5822872 (df=6) - reject interaction elevation*rain
# 
# ###
# 
# # best model 
# m1 <- glm(log(q1)~Rank, family=gaussian)
# summary(m1) # est.coeff Rank: 0.10839   p=0.0533 .
# par(mfrow=c(2,2))
# plot(m1)
# par(mfrow=c(1,1))
# plot(m1$fitted.values,m1$residuals)
# plot(exp(m1$fitted.values),q1,xlim=c(2,13),ylim=c(2,13))
# # fitted values slightly underestimate diversity at high q1
# 
# # Plot data points (q1~Rank) and fitted model with confidence interval
# library(visreg)
# visreg(m1)
# preds <- predict(m1,type="response",se.fit=T)
# # because there are 3x the same prediction (because 3x same rank), I am going to 
# # just use every third point for creating the lines
# pfits <- preds$fit[c(1,4,7,10,13,16)]
# pse <- preds$se.fit[c(1,4,7,10,13,16)]
# UCI <- pfits+(1.96*pse)
# LCI <- pfits-(1.96*pse)
# 
# 
# ###
# ## Paul recommended using spearman rank correlation test instead of glm
# 
# # I plan to use the glm's to demonstrate the lack of effect from any confounding variables, 
# # backed up with the correlation test
# 
# # rank correlation (more general, so less likely to be the wrong model)
# cor.test(Rank, q1, method = "spearman",exact=F) # used exact = F to supress warning about ties - this 
# # spearman calc uses ties corrected formula I believe.
# # moderately strong correlation between shannon diversity and rank, statistically significant
# # S = 473.83, p-value = 0.03021  rho 0.511012 
# 
# 
# # bootstrapped 95% CI
# quantile(
#   replicate(10000, {
#     boot.i <- sample(length(Rank), replace = TRUE)
#     cor(Rank[boot.i], q1[boot.i], method = "spearman")
#     }), 
#   c(0.025, 0.975))
# #   2.5%      97.5% 
# #  0.09462049 0.76995471 
# 
# 
# ### added based on stack exchange advice:
# # permutation test
# test <- function(q1) suppressWarnings(cor.test(Rank, q1, method="spearman")$estimate)
# rho <- test(q1)                                     # Test statistic
# p <- replicate(10^3, test(sample(q1, length(q1))))   # Simulated permutation distribution
# 
# p.out <- sum(abs(p) > rho)    # Count of strict (absolute) exceedances
# p.at <- sum(abs(p) == rho)    # Count of equalities, if any
# (p.out + p.at /2) / length(p) # Proportion of exceedances: the p-value.
# 
# # Use a permutation test. You only need to permute one of the variables independently 
# # of the other; here, the response is permuted. Because the relationship in the example
# # is strong, only a small number of permutations are needed (1000 in the example below).
# # 
# # As always, the actual statistic is compared to the distribution of permuted statistics.
# # The p-value is the estimate of the tail probability of the permutation distribution 
# # relative to the actual statistic. In some cases the test statistic has a discrete 
# # distribution, so it's wise to check the frequencies with which (a) the permutation 
# # statistics strictly exceed the actual statistic and (b) the permutation statistics 
# # equal or exceed the actual statistic. The code illustrates this by splitting the difference.
# 
# ###
# 
# # Moran's i test for spatial autocorrelation
# 
# library(ape)
# resids <- m1$residuals
# q1moran <- cbind.data.frame(Site,resids,UTM_Coords_S,UTM_Coords_E)
# DB.dists <- as.matrix(dist(cbind(UTM_Coords_E,UTM_Coords_S))) # create distance matrix of site coordinates
# DB.dists.inv <- 1/DB.dists # inverse dist matrix
# diag(DB.dists.inv) <- 0 # replace diagonal with 0s
# DB.dists.inv[1:5, 1:5] # view
# Moran.I(q1moran$resids, DB.dists.inv) # caluclate moran's i
# # $observed
# # [1] 0.02236334
# # $expected
# # [1] -0.05882353
# # $sd
# # [1] 0.09064366
# # $p.value
# # [1] 0.3704287 # No significant spatial autocorrelation in residuals (null hypothesis of no spatial correlation)
# 
# 
# 
# 
# ### 
# #q=0
# ###
# 
# # model q0 against environmental variables
# 
# # tested model fit (tried gaussian, gamma and inverse gaussian)
# 
# m1<- glm(log(q0)~Rank, family=gaussian)
# 
# ### Testing all possible variables
# 
# #
# m2<- glm(log(q0)~Rank+River_dist, family=gaussian)
# summary(m2)
# anova(m2,m1)
# Ldiff <- logLik(m2)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'0.9123452 (df=4) - reject River_dist
# 
# #
# m3<- glm(log(q0)~Rank+Road_dist, family=gaussian)
# summary(m3)
# anova(m3,m1)
# Ldiff <- logLik(m3)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.6120657 (df=4) - reject Road_dist
# 
# #
# m4<- glm(log(q0)~Rank+Elevation, family=gaussian)
# summary(m4)
# anova(m4,m1)
# Ldiff <- logLik(m4)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.1465056 (df=4) - reject elevation
# 
# 
# #
# m5<- glm(log(q0)~Rank+Rain_mean, family=gaussian)
# summary(m5)
# anova(m5,m1)
# Ldiff <- logLik(m5)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.5590096 (df=4) - reject rain
# 
# #
# m6<- glm(log(q0)~Rank+Elevation+Rank*Elevation, family=gaussian)
# summary(m6)
# anova(m6,m4)
# Ldiff <- logLik(m6)-logLik(m4) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 'log Lik.' 0.8058577 (df=5) - reject interaction rank*elevation
# 
# #
# m7<- glm(log(q0)~Rank+River_dist+Rank*River_dist, family=gaussian)
# summary(m7)
# anova(m7,m2)
# Ldiff <- logLik(m7)-logLik(m2) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' ' 0.6959888 (df=5) - reject interaction rank*river_dist
# 
# #
# m8<- glm(log(q0)~Rank+River_dist+Elevation, family=gaussian)
# summary(m8)
# #
# m9<- glm(log(q0)~Rank+River_dist+Elevation+River_dist*Elevation, family=gaussian)
# summary(m9)
# anova(m9,m8)
# Ldiff <- logLik(m9)-logLik(m8) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' '0.6718668 (df=6)- reject interaction elevation*river_dist
# 
# 
# #
# m10<- glm(log(q0)~Rank+Rain_mean+Elevation, family=gaussian)
# summary(m10)
# #
# m11<- glm(log(q0)~Rank+Rain_mean+Elevation+Rain_mean*Elevation, family=gaussian)
# summary(m11)
# anova(m11,m10)
# Ldiff <- logLik(m11)-logLik(m10) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.6455767 (df=6) - reject interaction elevation*rain
# 
# #incase of autocorrelation
# m12 <- glm(log(q0)~Rank+UTM_Coords_E*UTM_Coords_S, family=gaussian)
# summary(m12)
# anova(m12,m1)
# Ldiff <- logLik(m12)-logLik(m1) 
# 1-pchisq(2*(Ldiff),3) #0.7511383 (df=6)
# 
# ###
# # best model 
# m1 <- glm(log(q0)~Rank, family=gaussian)
# summary(m1) # est.coeff Rank:  0.09935  p=0.00126 **
# par(mfrow=c(2,2))
# plot(m1)
# par(mfrow=c(1,1))
# plot(m1$fitted.values,m1$residuals)
# plot(exp(m1$fitted.values),q0,xlim=c(min(q0)-1,max(q0)-1))
# # fitted values slightly underestimate diversity at high q0
# 
# # Plot data points (q0~Rank) and fitted model with confidence interval
# visreg(m1)
# 
# 
# ###
# #spearman rank correlation test 
# cor.test(Rank, q0, method = "spearman",exact=F) 
# # S = 259.35, p-value = 0.0005482  rho 0.7323569 - strong correlation
# 
# # bootstrapped 95% CI
# quantile(
#   replicate(10000, {
#     boot.i <- sample(length(Rank), replace = TRUE)
#     cor(Rank[boot.i], q0[boot.i], method = "spearman")
#   }), 
#   c(0.025, 0.975))
# #   2.5%      97.5% 
# #  0.3727193 0.9480657
# 
# 
# ### added based on stack exchange advice:
# # permutation test
# test <- function(q0) suppressWarnings(cor.test(Rank, q0, method="spearman")$estimate)
# rho <- test(q0)                                     # Test statistic
# p <- replicate(10^3, test(sample(q0, length(q0))))   # Simulated permutation distribution
# 
# p.out <- sum(abs(p) > rho)    # Count of strict (absolute) exceedances
# p.at <- sum(abs(p) == rho)    # Count of equalities, if any
# (p.out + p.at /2) / length(p) # Proportion of exceedances: the p-value.
# 
# ###
# 
# # Moran's i test for spatial autocorrelation
# resids <- m1$residuals
# q0moran <- cbind.data.frame(Site,resids,UTM_Coords_S,UTM_Coords_E)
# DB.dists <- as.matrix(dist(cbind(UTM_Coords_E,UTM_Coords_S))) # create distance matrix of site coordinates
# DB.dists.inv <- 1/DB.dists # inverse dist matrix
# diag(DB.dists.inv) <- 0 # replace diagonal with 0s
# DB.dists.inv[1:5, 1:5] # view
# Moran.I(q0moran$resids, DB.dists.inv) 
# # $observed
# # [1] -0.2359096
# # 
# # $expected
# # [1] -0.05882353
# # 
# # $sd
# # [1] 0.09264835
# # 
# # $p.value
# # [1] 0.05595596 #  slightly worrying!
# 
# 
# 
# ###
# #
# #
# ###
# ###
# ## q=2
# ###
# 
# # model q2 against environmental variables
# 
# # tested model fit (tried gaussian, gamma and inverse gaussian)
# 
# m1<- glm(log(q2)~Rank, family=gaussian)
# summary(m1)
# 
# ### Testing all possible variables
# #
# m2<- glm(log(q2)~Rank+River_dist, family=gaussian)
# summary(m2)
# anova(m2,m1)
# Ldiff <- logLik(m2)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'0.12719 (df=4) - reject River_dist
# 
# #
# m3<- glm(log(q2)~Rank+Road_dist, family=gaussian)
# summary(m3)
# anova(m3,m1)
# Ldiff <- logLik(m3)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.03173669 (df=4) - Road_dist improves model but is autocorrelates with rank
# 
# #
# m4<- glm(log(q2)~Rank+Elevation,family=gaussian)
# summary(m4)
# anova(m4,m1)
# Ldiff <- logLik(m4)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.6316607 (df=4) - reject elevation
# 
# #
# m5<- glm(log(q2)~Rank+Rain_mean, family=gaussian)
# summary(m5)
# anova(m5,m1)
# Ldiff <- logLik(m5)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.2322678 (df=4) - reject rain
# 
# #
# m6<- glm(log(q2)~Rank+Elevation+Rank*Elevation, family=gaussian)
# summary(m6)
# anova(m6,m4)
# Ldiff <- logLik(m6)-logLik(m4) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.5658446 (df=5) - reject interaction rank*elevation
# 
# #
# m7<- glm(log(q2)~Rank+River_dist+Rank*River_dist, family=gaussian)
# summary(m7)
# anova(m7,m2)
# Ldiff <- logLik(m7)-logLik(m2) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.5361314 (df=5) - reject interaction rank*river_dist
# 
# #
# m8<- glm(log(q2)~Rank+River_dist+Elevation, family=gaussian)
# summary(m8)
# #
# m9<- glm(log(q2)~Rank+River_dist+Elevation+River_dist*Elevation, family=gaussian)
# summary(m9)
# anova(m9,m8)
# Ldiff <- logLik(m9)-logLik(m8) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' ' 0.3874617 (df=6) reject interaction elevation*river_dist
# 
# 
# #
# m10<- glm(log(q2)~Rank+Rain_mean+Elevation, family=gaussian)
# summary(m10)
# #
# m11<- glm(log(q2)~Rank+Rain_mean+Elevation+Rain_mean*Elevation, family=gaussian)
# summary(m11)
# anova(m11,m10)
# Ldiff <- logLik(m11)-logLik(m10) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 00.3267499 (df=6) - reject interaction elevation*rain
# 
# ###
# # best model 
# m1q2 <- glm(log(q2)~Rank, family=gaussian)
# summary(m1) # est.coeff Rank:   0.07033  p=0.286 # NO signifanct effect of rank on q2
# par(mfrow=c(2,2))
# plot(m1)
# par(mfrow=c(1,1))
# plot(m1$fitted.values,m1$residuals)
# plot(exp(m1$fitted.values),q2,xlim=c(min(q2)-1,max(q2)-1))
# # fitted values slightly underestimate diversity at high q2
# 
# # Plot data points (q2~Rank) and fitted model with confidence interval
# visreg(m1q2)
# 
# 
# ###
# #spearman rank correlation test 
# cor.test(Rank, q2, method = "spearman",exact=F) 
# # S = 704.71, p-value = 0.2735 rho 0.2727487 - no significant correlation
# 
# # bootstrapped 95% CI
# quantile(
#   replicate(10000, {
#     boot.i <- sample(length(Rank), replace = TRUE)
#     cor(Rank[boot.i], q2[boot.i], method = "spearman")
#   }), 
#   c(0.025, 0.975))
# #   2.5%      97.5% 
# #  -0.2170656  0.6550296  crosses 0
# 
# 
# ### added based on stack exchange advice:
# # permutation test
# test <- function(q2) suppressWarnings(cor.test(Rank, q2, method="spearman")$estimate)
# rho <- test(q2)                                     # Test statistic
# p <- replicate(10^3, test(sample(q2, length(q2))))   # Simulated permutation distribution
# 
# p.out <- sum(abs(p) > rho)    # Count of strict (absolute) exceedances
# p.at <- sum(abs(p) == rho)    # Count of equalities, if any
# (p.out + p.at /2) / length(p) # Proportion of exceedances: the p-value.
# # p= 0.2515
# 
# ###
# 
# # Moran's i test for spatial autocorrelation
# resids <- m1$residuals
# q2moran <- cbind.data.frame(Site,resids,UTM_Coords_S,UTM_Coords_E)
# DB.dists <- as.matrix(dist(cbind(UTM_Coords_E,UTM_Coords_S))) # create distance matrix of site coordinates
# DB.dists.inv <- 1/DB.dists # inverse dist matrix
# diag(DB.dists.inv) <- 0 # replace diagonal with 0s
# DB.dists.inv[1:5, 1:5] # view
# Moran.I(q2moran$resids, DB.dists.inv) 
# # $observed
# # [1] -0.02878403
# # 
# # $expected
# # [1] -0.05882353
# # 
# # $sd
# # [1] 0.09124388
# # 
# # $p.value
# # [1] 0.7419878 - no spatial autocorrelation
# 
# 
# ###
# ###
# ## q=3
# ###
# 
# # model q3 against environmental variables
# 
# # tested model fit (tried gaussian, gamma and inverse gaussian)
# 
# m1<- glm(log(q3)~Rank, family=gaussian)
# summary(m1)
# 
# ### Testing all possible variables
# 
# #
# m2<- glm(log(q3)~Rank+River_dist, family=gaussian)
# summary(m2)
# anova(m2,m1)
# Ldiff <- logLik(m2)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'0.1611487 (df=4) - reject River_dist
# 
# #
# m3<- glm(log(q3)~Rank+Road_dist, family=gaussian)
# summary(m3)
# anova(m3,m1)
# Ldiff <- logLik(m3)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.04329291 (df=4) - Road_dist improves model but is autocorrelates with rank
# 
# #
# m4<- glm(log(q3)~Rank+Elevation, family=gaussian)
# summary(m4)
# anova(m4,m1)
# Ldiff <- logLik(m4)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.5455594 (df=4) - reject elevation
# 
# 
# #
# m5<- glm(log(q3)~Rank+Rain_mean, family=gaussian)
# summary(m5)
# anova(m5,m1)
# Ldiff <- logLik(m5)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.2497936 (df=4) - reject rain
# 
# #
# m6<- glm(log(q3)~Rank+Elevation+Rank*Elevation, family=gaussian)
# summary(m6)
# anova(m6,m4)
# Ldiff <- logLik(m6)-logLik(m4) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.5665957 (df=5) - reject interaction rank*elevation
# 
# #
# m7<- glm(log(q3)~Rank+River_dist+Rank*River_dist, family=gaussian)
# summary(m7)
# anova(m7,m2)
# Ldiff <- logLik(m7)-logLik(m2) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.4943849 (df=5)- reject interaction rank*river_dist
# 
# #
# m8<- glm(log(q3)~Rank+River_dist+Elevation, family=gaussian)
# summary(m8)
# #
# m9<- glm(log(q3)~Rank+River_dist+Elevation+River_dist*Elevation, family=gaussian)
# summary(m9)
# anova(m9,m8)
# Ldiff <- logLik(m9)-logLik(m8) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' ' 0.3795459 (df=6) reject interaction elevation*river_dist
# 
# 
# #
# m10<- glm(log(q3)~Rank+Rain_mean+Elevation, family=gaussian)
# summary(m10)
# #
# m11<- glm(log(q3)~Rank+Rain_mean+Elevation+Rain_mean*Elevation, family=gaussian)
# summary(m11)
# anova(m11,m10)
# Ldiff <- logLik(m11)-logLik(m10) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.2721137 (df=6)- reject interaction elevation*rain
# 
# ###
# # best model 
# m1q3 <- glm(log(q3)~Rank, family=gaussian)
# summary(m1q3) # est.coeff Rank:   0.07033  p=0.286 # NO signifanct effect of rank on q3
# par(mfrow=c(2,2))
# plot(m1)
# par(mfrow=c(1,1))
# plot(m1$fitted.values,m1$residuals)
# plot(exp(m1$fitted.values),q3,xlim=c(min(q3)-1,max(q3)-1))
# # fitted values slightly underestimate diversity at high q3
# 
# # Plot data points (q3~Rank) and fitted model with confidence interval
# visreg(m1q3)
# 
# 
# ###
# #spearman rank correlation test 
# cor.test(Rank, q3, method = "spearman",exact=F) 
# # S = 771.54, p-value = 0.4173 rho 0.2037778 - no significant correlation
# 
# # bootstrapped 95% CI
# quantile(
#   replicate(10000, {
#     boot.i <- sample(length(Rank), replace = TRUE)
#     cor(Rank[boot.i], q3[boot.i], method = "spearman")
#   }), 
#   c(0.025, 0.975))
# #   2.5%      97.5% 
# #  -0.2738419  0.6025073
# 
# ### added based on stack exchange advice:
# # permutation test
# test <- function(q3) suppressWarnings(cor.test(Rank, q3, method="spearman")$estimate)
# rho <- test(q3)                                     # Test statistic
# p <- replicate(10^3, test(sample(q3, length(q3))))   # Simulated permutation distribution
# 
# p.out <- sum(abs(p) > rho)    # Count of strict (absolute) exceedances
# p.at <- sum(abs(p) == rho)    # Count of equalities, if any
# (p.out + p.at /2) / length(p) # Proportion of exceedances: the p-value.
# #  0.4125
# 
# ###
# ###
# 
# # Moran's i test for spatial autocorrelation
# resids <- m1q3$residuals
# q3moran <- cbind.data.frame(Site,resids,UTM_Coords_S,UTM_Coords_E)
# DB.dists <- as.matrix(dist(cbind(UTM_Coords_E,UTM_Coords_S))) # create distance matrix of site coordinates
# DB.dists.inv <- 1/DB.dists # inverse dist matrix
# diag(DB.dists.inv) <- 0 # replace diagonal with 0s
# DB.dists.inv[1:5, 1:5] # view
# Moran.I(q3moran$resids, DB.dists.inv) 
# # $observed
# # [1] -0.03815589
# # 
# # $expected
# # [1] -0.05882353
# # 
# # $sd
# # [1] 0.09216267
# # 
# # $p.value
# # [1] 0.8225614
# 
# ###
# ###
# ## q=Inf
# ###
# 
# # model qInf against environmental variables
# 
# # tested model fit (tried gaussian, gamma and inverse gaussian)
# 
# m1<- glm(log(qInf)~Rank, family=gaussian)
# summary(m1)
# 
# ### Testing all possible variables
# 
# #
# m2<- glm(log(qInf)~Rank+River_dist, family=gaussian)
# summary(m2)
# anova(m2,m1)
# Ldiff <- logLik(m2)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.2354246 (df=4) - reject River_dist
# 
# #
# m3<- glm(log(qInf)~Rank+Road_dist, family=gaussian)
# summary(m3)
# anova(m3,m1)
# Ldiff <- logLik(m3)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.0472482 (df=4) - Road_dist improves model but is autocorrelates with rank
# 
# #
# m4<- glm(log(qInf)~Rank+Elevation, family=gaussian)
# summary(m4)
# anova(m4,m1)
# Ldiff <- logLik(m4)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.5408907 (df=4) - reject elevation
# 
# 
# #
# m5<- glm(log(qInf)~Rank+Rain_mean, family=gaussian)
# summary(m5)
# anova(m5,m1)
# Ldiff <- logLik(m5)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'0.2989462 (df=4) - reject rain
# 
# #
# m6<- glm(log(qInf)~Rank+Elevation+Rank*Elevation, family=gaussian)
# summary(m6)
# anova(m6,m4)
# Ldiff <- logLik(m6)-logLik(m4) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.5940544 (df=5)- reject interaction rank*elevation
# 
# #
# m7<- glm(log(qInf)~Rank+River_dist+Rank*River_dist, family=gaussian)
# summary(m7)
# anova(m7,m2)
# Ldiff <- logLik(m7)-logLik(m2) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.4391611 (df=5)- reject interaction rank*river_dist
# 
# #
# m8<- glm(log(qInf)~Rank+River_dist+Elevation, family=gaussian)
# summary(m8)
# #
# m9<- glm(log(qInf)~Rank+River_dist+Elevation+River_dist*Elevation, family=gaussian)
# summary(m9)
# anova(m9,m8)
# Ldiff <- logLik(m9)-logLik(m8) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' '  0.3430887 (df=6) reject interaction elevation*river_dist
# 
# 
# #
# m10<- glm(log(qInf)~Rank+Rain_mean+Elevation, family=gaussian)
# summary(m10)
# #
# m11<- glm(log(qInf)~Rank+Rain_mean+Elevation+Rain_mean*Elevation, family=gaussian)
# summary(m11)
# anova(m11,m10)
# Ldiff <- logLik(m11)-logLik(m10) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.2498547 (df=6) - reject interaction elevation*rain
# 
# ###
# # best model 
# m1qInf <- glm(log(qInf)~Rank, family=gaussian)
# summary(m1qInf) # est.coeff Rank:   0.02477 p=0.627595 # NO signifanct effect of rank on qInf
# par(mfrow=c(2,2))
# plot(m1)
# par(mfrow=c(1,1))
# plot(m1$fitted.values,m1$residuals)
# plot(exp(m1$fitted.values),qInf,xlim=c(min(qInf)-1,max(qInf)-1))
# # fitted values slightly underestimate diversity at high qInf
# 
# # Plot data points (qInf~Rank) and fitted model with confidence interval
# visreg(m1qInf)
# 
# 
# ###
# #spearman rank correlation test 
# cor.test(Rank, qInf, method = "spearman",exact=F) 
# #S = 856.6, p-value = 0.6467 rho 0.1159966 - no significant correlation
# 
# # bootstrapped 95% CI
# quantile(
#   replicate(10000, {
#     boot.i <- sample(length(Rank), replace = TRUE)
#     cor(Rank[boot.i], qInf[boot.i], method = "spearman")
#   }), 
#   c(0.025, 0.975))
# #   2.5%      97.5% 
# #  -0.3800793  0.5336206
# 
# ### added based on stack exchange advice:
# # permutation test
# test <- function(qInf) suppressWarnings(cor.test(Rank, qInf, method="spearman")$estimate)
# rho <- test(qInf)                                     # Test statistic
# p <- replicate(10^3, test(sample(qInf, length(qInf))))   # Simulated permutation distribution
# 
# p.out <- sum(abs(p) > rho)    # Count of strict (absolute) exceedances
# p.at <- sum(abs(p) == rho)    # Count of equalities, if any
# (p.out + p.at /2) / length(p) # Proportion of exceedances: the p-value.
# #  p=0.63
# 
# ###
# ###
# 
# # Moran's i test for spatial autocorrelation
# resids <- m1qInf$residuals
# qInfmoran <- cbind.data.frame(Site,resids,UTM_Coords_S,UTM_Coords_E)
# DB.dists <- as.matrix(dist(cbind(UTM_Coords_E,UTM_Coords_S))) # create distance matrix of site coordinates
# DB.dists.inv <- 1/DB.dists # inverse dist matrix
# diag(DB.dists.inv) <- 0 # replace diagonal with 0s
# DB.dists.inv[1:5, 1:5] # view
# Moran.I(qInfmoran$resids, DB.dists.inv) 
# # $observed
# # [1] -0.03744623
# # 
# # $expected
# # [1] -0.05882353
# # 
# # $sd
# # [1] 0.09288762
# # 
# # $p.value
# # [1] 0.8179818
# 
# 
# ###
# ###
# ### Estimated diversity from iNEXT
# 
# ###
# ## q=0 -  estimated richness -  extrap up to double sample size, comparison made at 95% sample completeness
# ###
# 
# # model q0 against environmental variables
# 
# # tested model fit (tried gaussian, gamma and inverse gaussian)
# 
# m1<- glm(log(e95q0)~Rank, family=gaussian)
# summary(m1)
# 
# ### Testing all possible variables
# 
# #
# m2<- glm(log(e95q0)~Rank+River_dist, family=gaussian)
# summary(m2)
# anova(m2,m1)
# Ldiff <- logLik(m2)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'0.3567349 (df=4) - reject River_dist
# 
# #
# m3<- glm(log(e95q0)~Rank+Road_dist, family=gaussian)
# summary(m3)
# anova(m3,m1)
# Ldiff <- logLik(m3)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.09771971 (df=4) - reject Road_dist
# 
# #
# m4<- glm(log(e95q0)~Rank+Elevation, family=gaussian)
# summary(m4)
# anova(m4,m1)
# Ldiff <- logLik(m4)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.983098 (df=4) - reject elevation
# 
# 
# #
# m5<- glm(log(e95q0)~Rank+Rain_mean, family=gaussian)
# summary(m5)
# anova(m5,m1)
# Ldiff <- logLik(m5)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.4893086 (df=4) - reject rain
# 
# #
# m6<- glm(log(e95q0)~Rank+Elevation+Rank*Elevation, family=gaussian)
# summary(m6)
# anova(m6,m4)
# Ldiff <- logLik(m6)-logLik(m4) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.539037 (df=5) - reject interaction rank*elevation
# 
# #
# m7<- glm(log(e95q0)~Rank+River_dist+Rank*River_dist, family=gaussian)
# summary(m7)
# anova(m7,m2)
# Ldiff <- logLik(m7)-logLik(m2) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' ' 0.6725072 (df=5) - reject interaction rank*river_dist
# 
# #
# m8<- glm(log(e95q0)~Rank+River_dist+Elevation, family=gaussian)
# summary(m8)
# #
# m9<- glm(log(e95q0)~Rank+River_dist+Elevation+River_dist*Elevation, family=gaussian)
# summary(m9)
# anova(m9,m8)
# Ldiff <- logLik(m9)-logLik(m8) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' ' 0.8825159 (df=6)- reject interaction elevation*river_dist
# 
# 
# #
# m10<- glm(log(e95q0)~Rank+Rain_mean+Elevation, family=gaussian)
# summary(m10)
# #
# m11<- glm(log(e95q0)~Rank+Rain_mean+Elevation+Rain_mean*Elevation, family=gaussian)
# summary(m11)
# anova(m11,m10)
# Ldiff <- logLik(m11)-logLik(m10) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.1305948 (df=6) - reject interaction elevation*rain
# 
# 
# ###
# # best model 
# m1 <- glm(log(e95q0)~Rank, family=gaussian)
# summary(m1) # est.coeff Rank:  0.18568    p= 0.000103 **
# par(mfrow=c(2,2))
# plot(m1)
# par(mfrow=c(1,1))
# plot(m1$fitted.values,m1$residuals)
# plot(exp(m1$fitted.values),e95q0,xlim=c(min(e95q0)-1,max(e95q0)-1))
# # fitted values slightly underestimate diversity at high e95q0
# 
# # Plot data points (e95q0~Rank) and fitted model with confidence interval
# visreg(m1)
# 
# 
# ###
# #spearman rank correlation test 
# cor.test(Rank, e95q0, method = "spearman",exact=F) 
# # S = 249.03, p-value = 0.0004111  rho 0.7430051 - strong correlation
# 
# # bootstrapped 95% CI
# quantile(
#   replicate(10000, {
#     boot.i <- sample(length(Rank), replace = TRUE)
#     cor(Rank[boot.i], e95q0[boot.i], method = "spearman")
#   }), 
#   c(0.025, 0.975))
# #   2.5%      97.5% 
# #  0.3595491 0.9271219
# 
# 
# ### added based on stack exchange advice:
# # permutation test
# test <- function(e95q0) suppressWarnings(cor.test(Rank, e95q0, method="spearman")$estimate)
# rho <- test(e95q0)                                     # Test statistic
# p <- replicate(10^3, test(sample(e95q0, length(e95q0))))   # Simulated permutation distribution
# 
# p.out <- sum(abs(p) > rho)    # Count of strict (absolute) exceedances
# p.at <- sum(abs(p) == rho)    # Count of equalities, if any
# (p.out + p.at /2) / length(p) # Proportion of exceedances: the p-value.
# 
# ###
# ###
# 
# # Moran's i test for spatial autocorrelation
# resids <- m1$residuals
# e95q0moran <- cbind.data.frame(Site,resids,UTM_Coords_S,UTM_Coords_E)
# DB.dists <- as.matrix(dist(cbind(UTM_Coords_E,UTM_Coords_S))) # create distance matrix of site coordinates
# DB.dists.inv <- 1/DB.dists # inverse dist matrix
# diag(DB.dists.inv) <- 0 # replace diagonal with 0s
# DB.dists.inv[1:5, 1:5] # view
# Moran.I(e95q0moran$resids, DB.dists.inv) 
# # $observed
# # [1] -0.1499505
# # 
# # $expected
# # [1] -0.05882353
# # 
# # $sd
# # [1] 0.09237277
# # 
# # $p.value
# # [1] 0.3238815
# 
# 
# 
# ###
# #
# ###
# ## q=1 -  estimated q1 -  comparison at asymptote of estimate
# ###
# 
# # model Estq1 against environmental variables
# 
# # tested model fit (tried gaussian, gamma and inverse gaussian)
# 
# m1<- glm(log(e95q1)~Rank, family=gaussian)
# summary(m1)
# 
# ### Testing all possible variables
# 
# #
# m2<- glm(log(e95q1)~Rank+River_dist, family=gaussian)
# summary(m2)
# anova(m2,m1)
# Ldiff <- logLik(m2)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'0.06522708 (df=4) - reject River_dist
# 
# #
# m3<- glm(log(e95q1)~Rank+Road_dist, family=gaussian)
# summary(m3)
# anova(m3,m1)
# Ldiff <- logLik(m3)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.03323337 (df=4) - reject Road_dist -p<0.05, but autocorrelated
# 
# #
# m4<- glm(log(e95q1)~Rank+Elevation, family=gaussian)
# summary(m4)
# anova(m4,m1)
# Ldiff <- logLik(m4)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.9210912 (df=4) - reject elevation
# 
# 
# #
# m5<- glm(log(e95q1)~Rank+Rain_mean, family=gaussian)
# summary(m5)
# anova(m5,m1)
# Ldiff <- logLik(m5)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'0.3617807 (df=4) - reject rain
# 
# #
# m6<- glm(log(e95q1)~Rank+Elevation+Rank*Elevation, family=gaussian)
# summary(m6)
# anova(m6,m4)
# Ldiff <- logLik(m6)-logLik(m4) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.6098945 (df=5) - reject interaction rank*elevation
# 
# #
# m7<- glm(log(e95q1)~Rank+River_dist+Rank*River_dist, family=gaussian)
# summary(m7)
# anova(m7,m2)
# Ldiff <- logLik(m7)-logLik(m2) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' ' 0.6279024 (df=5) - reject interaction rank*river_dist
# 
# #
# m8<- glm(log(e95q1)~Rank+River_dist+Elevation, family=gaussian)
# summary(m8)
# #
# m9<- glm(log(e95q1)~Rank+River_dist+Elevation+River_dist*Elevation, family=gaussian)
# summary(m9)
# anova(m9,m8)
# Ldiff <- logLik(m9)-logLik(m8) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.4378211 (df=6)- reject interaction elevation*river_dist
# 
# 
# #
# m10<- glm(log(e95q1)~Rank+Rain_mean+Elevation, family=gaussian)
# summary(m10)
# #
# m11<- glm(log(e95q1)~Rank+Rain_mean+Elevation+Rain_mean*Elevation, family=gaussian)
# summary(m11)
# anova(m11,m10)
# Ldiff <- logLik(m11)-logLik(m10) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.6082342 (df=6) - reject interaction elevation*rain
# 
# 
# ###
# # best model 
# m1 <- glm(log(e95q1)~Rank, family=gaussian)
# summary(m1) # est.coeff Rank:  0.18568    p= 0.000103 **
# par(mfrow=c(2,2))
# plot(m1)
# par(mfrow=c(1,1))
# plot(m1$fitted.values,m1$residuals)
# plot(exp(m1$fitted.values),e95q1,xlim=c(min(e95q1)-1,max(e95q1)-1))
# # fitted values slightly underestimate diversity at high e95q1
# 
# # Plot data points (e95q1~Rank) and fitted model with confidence interval
# visreg(m1)
# 
# 
# ###
# #spearman rank correlation test 
# cor.test(Rank, e95q1, method = "spearman",exact=F) 
# # S = 540.66, p-value = 0.06625 rho 0.442041 - moderate correlation, but not significant
# 
# # bootstrapped 95% CI
# quantile(
#   replicate(10000, {
#     boot.i <- sample(length(Rank), replace = TRUE)
#     cor(Rank[boot.i], e95q1[boot.i], method = "spearman")
#   }), 
#   c(0.025, 0.975))
# #   2.5%      97.5% 
# # -0.00947622  0.74904702  # crosses 0
# 
# 
# ### added based on stack exchange advice:
# # permutation test
# test <- function(e95q1) suppressWarnings(cor.test(Rank, e95q1, method="spearman")$estimate)
# rho <- test(e95q1)                                     # Test statistic
# p <- replicate(10^3, test(sample(e95q1, length(e95q1))))   # Simulated permutation distribution
# 
# p.out <- sum(abs(p) > rho)    # Count of strict (absolute) exceedances
# p.at <- sum(abs(p) == rho)    # Count of equalities, if any
# (p.out + p.at /2) / length(p) # Proportion of exceedances: the p-value.
# # 0.079
# 
# ###
# ###
# 
# # Moran's i test for spatial autocorrelation
# resids <- m1$residuals
# e95q1moran <- cbind.data.frame(Site,resids,UTM_Coords_S,UTM_Coords_E)
# DB.dists <- as.matrix(dist(cbind(UTM_Coords_E,UTM_Coords_S))) # create distance matrix of site coordinates
# DB.dists.inv <- 1/DB.dists # inverse dist matrix
# diag(DB.dists.inv) <- 0 # replace diagonal with 0s
# DB.dists.inv[1:5, 1:5] # view
# Moran.I(e95q1moran$resids, DB.dists.inv) 
# # $observed
# # [1] 0.01600574
# # 
# # $expected
# # [1] -0.05882353
# # 
# # $sd
# # [1] 0.09177197
# # 
# # $p.value
# # [1] 0.4148533
# 
# 
# ###
# #
# ###
# ## q=2 -  estimated q2 -  comparison at asymptote of estimate
# ###
# 
# # model Estq2 against environmental variables
# 
# # tested model fit (tried gaussian, gamma and inverse gaussian)
# 
# m1<- glm(log(e95q2)~Rank, family=gaussian)
# summary(m1)
# 
# ### Testing all possible variables
# 
# #
# m2<- glm(log(e95q2)~Rank+River_dist, family=gaussian)
# summary(m2)
# anova(m2,m1)
# Ldiff <- logLik(m2)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'0.1068385 (df=4)- reject River_dist
# 
# #
# m3<- glm(log(e95q2)~Rank+Road_dist, family=gaussian)
# summary(m3)
# anova(m3,m1)
# Ldiff <- logLik(m3)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.04242739 ((df=4) - reject Road_dist -p<0.05, but autocorrelated
# 
# #
# m4<- glm(log(e95q2)~Rank+Elevation, family=gaussian)
# summary(m4)
# anova(m4,m1)
# Ldiff <- logLik(m4)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.564124 (df=4) - reject elevation
# 
# 
# #
# m5<- glm(log(e95q2)~Rank+Rain_mean, family=gaussian)
# summary(m5)
# anova(m5,m1)
# Ldiff <- logLik(m5)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'0.290226 (df=4) - reject rain
# 
# #
# m6<- glm(log(e95q2)~Rank+Elevation+Rank*Elevation, family=gaussian)
# summary(m6)
# anova(m6,m4)
# Ldiff <- logLik(m6)-logLik(m4) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.6190971 (df=5) - reject interaction rank*elevation
# 
# #
# m7<- glm(log(e95q2)~Rank+River_dist+Rank*River_dist, family=gaussian)
# summary(m7)
# anova(m7,m2)
# Ldiff <- logLik(m7)-logLik(m2) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' '  0.5080854(df=5) - reject interaction rank*river_dist
# 
# #
# m8<- glm(log(e95q2)~Rank+River_dist+Elevation, family=gaussian)
# summary(m8)
# #
# m9<- glm(log(e95q2)~Rank+River_dist+Elevation+River_dist*Elevation, family=gaussian)
# summary(m9)
# anova(m9,m8)
# Ldiff <- logLik(m9)-logLik(m8) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.4149681 (df=6)- reject interaction elevation*river_dist
# 
# 
# #
# m10<- glm(log(e95q2)~Rank+Rain_mean+Elevation, family=gaussian)
# summary(m10)
# #
# m11<- glm(log(e95q2)~Rank+Rain_mean+Elevation+Rain_mean*Elevation, family=gaussian)
# summary(m11)
# anova(m11,m10)
# Ldiff <- logLik(m11)-logLik(m10) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.3517385 (df=6) - reject interaction elevation*rain
# 
# 
# ###
# # best model 
# m1 <- glm(log(e95q2)~Rank, family=gaussian)
# summary(m1) # est.coeff Rank:  0.07168   p=  0.303479  
# par(mfrow=c(2,2))
# plot(m1)
# par(mfrow=c(1,1))
# plot(m1$fitted.values,m1$residuals)
# plot(exp(m1$fitted.values),e95q2,xlim=c(min(e95q2)-1,max(e95q2)-1))
# # fitted values slightly underestimate diversity at high e95q2
# 
# # Plot data points (e95q2~Rank) and fitted model with confidence interval
# visreg(m1)
# 
# 
# ###
# #spearman rank correlation test 
# cor.test(Rank, e95q2, method = "spearman",exact=F) 
# # S = 704.71, p-value = 0.2735 rho0.2727487  - weak correlation, not significant
# 
# # bootstrapped 95% CI
# quantile(
#   replicate(10000, {
#     boot.i <- sample(length(Rank), replace = TRUE)
#     cor(Rank[boot.i], e95q2[boot.i], method = "spearman")
#   }), 
#   c(0.025, 0.975))
# #   2.5%      97.5% 
# # -0.2071749  0.6661216  # crosses 0
# 
# 
# ### added based on stack exchange advice:
# # permutation test
# test <- function(e95q2) suppressWarnings(cor.test(Rank, e95q2, method="spearman")$estimate)
# rho <- test(e95q2)                                     # Test statistic
# p <- replicate(10^3, test(sample(e95q2, length(e95q2))))   # Simulated permutation distribution
# 
# p.out <- sum(abs(p) > rho)    # Count of strict (absolute) exceedances
# p.at <- sum(abs(p) == rho)    # Count of equalities, if any
# (p.out + p.at /2) / length(p) # Proportion of exceedances: the p-value.
# #  0.2565
# 
# ###
# ###
# 
# # Moran's i test for spatial autocorrelation
# resids <- m1$residuals
# e95q2moran <- cbind.data.frame(Site,resids,UTM_Coords_S,UTM_Coords_E)
# DB.dists <- as.matrix(dist(cbind(UTM_Coords_E,UTM_Coords_S))) # create distance matrix of site coordinates
# DB.dists.inv <- 1/DB.dists # inverse dist matrix
# diag(DB.dists.inv) <- 0 # replace diagonal with 0s
# DB.dists.inv[1:5, 1:5] # view
# Moran.I(e95q2moran$resids, DB.dists.inv) 
# # $observed
# # [1] -0.02684998
# # 
# # $expected
# # [1] -0.05882353
# # 
# # $sd
# # [1] 0.09147996
# # 
# # $p.value
# # [1] 0.7267033
# 
# 
# ###
# #
# ###
# ## q=3 -  estimated q3 -  comparison at 95% Sample coverage 
# 
# 
# ###
# 
# # model Estq2 against environmental variables
# 
# # tested model fit (tried gaussian, gamma and inverse gaussian)
# 
# m1<- glm(log(e95q3)~Rank, family=gaussian)
# summary(m1)
# 
# ### Testing all possible variables
# 
# #
# m2<- glm(log(e95q3)~Rank+River_dist, family=gaussian)
# summary(m2)
# anova(m2,m1)
# Ldiff <- logLik(m2)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.1404416 (df=4)- reject River_dist
# 
# #
# m3<- glm(log(e95q3)~Rank+Road_dist, family=gaussian)
# summary(m3)
# anova(m3,m1)
# Ldiff <- logLik(m3)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'0.05397449(df=4) - reject Road_dist -p<0.05, but autocorrelated
# 
# #
# m4<- glm(log(e95q3)~Rank+Elevation, family=gaussian)
# summary(m4)
# anova(m4,m1)
# Ldiff <- logLik(m4)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'  0.5249224 (df=4) - reject elevation
# 
# 
# #
# m5<- glm(log(e95q3)~Rank+Rain_mean, family=gaussian)
# summary(m5)
# anova(m5,m1)
# Ldiff <- logLik(m5)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'0.2710427 (df=4) - reject rain
# 
# #
# m6<- glm(log(e95q3)~Rank+Elevation+Rank*Elevation, family=gaussian)
# summary(m6)
# anova(m6,m4)
# Ldiff <- logLik(m6)-logLik(m4) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.499004 (df=5) - reject interaction rank*elevation
# 
# #
# m7<- glm(log(e95q3)~Rank+River_dist+Rank*River_dist, family=gaussian)
# summary(m7)
# anova(m7,m2)
# Ldiff <- logLik(m7)-logLik(m2) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' '  0.5733542 (df=5) - reject interaction rank*river_dist
# 
# #
# m8<- glm(log(e95q3)~Rank+River_dist+Elevation, family=gaussian)
# summary(m8)
# #
# m9<- glm(log(e95q3)~Rank+River_dist+Elevation+River_dist*Elevation, family=gaussian)
# summary(m9)
# anova(m9,m8)
# Ldiff <- logLik(m9)-logLik(m8) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.4398869 (df=6)- reject interaction elevation*river_dist
# 
# 
# #
# m10<- glm(log(e95q3)~Rank+Rain_mean+Elevation, family=gaussian)
# summary(m10)
# #
# m11<- glm(log(e95q3)~Rank+Rain_mean+Elevation+Rain_mean*Elevation, family=gaussian)
# summary(m11)
# anova(m11,m10)
# Ldiff <- logLik(m11)-logLik(m10) 
# 1-pchisq(2*(Ldiff),1) #'log Lik.'   0.2874275  (df=6) - reject interaction elevation*rain
# 
# 
# ###
# # best model 
# m1 <- glm(log(e95q3)~Rank, family=gaussian)
# summary(m1) # est.coeff Rank:  0.05092   p=  0.435443 
# par(mfrow=c(2,2))
# plot(m1)
# par(mfrow=c(1,1))
# plot(m1$fitted.values,m1$residuals)
# plot(exp(m1$fitted.values),e95q3,xlim=c(min(e95q3)-1,max(e95q3)-1))
# # fitted values slightly underestimate diversity at high e95q3
# 
# # Plot data points (e95q3~Rank) and fitted model with confidence interval
# visreg(m1)
# 
# 
# ###
# #spearman rank correlation test 
# cor.test(Rank, e95q3, method = "spearman",exact=F) 
# # S = 783.69, p-value = 0.4472  rho 0.1912376   - weak correlation, not significant
# 
# # bootstrapped 95% CI
# quantile(
#   replicate(10000, {
#     boot.i <- sample(length(Rank), replace = TRUE)
#     cor(Rank[boot.i], e95q3[boot.i], method = "spearman")
#   }), 
#   c(0.025, 0.975))
# #   2.5%      97.5% 
# # -0.2897893  0.5934263  # crosses 0
# 
# 
# ### added based on stack exchange advice:
# # permutation test
# test <- function(e95q3) suppressWarnings(cor.test(Rank, e95q3, method="spearman")$estimate)
# rho <- test(e95q3)                                     # Test statistic
# p <- replicate(10^3, test(sample(e95q3, length(e95q3))))   # Simulated permutation distribution
# 
# p.out <- sum(abs(p) > rho)    # Count of strict (absolute) exceedances
# p.at <- sum(abs(p) == rho)    # Count of equalities, if any
# (p.out + p.at /2) / length(p) # Proportion of exceedances: the p-value.
# #  0.428
# 
# ###
# ###
# 
# # Moran's i test for spatial autocorrelation
# resids <- m1$residuals
# e95q3moran <- cbind.data.frame(Site,resids,UTM_Coords_S,UTM_Coords_E)
# DB.dists <- as.matrix(dist(cbind(UTM_Coords_E,UTM_Coords_S))) # create distance matrix of site coordinates
# DB.dists.inv <- 1/DB.dists # inverse dist matrix
# diag(DB.dists.inv) <- 0 # replace diagonal with 0s
# DB.dists.inv[1:5, 1:5] # view
# Moran.I(e95q3moran$resids, DB.dists.inv) 
# # $observed
# # [1] -0.03410654
# # 
# # $expected
# # [1] -0.05882353
# # 
# # $sd
# # [1] 0.09262909
# # 
# # $p.value
# # [1] 0.7895937
# 
# 
# ###
# ###
# 
# # # model est richness (Chao1) against environmental variables
# 
# 
# m1<- glm(Chao_1_Mean~Rank, family=poisson(link="log"))
# summary(m1)
# plot(Chao_1_Mean~Rank)
# lines(m1$fitted.values)
# 
# hist(Chao_1_Mean)
# anova(m1) #  testing goodness of fit.
# pval <- 1-pchisq(6.4195,16) # model assumptions and distribution are ok??
# plot(m1) # resids look ok
# 
# # Moran's i
# resids <- m1$residuals
# CHmoran <- cbind.data.frame(Site,resids,UTM_Coords_S,UTM_Coords_E)
# DB.dists <- as.matrix(dist(cbind(UTM_Coords_E,UTM_Coords_S))) # create distance matrix of site coordinates
# DB.dists.inv <- 1/DB.dists # inverse dist matrix
# diag(DB.dists.inv) <- 0 # replace diagonal with 0s
# DB.dists.inv[1:5, 1:5] # view
# Moran.I(CHmoran$resids, DB.dists.inv) # calculate moran's i - no significant autocorrelation
# 
# # #$observed
# # [1] -0.1941475
# # 
# # $expected
# # [1] -0.05882353
# # 
# # $sd
# # [1] 0.09001515
# # 
# # $p.value
# # [1] 0.1327498 # no autocorrelation
# 
# 
# 
# m2<- glm(Chao_1_Mean~Rank+River_dist, family=poisson(link="log"))
# summary(m2)
# 
# Ldiff <- logLik(m2)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) # ##! Cant test log like because model logLik = -Inf because of Poisson integer combination
# 
# m3<- glm(Chao_1_Mean~Rank+Elevation, family=poisson(link="log"))
# summary(m3)
# 
# Ldiff <- logLik(m3)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) # 
# 
# m4<- glm(Chao_1_Mean~Rank+Road_dist, family=poisson(link="log"))
# summary(m4)
# 
# Ldiff <- logLik(m4)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) # Road didn't improve model
# 
# m5<- glm(Chao_1_Mean~Rank+Rain_mean,family=poisson(link="log"))
# summary(m5)
# 
# Ldiff <- logLik(m5)-logLik(m1) 
# 1-pchisq(2*(Ldiff),1) # Rain didn't improve model
# 
# #Best model:
# m1<- glm(Chao_1_Mean~Rank, family=poisson(link="log"))
# summary(m1)
# ### 
# # significant probability? that Chao1  est richness decreases  with increased disturbance
# 
# ###
# # 
# ###
# # # model est richness (ICE) against environmental variables
# # 
# # m1<- glm(ICE_Mean~Rank, family=gaussian)
# # summary(m1)
# # 
# # m2<- glm(ICE_Mean~Rank+River_dist, family=gaussian)
# # summary(m2)
# # 
# # Ldiff <- logLik(m2)-logLik(m1) 
# # 1-pchisq(2*(Ldiff),1) # River_dist didn't improve model
# # 
# # m3<- glm(ICE_Mean~Rank+Elevation, family=gaussian)
# # summary(m3)
# # 
# # Ldiff <- logLik(m3)-logLik(m1) 
# # 1-pchisq(2*(Ldiff),1) # Elevation didn't improve model
# # 
# # m4<- glm(ICE_Mean~Rank+Road_dist, family=gaussian)
# # summary(m4)
# # 
# # Ldiff <- logLik(m4)-logLik(m1) 
# # 1-pchisq(2*(Ldiff),1) # Road didn't improve model
# # 
# # m5<- glm(ICE_Mean~Rank+Rain_mean,family=gaussian)
# # summary(m5)
# # 
# # Ldiff <- logLik(m5)-logLik(m1) 
# # 1-pchisq(2*(Ldiff),1) # Rain didn't improve model
# # 
# # #Best model:
# # m1<- glm(ICE_Mean~Rank, family=gaussian)
# # summary(m1)
# # hist(m1$residuals,breaks=10)
# # confint(m1)
# 
# # significant probability that ICE est richness decreases  with increased disturbance
# 
# ###
# # Summary
# ###
# # Rank of distrbance is a good predictor of estimated 
# # richness (Chao1 and ICE) and of q1 diversity -  not of anything else
# 
# 
# ###
# # Plots
# 
# png(file="C://Data/PhD/Outputs/Dungbeetles/Diversity/q1_rank.png",width=10,height=7.5,units="in",res=180)  
# plot(q1~Rank,pch=21,bg="mediumseagreen",cex=3,bty="n", cex.axis=1.7,cex.lab=1.7,
#      xlim=c(1,6),ylim=c(0,15),ylab="Shannon diversity (q=1)",xlab="Disturbance rank")
# m4<- glm(q1~Rank,family=Gamma(link="inverse"))
# abline(m4,col="mediumseagreen",lwd=3)
# dev.off()
# 
# 
# 
# png(file="C://Data/PhD/Outputs/Dungbeetles/Diversity/richness_rank.png",width=10,height=7.5,units="in",res=180)  
# plot(q0~Rank,pch=21,bg="mediumseagreen",cex=3,bty="n", cex.axis=1.7,cex.lab=1.7,
#      xlim=c(1,6),ylim=c(0,30),ylab="Species richness",xlab="Disturbance rank")
# m4<- glm(q0~Rank,family=Gamma(link="inverse"))
# abline(m4,col="mediumseagreen",lwd=3)
# points(Chao_1_Mean~Rank,pch=21,bg="royalblue",cex=3)
# m2<- glm(Chao_1_Mean~Rank, family=Gamma(link="inverse"))
# abline(m2,col="royalblue",lwd=3)
# legend(4,7,legend= c("Observed richness (q=0)","Chao1 estimated richness"),
#        cex=1.5,pch=21,pt.bg=c("mediumseagreen","royalblue"),
#        pt.cex=2)
# dev.off()
# 
# 
# 
# 
# 
# ###
# ### added based on stack exchange advice:
# # permutation test for correlation
# test <- function(q1) suppressWarnings(cor.test(Rank, q1, method="spearman")$estimate)
# rho <- test(q1)                                     # Test statistic
# p <- replicate(10^3, test(sample(q1, length(q1))))   # Simulated permutation distribution
# 
# p.out <- sum(abs(p) > rho)    # Count of strict (absolute) exceedances
# p.at <- sum(abs(p) == rho)    # Count of equalities, if any
# (p.out + p.at /2) / length(p) # Proportion of exceedances: the p-value.
# 
# # Use a permutation test. You only need to permute one of the variables independently 
# # of the other; here, the response is permuted. Because the relationship in the example
# # is strong, only a small number of permutations are needed (1000 in the example below).
# # 
# # As always, the actual statistic is compared to the distribution of permuted statistics.
# # The p-value is the estimate of the tail probability of the permutation distribution 
# # relative to the actual statistic. In some cases the test statistic has a discrete 
# # distribution, so it's wise to check the frequencies with which (a) the permutation 
# # statistics strictly exceed the actual statistic and (b) the permutation statistics 
# # equal or exceed the actual statistic. The code illustrates this by splitting the difference.
# 
