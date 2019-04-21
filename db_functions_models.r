####
### Ecosystem Functions of dung beetles ----
####

rm(list=ls()) 

# Load packages ----

require(dplyr)
library(tidyr)
library(stringr)
require(lme4)
require(lmerTest)

## CREATE DATAFRAME ----
#  
# fun <- as.data.frame(read.csv("C://Data/PhD/Rawdata/Dungbeetles/Dungbeetlefunctions_data.csv"))
# weather <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_functions_weather.csv")
# veg <- read.csv("C://Data/PhD/Processed_data/Vegetation/veg_pca_out.csv")
# alpha <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/Diversity/alpha.csv")
# site_data <- read.csv("C://Data/PhD/Processed_data/Site_data/sites_elev_dist_coords.csv")
# rich <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/ESoutput/DB_ES_RichEst_replacement.csv")
# abund <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_Abund_groups.csv")
# Estqs <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/db_estqs_inext.csv")
# #
# head(abund)
# abund <- abund[,-1]
# str(weather)
# str(veg)
# str(alpha)
# str(site_data)
# str(rich)
# str(fun)
# #
# #
# str(rich) #get rid of some of the excess columns of richness output
# rem_rich <- c(8,9,10,11,12,13,14,15)#cols of richness ests to remove
# rich <- rich[,-rem_rich]
# #
# # Edited alpha file in Excel to separate Site_rank into rank and site columns
# # Also edited richness output to replace spaces with underscores and remove % and () from colnames
# colnames(weather)[1] <- "Site"
# colnames(veg)[1] <- "Site"
# colnames(rich)[1] <- "Site"
# colnames(alpha)[1] <- "Site"
# #
# newalphasite <- str_split_fixed(alpha$Site, ".",3) #split Site column, to get site values without Rank. infront
# alpha$Site <- newalphasite[,3]
# #
# #
# alpha$Site <- as.factor(alpha$Site)
# levels(alpha$Site)
# levels(alpha$Site)[levels(alpha$Site)=="MinD-B"] <- "MIN-B"
# levels(alpha$Site)[levels(alpha$Site)=="MinD-A"] <- "MIN-A"
# levels(alpha$Site)[levels(alpha$Site)=="MinD-C"] <- "MIN-C"
# #
# str(fun)
# fun$Rank <- as.numeric(as.character(fun$Rank))
# fun$Site <- as.factor(fun$Site)
# fun$Habitat <- as.factor(fun$Habitat)
# fun$Arena <- as.factor(fun$Arena)
# levels(fun$Site)[levels(fun$Site)=="MinD-B"] <- "MIN-B"
# levels(fun$Site)[levels(fun$Site)=="MinD-A"] <- "MIN-A"
# # levels(fun$Site)[levels(alpha$Site)=="MinD-C"] <- "MIN-C" # I manually edited this site name in excel because I couldn't get it to work here
# #
# 
# levels(weather$Site)
# levels(veg$Site)
# levels(alpha$Site)
# levels(site_data$Site)
# levels(rich$Site)
# levels(fun$Site)
# 
# levels(DB_allvars$Rank.x)
# DB_merge0 <- merge(fun,site_data,by="Site")
# DB_merge1 <- merge(DB_merge0,alpha,by="Site")
# DB_merge2 <- merge(DB_merge1,veg,by="Site")
# DB_merge3 <- merge(DB_merge2,weather,by="Site")
# DB_merge4 <- merge(DB_merge3,abund,by="Site")
# DB_merge5 <- merge(DB_merge4,Estqs,by="Site")
# DB_allvars <- merge(DB_merge5,rich,by="Site")
# #
# head(DB_allvars)
# edit(DB_allvars)
# names(DB_allvars)[names(DB_allvars) == 'Rank.x'] <- 'Rank'
# 
# #write.csv(DB_allvars,"C://Data/PhD/Processed_data/Dungbeetles/DB_fun_allvars.csv")

####
#### Load data ----

DBfun <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_fun_allvars.csv")
head(DBfun)

rem <- which(DBfun$Date_checked=="11/06/2016") # remove data from friaje dates (these were repeated)
DBfun <- DBfun[-rem,]
names(DBfun)[names(DBfun) == 'Excavated_soil__weight'] <- 'Excavated_soil_weight'
# edit(DBfun)
# explore patterns:
head(DBfun,1)
plot(mean_dispersal_Medium~Rank,data=DBfun,col="forestgreen",pch=19)
plot(Excavated_soil_weight~Rank, data=DBfun, col="forestgreen",pch=19)
plot(Large_count_remain~Rank,data=DBfun,col="forestgreen",pch=19)
plot(Small_count_remain~Rank,data=DBfun,col="forestgreen",pch=19)
plot(Medium_count_remain~Rank,data=DBfun,col="forestgreen",pch=19)
plot(Weight_remaining_beads~Rank,data=DBfun,col="forestgreen",pch=19)
plot(mean_dispersal_Large~Rank,data=DBfun,col="forestgreen",pch=19)
plot(mean_dispersal_Medium~Rank,data=DBfun,col="forestgreen",pch=19)
plot(mean_dispersal_Medium~q1,data=DBfun)

# convert variables to more meaningful ones
# Dung_end_weight - amount of dung removed
Dung_removed <- c(100-DBfun$Dung_end_weight) # this is now a continuous normal variable
Small_dispersed <- c(50-DBfun$Small_count_remain)
Medium_dispersed <- c(20-DBfun$Medium_count_remain)
Large_dispersed <- c(10-DBfun$Large_count_remain)
Allbeads_disp <- Small_dispersed+Medium_dispersed+Large_dispersed
DBfun <- cbind(DBfun,Dung_removed,Small_dispersed,Medium_dispersed,Large_dispersed,Allbeads_disp)
plot(Allbeads_disp~Rank,data=DBfun)


## Variables to include ----
## Functions to test (response vars):
# dung removal
# excavated soil
# S,M,L beads dispersed (count)
# S,M,L bead dispersal distance

##  Explanatory vars:
# Rank
# Weather
# Elevation
# Road_dist
# River_dist
# Random effects: (1|Arena/Site) + (1|Date)
# diversity (q0,q1,q2) (instead of rank - separate models)

## Dung removal ----

# First test which of weather variables are most influential 
cor.test(DBfun$Dung_removed, DBfun$Temp07mean, method = "spearman",exact=F) 
cor.test(DBfun$Dung_removed, DBfun$Rain_mean, method = "spearman",exact=F) 
cor.test(DBfun$Dung_removed, DBfun$Hum07mean, method = "spearman",exact=F) 
# rain strongest effect

### Correlation test
cor.test(DBfun$Rank, DBfun$Dung_removed, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$Rank), replace = TRUE)
    cor(DBfun$Rank[boot.i], DBfun$Dung_removed[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

### No relationship between dung removal and rank, so no reason to run mixed models

## Soil excavation  ----

# Test which of weather variables are most influential 
cor.test(DBfun$Excavated_soil_weight, DBfun$Temp07mean, method = "spearman",exact=F) 
cor.test(DBfun$Excavated_soil_weight, DBfun$Rain_mean, method = "spearman",exact=F) 
cor.test(DBfun$Excavated_soil_weight, DBfun$Hum07mean, method = "spearman",exact=F) 
# rain strongest effect

### Correlation test
cor.test(DBfun$Rank, DBfun$Excavated_soil_weight, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$Rank), replace = TRUE)
    cor(DBfun$Rank[boot.i], DBfun$Excavated_soil_weight[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
# no relationship between rank and soil excavation

## Bead dispersal ALL SIZES  - proportion dispersed ----

### Correlation test
cor.test(DBfun$Rank, DBfun$Allbeads_disp, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$Rank), replace = TRUE)
    cor(DBfun$Rank[boot.i], DBfun$Allbeads_disp[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


## Bead dispersal - proportion dispersed (Small)  ----

### Correlation test
cor.test(DBfun$Rank, DBfun$Small_dispersed, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$Rank), replace = TRUE)
    cor(DBfun$Rank[boot.i], DBfun$Small_dispersed[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# Test which of weather variables are most influential 
cor.test(DBfun$Small_dispersed, DBfun$Temp07mean, method = "spearman",exact=F) 
cor.test(DBfun$Small_dispersed, DBfun$Rain_mean, method = "spearman",exact=F) 
cor.test(DBfun$Small_dispersed, DBfun$Hum07mean, method = "spearman",exact=F) 
# rain strongest effect

smallprop <- cbind(DBfun$Small_dispersed,DBfun$Small_count_remain)
m0 <- glmer(smallprop~Rank+(1|Site/Arena)+(1|Date_checked), binomial,data=DBfun)
summary(m0) #dev=848, df=67
logLik(m0)

#To model over-dispersion you can try fitting an observation-level random effect.
# and fitting (1|observ) in the model formula - recc by jarrod hadfield from https://stat.ethz.ch/pipermail/r-sig-mixed-models/2010q4/004731.html
DBfun$observ<-as.factor(1:dim(DBfun)[1])
m1 <- glmer(smallprop~Rank+(1|Site/Arena)+(1|Date_checked)+(1|observ), binomial,data=DBfun)
summary(m1) 
logLik(m1)

m2 <- glmer(smallprop~Rank+Elevation+(1|Site/Arena)+(1|Date_checked)+(1|observ), binomial, data=DBfun)
summary(m2)
# scales too different -  but rescaling didn't help, model failed to converge.

m3 <- glmer(smallprop~Rank+scale(Elevation)+(1|Site/Arena)+(1|Date_checked)+(1|observ), binomial, data=DBfun)
summary(m3)

m4 <- glmer(smallprop~Rank+River_dist+(1|Site/Arena)+(1|Date_checked)+(1|observ), binomial, data=DBfun)
summary(m4)



### test for overdispersion http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#testing-for-overdispersioncomputing-overdispersion-factor
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(m0)
overdisp_fun(m1) # observation level random effect seems to have corrected for overdispersion.



## Bead dispersal - proportion dispersed (Medium)  ----

### Correlation test
cor.test(DBfun$Rank, DBfun$Medium_dispersed, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$Rank), replace = TRUE)
    cor(DBfun$Rank[boot.i], DBfun$Medium_dispersed[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


## Bead dispersal - proportion dispersed (Large)  ----

### Correlation test
cor.test(DBfun$Rank, DBfun$Large_dispersed, method = "spearman",exact=F) 


## Bead dispersal distance small ----

### Correlation test
cor.test(DBfun$Rank, DBfun$mean_dispersal_Small, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$Rank), replace = TRUE)
    cor(DBfun$Rank[boot.i], DBfun$mean_dispersal_Small[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
# no relationship 

## Bead dispersal distance medium----

### Correlation test
cor.test(DBfun$Rank, DBfun$mean_dispersal_Medium, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$Rank), replace = TRUE)
    cor(DBfun$Rank[boot.i], DBfun$mean_dispersal_Medium[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
# no relationship 

## Bead dispersal distance Large ----

### Correlation test
cor.test(DBfun$Rank, DBfun$mean_dispersal_Large, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$Rank), replace = TRUE)
    cor(DBfun$Rank[boot.i], DBfun$mean_dispersal_Large[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
# no relationship 


## Functions plots----
#png(file="C://Data/PhD/Outputs/Dungbeetles/functions/ecofunctions.png",width=10,height=7.5,units="in",res=250)  
tiff(file="C://Data/PhD/Outputs/Dungbeetles/Biotropica figures/FigS5_seeddispersal.tiff",width=190,height=205,units="mm",res=1000, pointsize=14)  
par(mfrow=c(2,2))
par(mar=c(4.1,5.4,2,0.1))
plot((Allbeads_disp/80)*100~Rank,bty="n",pch=21,cex=1.2,cex.axis=1,cex.lab=1.2,col='black',data=DBfun,ylab="Total Seeds Dispersed %",xlab="Disturbance Rank",ylim=c(0,115))
text(1.2,112,"a",font=2,cex=1.2)
abline(lm(Allbeads_disp~DBfun$Rank),col="black")
plot((Small_dispersed/50)*100~Rank,pch=21,cex=1.2,cex.axis=1,cex.lab=1.2,col='black',bty="n",data=DBfun,ylab="Small Seeds Dispersed %",xlab="Disturbance Rank",ylim=c(0,115))
text(1.2,112,"b",font=2,cex=1.2)
abline(lm(((Small_dispersed/50)*100)~DBfun$Rank),col="black")
plot((Medium_dispersed/20)*100~Rank,bty="n",pch=21,cex=1.2,cex.axis=1,cex.lab=1.2,col='black',data=DBfun,ylab="Medium Seeds Dispersed %",xlab="Disturbance Rank",ylim=c(0,115))
text(1.2,112,"c",font=2,cex=1.2)
abline(lm(((Medium_dispersed/20)*100)~DBfun$Rank),col="black")
plot((Large_dispersed/10)*100~Rank,bty="n",pch=21,cex=1.2,cex.axis=1,cex.lab=1.2,col='black',data=DBfun,ylab="Large Seeds Dispersed %",xlab="Disturbance Rank",ylim=c(0,115))
text(1.2,112,"d",font=2,cex=1.2)
abline(lm(((Large_dispersed/10)*100)~DBfun$Rank),col="black")
dev.off()

#plot(Excavated_soil_weight~Rank,pch=19,cex=1.5,cex.axis=1.2,cex.lab=1.2,col='black',data=DBfun,main="Soil Excavation",ylab="Excavated Soil Weight (g)",xlab="Disturbance Rank",ylim=c(0,550))
#plot(mean_dispersal_Medium~Rank,pch=19,cex=1.5,cex.axis=1.2,cex.lab=1.2,col='black',data=DBfun,main="Mean Dispersal Distance - Medium 'Seeds'",ylab="Mean Dispersal Distance (cm)",xlab="Disturbance Rank",ylim=c(0,80))

###
###
### Response of functions to different dung beetle diversity ----

### function v q0 ----
# dung removal
res <- cor.test(DBfun$q0, DBfun$Dung_removed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# soil excavation
res <- cor.test(DBfun$q0, DBfun$Excavated_soil_weight, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# small beads dispersed
res <- cor.test(DBfun$q0, DBfun$Small_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$q0), replace = TRUE)
    cor(DBfun$q0[boot.i], DBfun$Small_dispersed[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# MEdium beads dispersed
res <- cor.test(DBfun$q0, DBfun$Medium_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$q0), replace = TRUE)
    cor(DBfun$q0[boot.i], DBfun$Medium_dispersed[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# Large beads dispersed
res <- cor.test(DBfun$q0, DBfun$Large_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$q0), replace = TRUE)
    cor(DBfun$q0[boot.i], DBfun$Large_dispersed[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# mean dispersal distance- small
res <- cor.test(DBfun$q0, DBfun$mean_dispersal_Small, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# mean dispersal distance- medium
res <- cor.test(DBfun$q0, DBfun$mean_dispersal_Medium, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# mean dispersal distance- large
res <- cor.test(DBfun$q0, DBfun$mean_dispersal_Large, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)


### function v q1 ----
# dung removal
res <- cor.test(DBfun$q1, DBfun$Dung_removed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# soil excavation
res <- cor.test(DBfun$q1, DBfun$Excavated_soil_weight, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# small beads dispersed
res <- cor.test(DBfun$q1, DBfun$Small_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# MEdium beads dispersed
res <- cor.test(DBfun$q1, DBfun$Medium_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# Large beads dispersed
res <- cor.test(DBfun$q1, DBfun$Large_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# mean dispersal distance- small
res <- cor.test(DBfun$q1, DBfun$mean_dispersal_Small, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# mean dispersal distance- medium
res <- cor.test(DBfun$q1, DBfun$mean_dispersal_Medium, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$q1), replace = TRUE)
    cor(DBfun$q1[boot.i], DBfun$mean_dispersal_Medium[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# mean dispersal distance- large
res <- cor.test(DBfun$q1, DBfun$mean_dispersal_Large, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$q1), replace = TRUE)
    cor(DBfun$q1[boot.i], DBfun$mean_dispersal_Large[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


### function v q2 ----
# dung removal
res <- cor.test(DBfun$q2, DBfun$Dung_removed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# soil excavation
res <- cor.test(DBfun$q2, DBfun$Excavated_soil_weight, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# small beads dispersed
res <- cor.test(DBfun$q2, DBfun$Small_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# MEdium beads dispersed
res <- cor.test(DBfun$q2, DBfun$Medium_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# Large beads dispersed
res <- cor.test(DBfun$q2, DBfun$Large_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# mean dispersal distance- small
res <- cor.test(DBfun$q2, DBfun$mean_dispersal_Small, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# mean dispersal distance- medium
res <- cor.test(DBfun$q2, DBfun$mean_dispersal_Medium, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# mean dispersal distance- large
res <- cor.test(DBfun$q2, DBfun$mean_dispersal_Large, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

### function v e95q0 ----
# dung removal
res <- cor.test(DBfun$e95q0, DBfun$Dung_removed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# soil excavation
res <- cor.test(DBfun$e95q0, DBfun$Excavated_soil_weight, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# small beads dispersed
res <- cor.test(DBfun$e95q0, DBfun$Small_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$e95q0), replace = TRUE)
    cor(DBfun$e95q0[boot.i], DBfun$Small_dispersed[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# MEdium beads dispersed
res <- cor.test(DBfun$e95q0, DBfun$Medium_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$e95q0), replace = TRUE)
    cor(DBfun$e95q0[boot.i], DBfun$Medium_dispersed[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# Large beads dispersed
res <- cor.test(DBfun$e95q0, DBfun$Large_dispersed, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$e95q0), replace = TRUE)
    cor(DBfun$e95q0[boot.i], DBfun$Large_dispersed[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# mean dispersal distance- small
res <- cor.test(DBfun$e95q0, DBfun$mean_dispersal_Small, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# mean dispersal distance- medium
res <- cor.test(DBfun$e95q0, DBfun$mean_dispersal_Medium, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$e95q0), replace = TRUE)
    cor(DBfun$e95q0[boot.i], DBfun$mean_dispersal_Medium[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

# mean dispersal distance- large
res <- cor.test(DBfun$e95q0, DBfun$mean_dispersal_Large, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)


### function v asEst.q1 ----
# dung removal
cor.test(DBfun$asEst.q1, DBfun$Dung_removed, method = "spearman",exact=F) 

# soil excavation
cor.test(DBfun$asEst.q1, DBfun$Excavated_soil_weight, method = "spearman",exact=F) 

# small beads dispersed
cor.test(DBfun$asEst.q1, DBfun$Small_dispersed, method = "spearman",exact=F) 

# MEdium beads dispersed
cor.test(DBfun$asEst.q1, DBfun$Medium_dispersed, method = "spearman",exact=F) 

# Large beads dispersed
cor.test(DBfun$asEst.q1, DBfun$Large_dispersed, method = "spearman",exact=F) 

# mean dispersal distance- small
cor.test(DBfun$asEst.q1, DBfun$mean_dispersal_Small, method = "spearman",exact=F) 

# mean dispersal distance- medium
cor.test(DBfun$asEst.q1, DBfun$mean_dispersal_Medium, method = "spearman",exact=F) 

# mean dispersal distance- large
cor.test(DBfun$asEst.q1, DBfun$mean_dispersal_Large, method = "spearman",exact=F) 


##///////////
#  Function ~ abundance ----

Spec <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/db_spXsite.csv",row.names = 1)
Spec <- t(Spec)
head(Spec)
names(Spec) <- c("BA-A","BA-B","BA-C","AF-A","AF-B","AF-C","SF-A","SF-B","SF-C","CCR-A","CCR-B","CCR-C","MXD-A","MXD-B","MXD-C","MIN-A","MIN-B","MIN-C")

ab <- colSums(Spec)
barplot(ab,main="abundance of tunnellers")

DBfun <- cbind(DBfun,ab)

#abundance v rank total
cor.test(DBfun$ab,DBfun$Rank, method = "spearman",exact=F) 

cor.test(DBfun$ab, DBfun$Dung_removed, method = "spearman",exact=F) 

cor.test(DBfun$ab, DBfun$Excavated_soil_weight, method = "spearman",exact=F) 
cor.test(DBfun$ab, DBfun$Dung_removed, method = "spearman",exact=F) 
cor.test(DBfun$ab, DBfun$Small_dispersed, method = "spearman",exact=F) 
cor.test(DBfun$ab, DBfun$Medium_dispersed, method = "spearman",exact=F) 
cor.test(DBfun$ab, DBfun$Large_dispersed, method = "spearman",exact=F) 
cor.test(DBfun$ab, DBfun$mean_dispersal_Small, method = "spearman",exact=F) 
cor.test(DBfun$ab, DBfun$mean_dispersal_Medium, method = "spearman",exact=F) 
cor.test(DBfun$ab, DBfun$mean_dispersal_Large, method = "spearman",exact=F) 


###
###
### Old code I haven't deleted ----
# # Dung removal 
# m0 <- lm(Dung_removed~1)
# summary(m0)
# 
# m1<- glm(Dung_removed~Temp07mean+Hum07mean +Rain_mean, family=gaussian)
# summary(m1)
# 
# m2<- glm(Dung_removed~Temp07mean+Hum07mean, family=gaussian)
# Ldiff <- logLik(m1)-logLik(m2)  
# 1-pchisq(2*(Ldiff),1) # 0.06679958 (df=5)(df=5) rain not quite significant
# 
# m3<- glm(Dung_removed~Temp07mean, family=gaussian)
# Ldiff <- logLik(m2)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 0.07477056 (df=4) Hum not significant
# 
# m4<- glm(Dung_removed~Temp07mean+Rain_mean, family=gaussian)
# 
# m5<- glm(Dung_removed~Rain_mean, family=gaussian)
# Ldiff <- logLik(m4)-logLik(m5)  
# 1-pchisq(2*(Ldiff),1) #0.5728953 (df=4) # remove temp
# 
# m6<- glm(Dung_removed~Rain_mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m6)-logLik(m5)  
# 1-pchisq(2*(Ldiff),2) #0.8182361 (df=5) not significant
# 
# m7<- glm(Dung_removed~Hum07mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m7)-logLik(m2)  
# 1-pchisq(2*(Ldiff),1) # 0.8831449 (df=5)
# 
# m8<- glm(Dung_removed~Hum07mean+Rain_mean, family=gaussian)
# m9<- glm(Dung_removed~Hum07mean*Rain_mean, family=gaussian)
# Ldiff <- logLik(m9)-logLik(m8)  
# 1-pchisq(2*(Ldiff),1) #log Lik.' 0.5264657 (df=5)
# 
# ## BEST weather/functons model= 
# m5<- glm(Dung_removed~Rain_mean, family=gaussian)
# summary(m5) # strongest effect from rain_mean, but insignificant - borderline effect of interaction 
# # with temp, but no difference between AIC so chose simpler model
# Ldiff <- logLik(m5)-logLik(m0)  
# 1-pchisq(2*(Ldiff),1) # p=0.04155909 (df=3) adj r sq 0.05106 
# 
# 
# # Soil excavation 
# m0 <- lm(Excavated_soil_weight~1)
# summary(m0)
# 
# m1<- glm(Excavated_soil_weight~Temp07mean+Hum07mean +Rain_mean, family=gaussian)
# summary(m1)
# 
# m2<- glm(Excavated_soil_weight~Temp07mean+Hum07mean, family=gaussian)
# Ldiff <- logLik(m1)-logLik(m2)  
# 1-pchisq(2*(Ldiff),1) #  0.06359054(df=5) rain not significant
# 
# m3<- glm(Excavated_soil_weight~Temp07mean, family=gaussian)
# Ldiff <- logLik(m2)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 0.6414527  (df=4) Hum not significant
# 
# m4<- glm(Excavated_soil_weight~Temp07mean+Rain_mean, family=gaussian)
# 
# m5<- glm(Excavated_soil_weight~Rain_mean, family=gaussian)
# Ldiff <- logLik(m4)-logLik(m5)  
# 1-pchisq(2*(Ldiff),1) #0.4349386 (df=4) # remove temp
# 
# m6<- glm(Excavated_soil_weight~Rain_mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m6)-logLik(m4)  
# 1-pchisq(2*(Ldiff),1) #0.7853989 (df=5) nearly significant
# 
# m7<- glm(Excavated_soil_weight~Hum07mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m7)-logLik(m2)  
# 1-pchisq(2*(Ldiff),1) # 0.8446541(df=5)
# 
# m8<- glm(Excavated_soil_weight~Hum07mean+Rain_mean, family=gaussian)
# m9<- glm(Excavated_soil_weight~Hum07mean*Rain_mean, family=gaussian)
# Ldiff <- logLik(m9)-logLik(m8)  
# 1-pchisq(2*(Ldiff),1) #log Lik.' 0.5571731 (df=5)
# 
# ## BEST weather/functons model= 
# m5<- glm(Excavated_soil_weight~Rain_mean, family=gaussian)
# summary(m5) # strongest effect from rain_mean, but insignificant - borderline effect of interaction 
# # with temp, but no difference between AIC so chose simpler model
# Ldiff <- logLik(m5)-logLik(m0)  
# 1-pchisq(2*(Ldiff),1) # p=0.2292595  (df=3) 
# 
# 
# # Small_beads_dispersed 
# m0 <- lm(Small_count_dispersed~1)
# summary(m0) 
# 
# m1<- glm(Small_count_dispersed~Temp07mean+Hum07mean +Rain_mean, family=gaussian)
# summary(m1)
# 
# m2<- glm(Small_count_dispersed~Temp07mean+Hum07mean, family=gaussian)
# Ldiff <- logLik(m1)-logLik(m2)  
# 1-pchisq(2*(Ldiff),1) #  0.1856153(df=5) rain not significant
# 
# m3<- glm(Small_count_dispersed~Temp07mean, family=gaussian)
# Ldiff <- logLik(m2)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 0.207282 (df=4) Hum not significant
# 
# m4<- glm(Small_count_dispersed~Temp07mean+Rain_mean, family=gaussian)
# 
# m5<- glm(Small_count_dispersed~Rain_mean, family=gaussian)
# Ldiff <- logLik(m4)-logLik(m5)  
# 1-pchisq(2*(Ldiff),1)  # 0.02662028 (df=4) # temp significant?
# 
# m6<- glm(Small_count_dispersed~Rain_mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m6)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 0.1073088 (df=5) 
# 
# m7<- glm(Small_count_dispersed~Hum07mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m7)-logLik(m2)  
# 1-pchisq(2*(Ldiff),1) # 0.4660973 (df=5)
# 
# m8<- glm(Small_count_dispersed~Temp07mean+Hum07mean*Rain_mean, family=gaussian)
# Ldiff <- logLik(m9)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) #log Lik.' 1 (df=5)
# 
# ## BEST weather model= 
# m3<- glm(Small_count_dispersed~Temp07mean, family=gaussian)
# summary(m3) # strongest effect from rain_mean, but insignificant - borderline effect of interaction 
# # with temp, but no difference between AIC so chose simpler model
# Ldiff <- logLik(m3)-logLik(m0)  
# 1-pchisq(2*(Ldiff),1) #0.0282022  (df=3)
# 
# ##
# #
# ## Medium_beads_dispersed 
# m0 <- lm(Medium_count_dispersed~1)
# summary(m0) 
# 
# m1<- glm(Medium_count_dispersed~Temp07mean+Hum07mean +Rain_mean, family=gaussian)
# summary(m1)
# 
# m2<- glm(Medium_count_dispersed~Temp07mean+Hum07mean, family=gaussian)
# Ldiff <- logLik(m1)-logLik(m2)  
# 1-pchisq(2*(Ldiff),1) #  0.3427905 rain not significant
# 
# m3<- glm(Medium_count_dispersed~Temp07mean, family=gaussian)
# Ldiff <- logLik(m2)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 0.08550825 (df=4) Hum not significant
# 
# m4<- glm(Medium_count_dispersed~Temp07mean+Rain_mean, family=gaussian)
# 
# m5<- glm(Medium_count_dispersed~Rain_mean, family=gaussian)
# Ldiff <- logLik(m4)-logLik(m5)  
# 1-pchisq(2*(Ldiff),1)  # 0.07847052 (df=4) # temp not significant
# 
# m6<- glm(Medium_count_dispersed~Rain_mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m6)-logLik(m4)  
# 1-pchisq(2*(Ldiff),1) # 0.1073088 (df=5) 
# 
# m7<- glm(Medium_count_dispersed~Hum07mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m7)-logLik(m3)  
# 1-pchisq(2*(Ldiff),2) # 0.2261504  (df=5)
# 
# m8<- glm(Medium_count_dispersed~Temp07mean+Hum07mean*Rain_mean, family=gaussian)
# Ldiff <- logLik(m9)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) #log Lik.' 1 (df=5)
# 
# ## BEST weather model= 
# m3<- glm(Medium_count_dispersed~Temp07mean, family=gaussian)
# summary(m3) # strongest effect from rain_mean, but insignificant - borderline effect of interaction 
# # with temp, but no difference between AIC so chose simpler model
# Ldiff <- logLik(m3)-logLik(m0)  
# 1-pchisq(2*(Ldiff),1) #p0.08068576(df=3)
# 
# # NO significant relationship between medium bead dispersal and weather
# 
# ## 
# #
# ## Large count dispersed
# m0 <- lm(Large_count_dispersed~1)
# summary(m0) 
# 
# m1<- glm(Large_count_dispersed~Temp07mean+Hum07mean +Rain_mean, family=gaussian)
# summary(m1)
# 
# m2<- glm(Large_count_dispersed~Temp07mean+Hum07mean, family=gaussian)
# Ldiff <- logLik(m1)-logLik(m2)  
# 1-pchisq(2*(Ldiff),1) #  0.2803172 (df=5) rain not significant
# 
# m3<- glm(Large_count_dispersed~Temp07mean, family=gaussian)
# Ldiff <- logLik(m2)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 0.2667621 df=4) Hum not significant
# 
# m4<- glm(Large_count_dispersed~Temp07mean+Rain_mean, family=gaussian)
# 
# m5<- glm(Large_count_dispersed~Rain_mean, family=gaussian)
# Ldiff <- logLik(m4)-logLik(m5)  
# 1-pchisq(2*(Ldiff),1)  # 0.05027358(df=4) # temp not significant
# 
# m6<- glm(Large_count_dispersed~Rain_mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m6)-logLik(m4)  
# 1-pchisq(2*(Ldiff),1) # 0.1167248 (df=5(df=5) 
# 
# m7<- glm(Large_count_dispersed~Hum07mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m7)-logLik(m3)  
# 1-pchisq(2*(Ldiff),2) # 0.5237506 (d (df=5)
# 
# m8<- glm(Large_count_dispersed~Temp07mean+Hum07mean*Rain_mean, family=gaussian)
# Ldiff <- logLik(m9)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) #log Lik.' 1 (df=5)
# 
# ## BEST weather model= 
# m3<- glm(Large_count_dispersed~Temp07mean, family=gaussian)
# summary(m3) # strongest effect from rain_mean, but insignificant - borderline effect of interaction 
# # with temp, but no difference between AIC so chose simpler model
# Ldiff <- logLik(m3)-logLik(m0)  
# 1-pchisq(2*(Ldiff),1) #0.05202936 (df=3)
# 
# 
# 
# ##
# #
# ## mean_dispersal_Small 
# m0 <- lm(mean_dispersal_Small~1)
# summary(m0) 
# 
# m1<- glm(mean_dispersal_Small~Temp07mean+Hum07mean +Rain_mean, family=gaussian)
# summary(m1)
# 
# m2<- glm(mean_dispersal_Small~Temp07mean+Hum07mean, family=gaussian)
# Ldiff <- logLik(m1)-logLik(m2)  
# 1-pchisq(2*(Ldiff),1) #  0.2217908 rain not significant
# 
# m3<- glm(mean_dispersal_Small~Temp07mean, family=gaussian)
# Ldiff <- logLik(m2)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) #'log Lik.' 0.7690955 Hum not significant
# 
# m4<- glm(mean_dispersal_Small~Temp07mean+Rain_mean, family=gaussian)
# 
# m5<- glm(mean_dispersal_Small~Rain_mean, family=gaussian)
# Ldiff <- logLik(m4)-logLik(m5)  
# 1-pchisq(2*(Ldiff),1)  # 0.6668113 (df=4) # 
# 
# m6<- glm(mean_dispersal_Small~Rain_mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m6)-logLik(m4)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 0.6239197 (df=5)
# 
# m7<- glm(mean_dispersal_Small~Hum07mean*Temp07mean, family=gaussian)
# Ldiff <- logLik(m7)-logLik(m2)  
# 1-pchisq(2*(Ldiff),2) # 0.1730087 (df=5)
# 
# m8<- glm(mean_dispersal_Small~Temp07mean+Hum07mean*Rain_mean, family=gaussian)
# Ldiff <- logLik(m8)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) #0.4555953 (df=6)
# 
# ## BEST weather model= 
# m3<- glm(mean_dispersal_Small~Temp07mean, family=gaussian)
# summary(m3) # strongest effect from rain_mean, but insignificant - borderline effect of interaction 
# # with temp, but no difference between AIC so chose simpler model
# Ldiff <- logLik(m3)-logLik(m0)  
# 1-pchisq(2*(Ldiff),1) #0.9211399(df=3) No effect of weather 
# 
# ###
# # Random effect included for date and site, as four dung pats
# # were studied at each site, over two days
# 
# library(lme4)
# library(lmerTest)
# 
# ## ECOSYSTEM FUNCTIONS IN RESPONSE TO DISTURBANCE
# 
# ## DUNG REMOVAL 
# 
# m0 <- lmer(Dung_removed~(1|Site/Arena)+(1|Date_checked), REML = FALSE)
# summary(m0)
# 
# m1 <- lmer(Dung_removed~Rank+(1|Site/Arena)+(1|Date_checked), REML = FALSE)
# summary(m1)
# Ldiff <- logLik(m1)-logLik(m0)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 0.03878615  (df=6) # 
# 
# m2 <- lmer(Dung_removed~Rank+River_dist+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m2)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 1 (df=6) #reject river dist
# 
# m3 <- lmer(Dung_removed~Rank+Rain_mean+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m3)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) #  0.06312818  (df=6) #reject rain
# 
# m4 <- lmer(Dung_removed~Rank+Elevation+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m4)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 1 (df=6) #reject elevation
# 
# m5 <- lmer(Dung_removed~Rank*Elevation+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m5)-logLik(m4)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 1 (df=7)  #reject elevation*rank interaction
# 
# m6 <- lmer(Dung_removed~Rank+River_dist+Rank*log(River_dist)+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m6)-logLik(m1)  
# 1-pchisq(2*(Ldiff),2) # 0.0001427995 (df=8)  #interaction ? #*#*#
# 
# m7 <- lmer(Dung_removed~Rank*Rain_mean+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m7)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 0.1827076 (df=7) #no interaction with rain
# 
# #Best model
# m6 <- lmer(Dung_removed~Rank+River_dist+Rank*log(River_dist)+(1|Site/Arena)+(1|Date_checked))
# summary(m6)
# plot(Dung_removed~Rank)
# plot(Dung_removed~River_dist)
# anova(m6)
# 
#  ## NO effect of rank on dung removal
# ?ranef
# str(rr1 <- ranef(m6, condVar = TRUE))
# library(lattice)
# dotplot(rr1) # apparently useful plot but I don't know what it does.
# 
# 
# 
# ## SOIL EXCAVATION
# m0 <- lmer(Excavated_soil_weight~(1|Site/Arena)+(1|Date_checked))
# summary(m0)
# 
# m1 <- lmer(Excavated_soil_weight~Rank+(1|Site/Arena)+(1|Date_checked))
# summary(m1)
# Ldiff <- logLik(m1)-logLik(m0)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.'  0.009685993 (df=6) # rank significant
# 
# m2 <- lmer(Excavated_soil_weight~Rank+River_dist+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m2)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 1 (df=6) #reject river dist
# 
# m3 <- lmer(Excavated_soil_weight~Rank+Rain_mean+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m3)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) #  0.01385894 (df=7) #keep rain
# 
# m4 <- lmer(Excavated_soil_weight~Rank+Rain_mean+Elevation+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m4)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 0.07156507 (df=8) #reject elevation
# 
# m5 <- lmer(Excavated_soil_weight~Rank+Rain_mean+Rank*log(Elevation)+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m5)-logLik(m4)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.'  0.1635265 (df=9)  #reject elevation*rank interaction
# #  with log(Elev) p= 3.295982e-07 (df=9)
# 
# 
# m6.0 <- lmer(Excavated_soil_weight~Rank+Rain_mean+River_dist+(1|Site/Arena)+(1|Date_checked))
# m6.1 <- lmer(Excavated_soil_weight~Rank+Rain_mean+River_dist+Rank*log(River_dist)+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m6.1)-logLik(m6.0)  
# 1-pchisq(2*(Ldiff),3) # 0.0002497465 (df=10)  #interaction 
# 
# m7 <- lmer(Excavated_soil_weight~Rank+Rain_mean+Rank*Rain_mean+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m7)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 0.1107829  (df=8) no interaction with rain
# 
# m8 <- lmer(Excavated_soil_weight~River_dist+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m6.1)-logLik(m9)  
# 1-pchisq(2*(Ldiff),4) #'2.373405e-06 (df=10) Rank improves model
# 
# #Best model
# m8 <- lmer(Excavated_soil_weight~Rank+Rank*log(River_dist)+(1|Site/Arena)+(1|Date_checked))
# summary(m8)
# plot(Excavated_soil_weight~Rank)
# plot(Excavated_soil_weight~River_dist)
# plot(Excavated_soil_weight~Rain_mean)
# anova(m8) 
# # No significant effect of disturbance rank on soil excavation
# 
# 
# 
# ## Small beads dispersed
# 
# m0 <- lmer(Small_count_dispersed~(1|Site/Arena)+(1|Date_checked))
# summary(m0)
# 
# m1 <- lmer(Small_count_dispersed~Rank+(1|Site/Arena)+(1|Date_checked))
# summary(m1)
# Ldiff <- logLik(m1)-logLik(m0)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.'  0.03378018 (df=6) #rank improves model
# 
# m2 <- lmer(Small_count_dispersed~Rank+River_dist+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m2)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 1 (df=6) #reject river dist
# 
# m3 <- lmer(Small_count_dispersed~Rank+Temp07mean+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m3)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) #  0.02326218 (df=7) keep temp
# 
# m4 <- lmer(Small_count_dispersed~Rank+Temp07mean+Elevation+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m4)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 1 (df=8) #reject elevation
# 
# m5 <- lmer(Small_count_dispersed~Rank+Temp07mean+Rank*Elevation+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m5)-logLik(m4)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.'  1 (df=9)  #reject elevation*rank interaction
# 
# m6.0 <- lmer(Small_count_dispersed~Rank+Temp07mean+River_dist+(1|Site/Arena)+(1|Date_checked))
# m6.1 <- lmer(Small_count_dispersed~Rank+Temp07mean+Rank*log(River_dist)+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m6.1)-logLik(m6.0)  
# 1-pchisq(2*(Ldiff),1) #  4.389661e-06 (df=9)  #interaction 
# 
# m7 <- lmer(Small_count_dispersed~Rank*Temp07mean+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m7)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 0.06683838 (df=8)  no interaction with temp
# 
# m8 <- lmer(Small_count_dispersed~Temp07mean+River_dist+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m6.1)-logLik(m8)  
# 1-pchisq(2*(Ldiff),2) #'3.262576e-06 (df=9) Rank  does improve the model
# 
# #Best model
# m6.1 <- lmer(Small_count_dispersed~Rank+Temp07mean+Rank*log(River_dist)+(1|Site/Arena)+(1|Date_checked))
# summary(m6.1)
# plot(Small_count_dispersed~Rank)
# plot(Small_count_dispersed~River_dist)
# plot(Small_count_dispersed~Temp07mean)
# anova(m6.1) 
# # ?Significant effect of disturbance rank on small count dispersed - if you use LLdif, not summary(M)
# # depends on usign LLdiff or anova(m)
# 
# #*~*~*# NEXT STEP: run lmer(medium count dispersed~rank), and also figure out selection process -  not really sure how to tell if something is really significant or what p values to use!
# 
# ## Medium beads dispersed
# 
# m0 <- lmer(Medium_count_dispersed~(1|Site/Arena)+(1|Date_checked))
# summary(m0)
# 
# m1 <- lmer(Medium_count_dispersed~Rank+(1|Site/Arena)+(1|Date_checked))
# summary(m1)
# Ldiff <- logLik(m1)-logLik(m0)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.'  0.072485 (df=6) #rank not improves model
# 
# m2 <- lmer(Medium_count_dispersed~Rank+River_dist+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m2)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 1 (df=6) #reject river dist
# 
# m3 <- lmer(Medium_count_dispersed~Rank+Temp07mean+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m3)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) #  0.06 (df=7) 
# 
# m4 <- lmer(Medium_count_dispersed~Rank+Elevation+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m4)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 1 (df=8) #reject elevation
# 
# m5 <- lmer(Medium_count_dispersed~Rank*Elevation+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m5)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.'  1 (df=9)  #reject elevation*rank interaction
# 
# m6 <- lmer(Medium_count_dispersed~Rank*log(River_dist)+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m6)-logLik(m2)  
# 1-pchisq(2*(Ldiff),1) #  4.389661e-06 (df=9)  #interaction 
# 
# m7 <- lmer(Medium_count_dispersed~Rank*Temp07mean+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m7)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 0.17 (df=8)  no interaction with temp
# 
# m8 <- lmer(Medium_count_dispersed~River_dist+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m6)-logLik(m8)  
# 1-pchisq(2*(Ldiff),2) #'3.262576e-06 (df=9) Rank  does improve the model
# 
# #Best model
# m6 <- lmer(Medium_count_dispersed~Rank+Rank*log(River_dist)+(1|Site/Arena)+(1|Date_checked))
# summary(m6)
# plot(Medium_count_dispersed~Rank)
# plot(Medium_count_dispersed~River_dist)
# plot(Medium_count_dispersed~Temp07mean)
# anova(m6) 
# 
# 
# ## Large beads dispersed
# 
# m0 <- lmer(Large_count_dispersed~(1|Site/Arena)+(1|Date_checked))
# summary(m0)
# 
# m1 <- lmer(Large_count_dispersed~Rank+(1|Site/Arena)+(1|Date_checked))
# summary(m1)
# Ldiff <- logLik(m1)-logLik(m0)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.'  0.5800527(df=6) #rank not improves model
# 
# m2 <- lmer(Large_count_dispersed~Rank+River_dist+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m2)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 1 (df=6) #reject river dist
# 
# m3 <- lmer(Large_count_dispersed~Rank+Temp07mean+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m3)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) #  0.14 (df=7) 
# 
# m4 <- lmer(Large_count_dispersed~Rank+Elevation+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m4)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.' 1 (df=8) #reject elevation
# 
# m5 <- lmer(Large_count_dispersed~Rank*Elevation+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m5)-logLik(m1)  
# 1-pchisq(2*(Ldiff),1) # 'log Lik.'  1 (df=9)  #reject elevation*rank interaction
# 
# m6 <- lmer(Large_count_dispersed~Rank*log(River_dist)+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m6)-logLik(m2)  
# 1-pchisq(2*(Ldiff),1) #  4.389661e-06 (df=9)  #interaction 
# 
# m7 <- lmer(Large_count_dispersed~Rank*Temp07mean+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m7)-logLik(m3)  
# 1-pchisq(2*(Ldiff),1) # 0.17 (df=8)  no interaction with temp
# 
# m8 <- lmer(Large_count_dispersed~River_dist+(1|Site/Arena)+(1|Date_checked))
# Ldiff <- logLik(m6)-logLik(m8)  
# 1-pchisq(2*(Ldiff),2) #'3.262576e-06 (df=9) Rank  does improve the model
# 
# #Best model
# m6 <- lmer(Large_count_dispersed~Rank+(1|Site/Arena)+(1|Date_checked))
# summary(m6)
# plot(Large_count_dispersed~Rank)
# plot(Large_count_dispersed~River_dist)
# plot(Large_count_dispersed~Temp07mean)
# anova(m6) 
# 
# 
# ###
# # functions against rank
# 
# plot(Dung_end_weight~Rank)
# m <- glm(Dung_end_weight~Rank)
# summary(m)
# abline(m)
# 
# 
# plot(mean_dispersal_Medium~Rank)
# m <- glm(mean_dispersal_Medium~Rank)
# summary(m)
# abline(m)
# 
# plot(mean_dispersal_Medium~Rank)
# m <- glm(mean_dispersal_Medium~Rank)
# summary(m)
# abline(m)
# 
# plot(Medium_count_remain~Rank)
# m <- glm(Medium_count_remain~Rank)
# summary(m)
# abline(m)
# 
# png(file="C://Data/PhD/Outputs/Dungbeetles/Functions/Small_dispersal.png",width=5,height=3.25,units="in",res=180)  
# plot(Small_count_dispersed~Rank,pch=21,bg="mediumseagreen",cex.main=0.9,cex=1.5,bty="n", cex.axis=0.7,cex.lab=0.8,
#      xlim=c(1,6),ylim=c(0,50),main="Dispersal of small beads",ylab="Number of beads dispersed",xlab="Disturbance rank")
# m <- glm(Small_count_dispersed~Rank)
# abline(m,col="mediumseagreen",lwd=3)
# dev.off()
# 
# m <- glm(Large_count_remain~Rank)
# summary(m)
# 
# 
# 
# 
# ###
# # functions against diversity
# 
# 
# head(DBfun,1)
# 
# # Dung end weight: q0 and q1 close to significant, nothing else
# 
# plot(Dung_end_weight~Abund_total)
# m <- glm(Dung_end_weight~Abund_total)
# summary(m)
# abline(m)
# 
# plot(Dung_end_weight~Abund_roll)
# m <- glm(Dung_end_weight~Abund_roll)
# summary(m)
# abline(m)
# 
# plot(Dung_end_weight~Abund_tunn)
# m <- glm(Dung_end_weight~Abund_tunn)
# summary(m)
# abline(m)
# 
# plot(Dung_end_weight~q0)
# m <- glm(Dung_end_weight~q0)
# summary(m)
# abline(m) # Obs richness borderline signifcant - est-1.8287   p0.0615 .  
# 
# plot(Dung_end_weight~q1)
# m <- glm(Dung_end_weight~q1)
# summary(m)
# abline(m) # nearly significant est=-2.843    p= 0.0734
# 
# plot(Dung_end_weight~q2)
# m <- glm(Dung_end_weight~q2)
# summary(m)
# abline(m)
# 
# plot(Dung_end_weight~qInf)
# m <- glm(Dung_end_weight~qInf)
# summary(m)
# abline(m)
# 
# plot(Dung_end_weight~ICE_Mean)
# m <- glm(Dung_end_weight~ICE_Mean)
# summary(m)
# abline(m)
# 
# plot(Dung_end_weight~Chao_1_Mean)
# m <- glm(Dung_end_weight~Chao_1_Mean)
# summary(m)
# abline(m)
# 
# 
# # beads dispersal distance: MED q0 and q1 close to significant, nothing else
# 
# plot(mean_dispersal_Medium~Abund_total)
# m <- glm(mean_dispersal_Medium~Abund_total)
# summary(m)
# abline(m)
# 
# plot(mean_dispersal_Medium~Abund_roll)
# m <- glm(mean_dispersal_Medium~Abund_roll)
# summary(m)
# abline(m)
# 
# plot(mean_dispersal_Medium~Abund_tunn)
# m <- glm(mean_dispersal_Medium~Abund_tunn)
# summary(m)
# abline(m)
# 
# plot(mean_dispersal_Medium~q0)
# m <- glm(mean_dispersal_Medium~q0)
# summary(m)
# abline(m) # Obs richness borderline signifcant - est-0.8947  p=0.0546
# 
# plot(mean_dispersal_Medium~q1)
# m <- glm(mean_dispersal_Medium~q1)
# summary(m)
# abline(m) # nearly significant est=1.2751  p=0.0923 .
# 
# 
# plot(mean_dispersal_Medium~q2)
# m <- glm(mean_dispersal_Medium~q2)
# summary(m)
# abline(m)
# 
# plot(mean_dispersal_Medium~qInf)
# m <- glm(mean_dispersal_Medium~qInf)
# summary(m)
# abline(m)
# 
# plot(mean_dispersal_Medium~ICE_Mean)
# m <- glm(mean_dispersal_Medium~ICE_Mean)
# summary(m)
# abline(m)
# 
# plot(mean_dispersal_Medium~Chao_1_Mean)
# m <- glm(mean_dispersal_Medium~Chao_1_Mean)
# summary(m)
# abline(m)
# 
# # Beads remaining in dung: 
# # for count_remain_all, obs richness was nearly significants
# # For large and medium, Obs richness signifcant 
# # Large:  -0.2803    p=0.0106
# # Med= est-0.5946    p=0.00736
# # Small  -1.3671     p=0.00517
# 
# # Add a new column for bead dispersal of all sizes and counts of all sizes
# head(DBfun)
# count_remain_all <- Large_count_remain+Medium_count_remain+Small_count_remain
# DBfun <- cbind(DBfun,count_remain_all=count_remain_all)
# 
# plot(Small_count_remain~Abund_total)
# m <- glm(Small_count_remain~Abund_total)
# summary(m)
# abline(m)
# 
# plot(Small_count_remain~Abund_roll)
# m <- glm(Small_count_remain~Abund_roll)
# summary(m)
# abline(m)
# 
# plot(Small_count_remain~Abund_tunn)
# m <- glm(Small_count_remain~Abund_tunn)
# summary(m)
# abline(m)
# 
# plot(Small_count_remain~q0)
# m <- glm(Small_count_remain~q0)
# summary(m)
# abline(m) # Obs richness signifcant - est-0.5946    p=0.00736
# 
# plot(Small_count_remain~q1)
# m <- glm(Small_count_remain~q1)
# summary(m)
# abline(m) 
# 
# 
# plot(Small_count_remain~q2)
# m <- glm(Small_count_remain~q2)
# summary(m)
# abline(m)
# 
# plot(Small_count_remain~qInf)
# m <- glm(Small_count_remain~qInf)
# summary(m)
# abline(m)
# 
# plot(Small_count_remain~ICE_Mean)
# m <- glm(Small_count_remain~ICE_Mean)
# summary(m)
# abline(m)
# 
# plot(Small_count_remain~Chao_1_Mean)
# m <- glm(Small_count_remain~Chao_1_Mean)
# summary(m)
# abline(m)
# 
# # Soil excavation # No significant relationship with diversity
# 
# plot(Excavated_soil__weight~Abund_total)
# m <- glm(Excavated_soil__weight~Abund_total)
# summary(m)
# abline(m)
# 
# plot(Excavated_soil__weight~Abund_roll)
# m <- glm(Excavated_soil__weight~Abund_roll)
# summary(m)
# abline(m)
# 
# plot(Excavated_soil__weight~Abund_tunn)
# m <- glm(Excavated_soil__weight~Abund_tunn)
# summary(m)
# abline(m)
# 
# plot(Excavated_soil__weight~q0)
# m <- glm(Excavated_soil__weight~q0)
# summary(m)
# abline(m) 
# 
# plot(Excavated_soil__weight~q1)
# m <- glm(Excavated_soil__weight~q1)
# summary(m)
# abline(m) 
# 
# 
# plot(Excavated_soil__weight~q2)
# m <- glm(Excavated_soil__weight~q2)
# summary(m)
# abline(m)
# 
# plot(Excavated_soil__weight~qInf)
# m <- glm(Excavated_soil__weight~qInf)
# summary(m)
# abline(m)
# 
# plot(Excavated_soil__weight~ICE_Mean)
# m <- glm(Excavated_soil__weight~ICE_Mean)
# summary(m)
# abline(m)
# 
# plot(Excavated_soil__weight~Chao_1_Mean)
# m <- glm(Excavated_soil__weight~Chao_1_Mean)
# summary(m)
# abline(m)
# 
# 
# ###
# # Why is observed richness a possibly good predictor, but not estimated?
# 
# # how much do observed and est richness differ in patterns?
# 
# plot(q0~Rank,pch=16,col="blue",main="q0 Observed richness (blue)
#      Chao1 estimates (red)
#      ICE estimates (green)
#      ACE estimates (violet)*lower than obs?!",
#      cex.main=0.8)
# points(Chao_1_Mean~Rank,pch=16,col="red")
# points(ICE_Mean~Rank,pch=16,col="green")
# points(ACE_Mean~Rank,pch=16,col="violet")
# m1 <- glm(q0~Rank)
# m2 <- glm(Chao_1_Mean~Rank)
# m3 <- glm(ICE_Mean~Rank)
# m4<- glm(ACE_Mean~Rank)
# abline(m1,col="blue")
# abline(m2,col="red")
# abline(m3,col="green")
# abline(m4,col="violet")
# 

## Combining all seed sizes ----
### Correlation test
# Rank
cor.test(DBfun$Rank, DBfun$Allbeads_disp, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$Rank), replace = TRUE)
    cor(DBfun$Rank[boot.i], DBfun$Allbeads_disp[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))


#richness
cor.test(DBfun$q0, DBfun$Allbeads_disp, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DBfun$q0), replace = TRUE)
    cor(DBfun$q0[boot.i], DBfun$Allbeads_disp[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
