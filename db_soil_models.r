
###
# SOIL
# Load data----
###
# 
rm(list=ls()) 
library(ape)

DB <- read.csv("C://Data/PhD/Processed_data/Dungbeetles/DB_div_allvars.csv")

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


str(DB)

# soil variables:
# ph
# CE
# CaCO3
# MO
# P
# K
# Al3H


# explore soil  vs disturbance ----
plot(DB$Rank, DB$soil_pH) # pH increases with distrubance
res <- cor.test(DB$Rank, DB$soil_pH, method = "spearman",exact=F) 

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$soil_pH[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(soil_pH)~Rank,data=DB)
logLik(m0)
summary(m0)

m1 <- lm(log(soil_pH)~Rank+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(soil_pH)~Rank+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(soil_pH)~Rank+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

cbind(res$statistic,res$p.value,res$estimate)
rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)

#Moran's i test for spatial autocorrelation
resids <- m0$residuals
moran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Mor <- Moran.I(moran$resids, DB.dists.inv)
cbind(Mor$observed,Mor$expected,Mor$sd,Mor$p.value)


plot(DB$Rank, DB$soil_Al3H) # pH increases with distrubance
cor.test(DB$Rank, DB$soil_Al3H, method = "spearman",exact=F) 

plot(DB$Rank, DB$soil_CE) # pH increases with distrubance
cor.test(DB$Rank, DB$soil_CE, method = "spearman",exact=F) 

plot(DB$Rank, DB$soil_K) # pH increases with distrubance
cor.test(DB$Rank, DB$soil_K, method = "spearman",exact=F) 

plot(DB$Rank, DB$soil_MO) # pH increases with distrubance
cor.test(DB$Rank, DB$soil_MO, method = "spearman",exact=F) 
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$soil_MO[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(soil_MO)~Rank,data=DB)
logLik(m0)
summary(m0)
#Moran's i test for spatial autocorrelation
resids <- m0$residuals
moran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(moran$resids, DB.dists.inv)

m1 <- lm(log(soil_MO)~Rank+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(soil_MO)~Rank+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(soil_MO)~Rank+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

plot(DB$Rank, DB$soil_P) # pH increases with distrubance
cor.test(DB$Rank, DB$soil_P, method = "spearman",exact=F) 
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$Rank), replace = TRUE)
    cor(DB$Rank[boot.i], DB$soil_P[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(soil_P)~Rank,data=DB)
logLik(m0)
summary(m0)
#Moran's i test for spatial autocorrelation
resids <- m0$residuals
moran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(moran$resids, DB.dists.inv)

m1 <- lm(log(soil_P)~Rank+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(soil_P)~Rank+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(soil_P)~Rank+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

# explore soil  vs richness ---- 
plot(DB$e95q0, DB$soil_pH) # pH increases with distrubance
res <- cor.test(DB$e95q0, DB$soil_pH, method = "spearman",exact=F) 
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$e95q0), replace = TRUE)
    cor(DB$e95q0[boot.i], DB$soil_pH[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(soil_pH)~e95q0,data=DB)
logLik(m0)
summary(m0)

m1 <- lm(log(soil_pH)~e95q0+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(soil_pH)~e95q0+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(soil_pH)~e95q0+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

cbind(res$statistic,res$p.value,res$estimate)
rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)

#Moran's i test for spatial autocorrelation
resids <- m0$residuals
moran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Mor <- Moran.I(moran$resids, DB.dists.inv)
cbind(Mor$observed,Mor$expected,Mor$sd,Mor$p.value)



plot(DB$e95q0, DB$soil_Al3H) # pH increases with distrubance
res <- cor.test(DB$e95q0, DB$soil_Al3H, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

plot(DB$e95q0, DB$soil_CE) # pH increases with distrubance
cor.test(DB$e95q0, DB$soil_CE, method = "spearman",exact=F) 

plot(DB$e95q0, DB$soil_K) # pH increases with distrubance
res <- cor.test(DB$e95q0, DB$soil_K, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

plot(DB$e95q0, DB$soil_MO) # pH increases with distrubance
res <- cor.test(DB$e95q0, DB$soil_MO, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$e95q0), replace = TRUE)
    cor(DB$e95q0[boot.i], DB$soil_MO[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))

m0 <- lm(log(soil_MO)~e95q0,data=DB)
logLik(m0)
summary(m0)


m1 <- lm(log(soil_MO)~e95q0+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(soil_MO)~e95q0+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(soil_MO)~e95q0+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)

#Moran's i test for spatial autocorrelation
resids <- m0$residuals
moran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Mor <- Moran.I(moran$resids, DB.dists.inv)
cbind(Mor$observed,Mor$expected,Mor$sd,Mor$p.value)

# P
plot(DB$e95q0, DB$soil_P) # pH increases with distrubance
res <- cor.test(DB$e95q0, DB$soil_P, method = "spearman",exact=F) 
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$e95q0), replace = TRUE)
    cor(DB$e95q0[boot.i], DB$soil_P[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
m0 <- lm(log(soil_P)~e95q0,data=DB)
logLik(m0)
summary(m0)


m1 <- lm(log(soil_P)~e95q0+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(soil_P)~e95q0+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(soil_P)~e95q0+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

cbind(res$statistic,res$p.value,res$estimate)
rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)

#Moran's i test for spatial autocorrelation
resids <- m0$residuals
moran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Mor <- Moran.I(moran$resids, DB.dists.inv)
cbind(Mor$observed,Mor$expected,Mor$sd,Mor$p.value)


# explore soil  vs e95q1
plot(DB$e95q1, DB$soil_pH) # pH increases with distrubance
res <- cor.test(DB$e95q1, DB$soil_pH, method = "spearman",exact=F) 
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$e95q1), replace = TRUE)
    cor(DB$e95q1[boot.i], DB$soil_pH[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
m0 <- lm(log(soil_pH)~e95q1,data=DB)
logLik(m0)
summary(m0)
#Moran's i test for spatial autocorrelation
resids <- m0$residuals
moran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(moran$resids, DB.dists.inv)

m1 <- lm(log(soil_pH)~e95q1+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(soil_pH)~e95q1+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(soil_pH)~e95q1+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

cbind(res$statistic,res$p.value,res$estimate)
rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)

#Moran's i test for spatial autocorrelation
resids <- m0$residuals
moran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Mor <- Moran.I(moran$resids, DB.dists.inv)
cbind(Mor$observed,Mor$expected,Mor$sd,Mor$p.value)


plot(DB$e95q1, DB$soil_Al3H) # pH increases with distrubance
cor.test(DB$e95q1, DB$soil_Al3H, method = "spearman",exact=F) 

plot(DB$e95q1, DB$soil_CE) # pH increases with distrubance
cor.test(DB$e95q1, DB$soil_CE, method = "spearman",exact=F) 

plot(DB$e95q1, DB$soil_K) # pH increases with distrubance
res <- cor.test(DB$e95q1, DB$soil_K, method = "spearman",exact=F) 
cbind(res$statistic,res$p.value,res$estimate)

#MO
plot(DB$e95q1, DB$soil_MO) # pH increases with distrubance
res <- cor.test(DB$e95q1, DB$soil_MO, method = "spearman",exact=F) 
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$e95q1), replace = TRUE)
    cor(DB$e95q1[boot.i], DB$soil_MO[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
m0 <- lm(log(soil_MO)~e95q1,data=DB)
logLik(m0)
summary(m0)


m1 <- lm(log(soil_MO)~e95q1+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(soil_MO)~e95q1+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(soil_MO)~e95q1+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

cbind(res$statistic,res$p.value,res$estimate)
rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)

#Moran's i test for spatial autocorrelation
resids <- m0$residuals
moran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Mor <- Moran.I(moran$resids, DB.dists.inv)
cbind(Mor$observed,Mor$expected,Mor$sd,Mor$p.value)

# P
plot(DB$e95q1, DB$soil_P) # pH increases with distrubance
res <- cor.test(DB$e95q1, DB$soil_P, method = "spearman",exact=F) 
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$e95q1), replace = TRUE)
    cor(DB$e95q1[boot.i], DB$soil_P[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
m0 <- lm(log(soil_P)~e95q1,data=DB)
logLik(m0)
summary(m0)


m1 <- lm(log(soil_P)~e95q1+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(soil_P)~e95q1+River_dist,data=DB) 
anova(m0,m2) 
logLik(m2)

m3 <- lm(log(soil_P)~e95q1+Rain_mean,data=DB) 
anova(m0,m3) 
logLik(m3)

cbind(res$statistic,res$p.value,res$estimate)
rbind(logLik(m0),logLik(m1),logLik(m2),logLik(m3))
summary(m0)

#Moran's i test for spatial autocorrelation
resids <- m0$residuals
moran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Mor <- Moran.I(moran$resids, DB.dists.inv)
cbind(Mor$observed,Mor$expected,Mor$sd,Mor$p.value)

# explore soil  vs asEst.q2 ----
plot(DB$asEst.q2, DB$soil_pH) # pH increases with distrubance
cor.test(DB$asEst.q2, DB$soil_pH, method = "spearman",exact=F) 

plot(DB$asEst.q2, DB$soil_Al3H) # pH increases with distrubance
cor.test(DB$asEst.q2, DB$soil_Al3H, method = "spearman",exact=F) 

plot(DB$asEst.q2, DB$soil_CE) # pH increases with distrubance
cor.test(DB$asEst.q2, DB$soil_CE, method = "spearman",exact=F) 

plot(DB$asEst.q2, DB$soil_K) # pH increases with distrubance
cor.test(DB$asEst.q2, DB$soil_K, method = "spearman",exact=F) 

plot(DB$asEst.q2, DB$soil_MO) # pH increases with distrubance
cor.test(DB$asEst.q2, DB$soil_MO, method = "spearman",exact=F) 

plot(DB$asEst.q2, DB$soil_P) # pH increases with distrubance
cor.test(DB$asEst.q2, DB$soil_P, method = "spearman",exact=F) 
# bootstrapped 95% CI
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$asEst.q2), replace = TRUE)
    cor(DB$asEst.q2[boot.i], DB$soil_P[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
m0 <- lm(log(soil_P)~asEst.q2,data=DB)
logLik(m0)
summary(m1)
#Moran's i test for spatial autocorrelation
resids <- m0$residuals
moran <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
DB.dists.inv <- 1/DB.dists # inverse dist matrix
diag(DB.dists.inv) <- 0 # replace diagonal with 0s
DB.dists.inv[1:5, 1:5] # view
Moran.I(moran$resids, DB.dists.inv)

m1 <- lm(log(soil_P)~asEst.q2+Elevation,data=DB) 
anova(m0,m1) 
logLik(m1)

m2 <- lm(log(soil_P)~asEst.q2+Elevation+River_dist,data=DB) 
anova(m1,m2) 
logLik(m2)

m3 <- lm(log(soil_P)~asEst.q2+Elevation+Rain_mean,data=DB) 
anova(m1,m3) 
logLik(m3)

plot(DB$e95q3, DB$soil_P) # pH increases with distrubance
cor.test(DB$e95q3, DB$soil_P, method = "spearman",exact=F)
quantile(
  replicate(10000, {
    boot.i <- sample(length(DB$e95q3), replace = TRUE)
    cor(DB$e95q3[boot.i], DB$soil_P[boot.i], method = "spearman")
  }), 
  c(0.025, 0.975))
#

### old code -------
# 
# # soil vs richness
# plot(Chao_1_Mean, soil_pH) #
# m <- glm(soil_pH~Chao_1_Mean)
# summary(m)
# abline(m)
# 
# plot(Chao_1_Mean,soil_Al3H) #
# m <- glm(soil_Al3H~Chao_1_Mean)
# summary(m)
# abline(m)
# 
# plot(Chao_1_Mean,soil_CE) # 
# m <- glm(soil_CE~Chao_1_Mean)
# summary(m)
# abline(m)
# 
# plot(Chao_1_Mean,soil_K) # 
# m <- glm(soil_K~Chao_1_Mean)
# summary(m)
# abline(m)
# 
# plot(Chao_1_Mean,soil_MO) # 
# m <- glm(soil_MO~Chao_1_Mean)
# summary(m)
# abline(m)
# 
# plot(Chao_1_Mean,soil_P) # 
# m <- glm(soil_P~Chao_1_Mean)
# summary(m)
# abline(m)
# 
# 
# 
# #soil v q1 diversity
# plot(q1, soil_pH) # pH reduced at higher diversity
# m <- glm(soil_pH~q1)
# summary(m)
# abline(m)
# 
# plot(q1,soil_Al3H) #
# m <- glm(soil_Al3H~q1)
# summary(m)
# abline(m)
# 
# plot(q1,soil_CE) # 
# m <- glm(soil_CE~q1)
# summary(m)
# abline(m)
# 
# plot(q1,soil_K) # 
# m <- glm(soil_K~q1)
# summary(m)
# abline(m)
# 
# plot(q1,soil_MO) # MO increases with diversity
# m <- glm(soil_MO~q1)
# summary(m)
# abline(m)
# 
# plot(q1,soil_P) #  P reduced at higher diversity
# m <- glm(soil_P~q1)
# summary(m)
# abline(m)
# 
# 
# 
# 
# 
# 
# #soil v q2 diversity
# plot(q2, soil_pH) # pH reduced at higher diversity
# m <- glm(soil_pH~q2)
# summary(m)
# abline(m)
# 
# plot(q2,soil_Al3H) #
# m <- glm(soil_Al3H~q2)
# summary(m)
# abline(m)
# 
# plot(q2,soil_CE) # 
# m <- glm(soil_CE~q2)
# summary(m)
# abline(m)
# 
# plot(q2,soil_K) # 
# m <- glm(soil_K~q2)
# summary(m)
# abline(m)
# 
# plot(q2,soil_MO) # 
# m <- glm(soil_MO~q2)
# summary(m)
# abline(m)
# 
# plot(q2,soil_P) #  P reduced at higher diversity
# m <- glm(soil_P~q2)
# summary(m)
# abline(m)
# 
# 
# #soil v q1 diversity
# plot(, soil_pH) # pH reduced at higher diversity
# m <- glm(soil_pH~q1)
# summary(m)
# abline(m)
# 
# plot(q1,soil_Al3H) #
# m <- glm(soil_Al3H~q1)
# summary(m)
# abline(m)
# 
# plot(q1,soil_CE) # 
# m <- glm(soil_CE~q1)
# summary(m)
# abline(m)
# 
# plot(q1,soil_K) # 
# m <- glm(soil_K~q1)
# summary(m)
# abline(m)
# 
# plot(q1,soil_MO) # MO increases with diversity
# m <- glm(soil_MO~q1)
# summary(m)
# abline(m)
# 
# plot(q1,soil_P) #  P reduced at higher diversity
# m <- glm(soil_P~q1)
# summary(m)
# abline(m)

## Soil plots----
png(file="C://Data/PhD/Outputs/Dungbeetles/functions/soilq0.png",width=12,height=10,units="in",res=300)  

par(mfrow=c(2,2))
par(mai=c(1.02,1,1,0.6))
plot(soil_pH~q0,pch=19,cex=2,cex.axis=2,cex.lab=2,cex.main=2,col="black",data=DB,main="pH",ylab="pH",xlab="Estimated Richness",ylim=c(0,10))
plot(soil_Al3H~q0,pch=19,cex=2,cex.axis=2,cex.lab=2,cex.main=2,col="black",data=DB,main="Al+3 + H+",ylab="Al+3 + H+ (meq/100)",xlab="Estimated Richness",ylim=c(0,7))
plot(soil_CE~q0,pch=19,cex=2,cex.axis=2,cex.lab=2,cex.main=2,col="black",data=DB,main="Electrical Conductivity",ylab="Conductivity (Ds/m)",xlab="Estimated Richness",ylim=c(0,0.5))
plot(soil_K~q0,pch=19,cex=2,cex.axis=2,cex.lab=2,cex.main=2,col="black",data=DB,main="Potassium (K)",ylab="K (ppm)",xlab="Estimated Richness",ylim=c(0,110))
plot(soil_MO~q0,pch=19,cex=2,cex.axis=2,cex.lab=2,cex.main=2,col="black",data=DB,main="Organic Matter",ylab="Organic Matter (%)",xlab="Estimated Richness",ylim=c(0,5))
plot(soil_P~q0,pch=19,cex=2,cex.axis=2,cex.lab=2,cex.main=2,col="black",data=DB,main="Phosphorus (P)",ylab="P (ppm)",xlab="Estimated Richness",ylim=c(0,20))
dev.off()

#png(file="C://Data/PhD/Outputs/Dungbeetles/functions/soilrank.png",width=12,height=10,units="in",res=300)  
tiff("C://Data/PhD/Outputs/Dungbeetles/db chapter figures/soilrank-jitter.tiff",width=190,height=70,units="mm",res=1000, pointsize=12)  
par(mfrow=c(1,3))
par(mar=c(5,5,4,2))#,1,1,0.6))
plot(soil_MO~Rank,pch=21,bty="n",cex=1,cex.axis=1,cex.lab=1.2,col="black",data=DB,ylab="Organic Matter (%)",xlab="Disturbance Rank",ylim=c(0,5))
mtext(side = 3, line = 0,adj=0,"a",font=2,cex=1.2)
abline(lm(DB$soil_MO~DB$Rank))
plot(soil_P~Rank,pch=21,bty="n",cex=1,cex.axis=1,cex.lab=1.2,col="black",data=DB,ylab="Phosphorus (ppm)",xlab="Disturbance Rank",ylim=c(0,20))
mtext(side = 3, line = 0,adj=0,"b",font=2,cex=1.2)
abline(lm(DB$soil_P~DB$Rank))
plot(soil_K~Rank,pch=21,bty="n",cex=1,cex.axis=1,cex.lab=1.2,col="black",data=DB,ylab="Potassium (ppm)",xlab="Disturbance Rank",ylim=c(40,120))
mtext(side = 3, line = 0,adj=0,"c",font=2,cex=1.2)
abline(lm(DB$soil_K~DB$Rank))
dev.off()

png(file="C://Data/PhD/Outputs/Dungbeetles/functions/soilMO.png",width=14,height=7,units="in",res=250)  
par(mfrow=c(1,2))
par(mai=c(1.02,1,1,0.6))
plot(soil_MO~Rank,pch=19,cex=2,cex.axis=2,cex.lab=2,cex.main=2,col="black",data=DB,ylab="Organic Matter (%)",xlab="Disturbance Rank",ylim=c(0,5))
plot(soil_MO~q0,pch=19,cex=2,cex.axis=2,cex.lab=2,cex.main=2,col="black",data=DB,ylab="Organic Matter (%)",xlab="Estimated Richness",ylim=c(0,5))
dev.off()
