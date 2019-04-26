# Moran's i test for spatial autocorrelation ----
morans <- function(model){
  resids <- model$residuals 
  resdf <- cbind.data.frame(DB$Site,resids,DB$UTM_Coords_S,DB$UTM_Coords_E)
  DB.dists <- as.matrix(dist(cbind(DB$UTM_Coords_E,DB$UTM_Coords_S))) # create distance matrix of site coordinates
  DB.dists.inv <- 1/DB.dists # inverse dist matrix
  diag(DB.dists.inv) <- 0 # replace diagonal with 0s
  DB.dists.inv[1:5, 1:5] # view
  df <- as.data.frame(Moran.I(resdf$resids, DB.dists.inv))
  df$diffobsexp <- df$observed-df$expected
  df}

docor <- function(ia,ib){
  res <- cor.test(ia, ib, method = "spearman",exact=F) # used exact = F to supress warning about ties -  spearman calc uses ties corrected formula I believe. 
  ci <- quantile( # bootstrap to est confidence interval, which helps control for data ties
    replicate(10000, {
      boot.i <- sample(length(ia), replace = TRUE)
      cor(ia[boot.i], ib[boot.i], method = "spearman")
    }), 
    c(0.025, 0.975))
  cor <- cbind(res$statistic,res$p.value,res$estimate,ci[1],ci[2])
  colnames(cor) <- c("S","p","rho","LCI","UCI")
  return(cor)
}
