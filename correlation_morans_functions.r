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

# Correlation test function  ----
#create correlation test function incl. nice output and bootstrap
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