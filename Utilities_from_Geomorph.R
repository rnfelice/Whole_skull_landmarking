
phylo.mat<-function(x,phy){
  C<-vcv.phylo(phy,anc.nodes=FALSE)
  C<-C[rownames(x),rownames(x)]
  invC <-fast.solve(C)
  eigC <- eigen(C)
  lambda <- zapsmall(eigC$values)
  if(any(lambda == 0)){
    warning("Singular phylogenetic covariance matrix. Proceed with caution")
    lambda = lambda[lambda > 0]
  }
  eigC.vect = eigC$vectors[,1:(length(lambda))]
  D.mat <- fast.solve(eigC.vect%*% diag(sqrt(lambda)) %*% t(eigC.vect))
  rownames(D.mat) <- colnames(D.mat) <- colnames(C)
  list(invC = invC, D.mat = D.mat,C = C)
}
​
# fast.ginv
# same as ginv, but without traps (faster)
# used in any function requiring a generalized inverse
fast.ginv <- function(X, tol = sqrt(.Machine$double.eps)){
  k <- ncol(X)
  Xsvd <- La.svd(X, k, k)
  Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  rtu <-((1/Xsvd$d[Positive]) * t(Xsvd$u[, Positive, drop = FALSE]))
  v <-t(Xsvd$vt)[, Positive, drop = FALSE]
  v%*%rtu
}
​
# fast.solve
# chooses between fast.ginv or qr.solve, when det might or might not be 0
# used in any function requiring a matrix inverse where the certainty of
# singular matrices is in doubt; mostly phylo. functions
fast.solve <- function(x) if(det(x) > 1e-8) qr.solve(x) else fast.ginv(x)
​
​
# sigma.d.multi
# multiple trait multivariate evolutionary rates
# used in: compare.multi.evol.rates
sigma.d.multi<-function(x,invC,D.mat,gps,Subset){
  sig.calc<-function(x.i,invC.i,D.mat.i,Subset){
    x.i<-as.matrix(x.i)
    N<-dim(x.i)[1];p<-dim(x.i)[2]
    ones<-matrix(1,N,N)
    x.c<- x.i - crossprod(ones,invC.i)%*%x.i/sum(invC.i)
    R<-crossprod(x.c, crossprod(invC.i,x.c))/N
    if(Subset==FALSE) sigma<-sigma<-sum((D.mat.i%*%x.c)^2)/N  else
      sigma<-sum((D.mat.i%*%x.c)^2)/N/p
    return(list(sigma=sigma,R=R))
  }
  g<-factor(as.numeric(gps))
  ngps<-nlevels(g)
  gps.combo <- combn(ngps, 2)
  global<-sig.calc(x,invC,D.mat,Subset)
  rate.global<-global$sigma; R<-global$R
  ngps<-nlevels(gps)
  rate.gps<-sapply(1:ngps, function(j){ sig.calc(x[,g==j],
                                                 invC,D.mat,Subset)$sigma  })
  sigma.d.ratio<-max(rate.gps)/min(rate.gps)
  sigma.d.rat <- sapply(1:ncol(gps.combo), function(j){
    rates<-c(rate.gps[levels(g)==gps.combo[1,j]],rate.gps[levels(g)==gps.combo[2,j]])
    max(rates)/min(rates)
  })
  if(length(sigma.d.rat) > 1) rate.mat <- dist(matrix(0, length(rate.gps),)) else
    rate.mat = 0
  for(i in 1:length(rate.mat)) rate.mat[[i]] <- sigma.d.rat[i]
  list(sigma.d.ratio = sigma.d.ratio, rate.global = rate.global,
       rate.gps = rate.gps, sigma.d.gp.ratio = rate.mat,R = R)
}
​
​
sig.calc<-function(x.i,invC.i,D.mat.i,Subset){
  x.i<-as.matrix(x.i)
  N<-dim(x.i)[1];p<-dim(x.i)[2]
  ones<-matrix(1,N,N)
  x.c<- x.i - crossprod(ones,invC.i)%*%x.i/sum(invC.i)
  R<-crossprod(x.c, crossprod(invC.i,x.c))/N
  if(Subset==FALSE) sigma<-sigma<-sum((D.mat.i%*%x.c)^2)/N  else
    sigma<-sum((D.mat.i%*%x.c)^2)/N/p
  return(list(sigma=sigma,R=R))
}
