
Discrete.SGoF <- function(u,pCDFlist=NA,K=NA, alpha = 0.05, gamma = 0.05, method=NA, Discrete=TRUE, Sides=1,...) {



discrete.sgof <- function(u,pCDFlist=NA, K=NA, alpha = 0.05, gamma = 0.05, method=NA, Discrete=TRUE, Sides=1,...) {




robust.fdr<-function(p,sides=1,p2=1-p,discrete=F,use8=T)

{
	m<-length(p)
	ord<-order(p)
	pcumtab<-cumsum(table(p))/m
	F05<-mean(p<=0.5)	
	edf<-approx(as.numeric(names(pcumtab)),pcumtab,xout=p,rule=2)$y
	if (sides==2)
	{
		pi<-min(1,2*mean(p))
		loc.fdr<-pi*p/edf
	}
	else
	{
		p.new<-2*(p*(p<=p2)+p2*(p>p2))
		pi<-min(1,2*mean(p.new))
		if (discrete) 
		{
			if (use8) pi<-min(1,8*mean(p.new))
			else
			{
				lam<-max(p[p<=0.5])
				k<-1/(lam^2+(0.5-lam)^2)
				pi<-min(k*mean(p.new),1)
			
			}
		}
		loc.fdr<-pi*p/edf
		loc.fdr[p>0.5]<-(0.5*pi+edf[p>0.5]-F05)/edf[p>0.5]
	}
	
	
	return(list(p=p,loc.fdr=loc.fdr,pi=pi,ord=ord))
}




n=length(u)

if(is.na(K)==TRUE || (is.na(K)==FALSE&K<n)){stepf <- lapply(pCDFlist, function(x) stepfun(x, c(0, x)))}

if(is.na(K)==TRUE){prob.vec<-unlist(lapply(stepf,function(s){s(gamma)}))}else{if(is.na(K)==FALSE&K<n){prob.vec<-c(unlist(lapply(stepf,function(s){s(gamma)})),rep(gamma,K))}else{prob.vec=rep(gamma,n)}}

crit.value.disc<-numeric(n)

b=seq(0,n)

ppoibin<-function (kk, pp, method = "DFT-CF", wts = NULL){
  if (any(pp < 0) | any(pp > 1)) {
    stop("invalid values in pp.")
  }
  if (is.null(wts)) {
    wts = rep(1, length(pp))
  }
  switch(method, `DFT-CF` = {
    mm = length(kk)
    res = double(mm)
    npp = length(pp)
    n = sum(wts)
    avec = double(n + 1)
    bvec = double(n + 1)
    funcate = 1
    ex = 0
    tmp = list( as.double(res), as.integer(kk), 
             as.integer(mm), as.integer(n), as.double(pp), as.double(avec), 
             as.double(bvec), as.integer(funcate), as.double(ex), 
             as.integer(npp), as.integer(wts))
    res = tmp[[1]]
    res[res < 0] = 0
    res[res > 1] = 1
    res[kk < 0] = 0
    res[kk >= sum(wts)] = 1
  }, RF = {
    kk1 = kk
    kk[kk < 0] = 0
    pp = rep(pp, wts)
    mm = length(kk)
    res = double(mm)
    n = length(pp)
    mat = rep(0, (n + 1) * (n + 2))
    tmp = list( as.double(res), as.integer(kk), 
             as.integer(mm), as.integer(n), as.double(pp), as.double(mat))
    res = tmp[[1]]
    res[kk1 < 0] = 0
    res[kk1 >= sum(wts)] = 1
  }, RNA = {
    pp = rep(pp, wts)
    muk = sum(pp)
    sigmak = sqrt(sum(pp * (1 - pp)))
    gammak = sum(pp * (1 - pp) * (1 - 2 * pp))
    ind = gammak/(6 * sigmak^3)
    kk1 = (kk + 0.5 - muk)/sigmak
    vkk.r = pnorm(kk1) + gammak/(6 * sigmak^3) * (1 - kk1^2) * 
      dnorm(kk1)
    vkk.r[vkk.r < 0] = 0
    vkk.r[vkk.r > 1] = 1
    res = vkk.r
  }, `NA` = {
    pp = rep(pp, wts)
    muk = sum(pp)
    sigmak = sqrt(sum(pp * (1 - pp)))
    gammak = sum(pp * (1 - pp) * (1 - 2 * pp))
    kk1 = (kk + 0.5 - muk)/sigmak
    res = pnorm(kk1)
  }, PA = {
    pp = rep(pp, wts)
    muk = sum(pp)
    res = ppois(q = kk, lambda = muk)
  })
  return(res)
}

if(is.na(method)==TRUE&n<2000){crit.value.disc<- b[min(which((1 -ppoibin(b , prob.vec,method = "DFT-CF")) <= alpha))]}
if(is.na(method)==TRUE&n>=2000){crit.value.disc<- b[min(which((1 -ppoibin(b , prob.vec,method = "RNA")) <= alpha))]}
if(is.na(method)==FALSE&method=="DFT-CF"){crit.value.disc<- b[min(which((1 -ppoibin(b , prob.vec,method = "DFT-CF")) <= alpha))]}
if(is.na(method)==FALSE&method=="RNA"){crit.value.disc<- b[min(which((1 -ppoibin(b , prob.vec,method = "RNA")) <= alpha))]}


num.reject.0<-length(u[u<=gamma])  
rejections.disc<-max(num.reject.0-crit.value.disc,0)

Discrete.SGoF = min(rejections.disc, sum(as.integer(n *ecdf(u)(u)) <= rejections.disc))
        
su <- sort(u)
jj <- which(u == 1)
if (length(jj) != 0) pi0 <- 1 else pi0 <- min((-1/n) * sum(log(1 - u)),1)

if (Discrete.SGoF == 0) {
FDR_DS <- 0
}else {
FDR_DS <-round(sort(robust.fdr(su, sides=Sides, p2 = 1 - su, discrete=Discrete , use8=TRUE)$loc.fdr)[Discrete.SGoF],4)
}




return(c(list(Rejections = Discrete.SGoF, FDR = min(FDR_DS,1))))
}

n=length(u)

if(missing(u)){stop("data argument is required")}

if(missing(pCDFlist)& (is.na(K)==TRUE || (is.na(K)==FALSE & K<n))){stop("pCDFlist argument is required")}



if(is.na(method)==F&(method!="RNA"&method!="DFT-CF")){stop("The specified method in incorrect")}


if(length(is.na(pCDFlist))==1&K<length(u)){stop("The specified K is incorrect")}

u<-as.vector(u)
res<-discrete.sgof(u,pCDFlist,K, alpha, gamma,method,Discrete,Sides,...) 

 
res$pvalues<-u
res$alpha<-alpha
res$gamma<-gamma
res$K<-K
res$method<-method
res$Discrete<-Discrete
res$Sides<-Sides
res$call<-match.call()
class(res)<-"Discrete.SGoF"
return(res)
}


