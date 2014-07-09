
Extended.SGoF <- function(u,pCDFlist=NA,K=NA, alpha = 0.05, gamma = 0.05, method=NA, Discrete=TRUE, Sides=1,...) {


extended.sgof <- function(u,pCDFlist=NA, K=NA, alpha = 0.05, gamma = 0.05, method=NA, Discrete=TRUE, Sides=1,...) {



n=length(u)


if(is.na(K)==TRUE || (is.na(K)==FALSE&K<n)){stepf <- lapply(pCDFlist, function(x) stepfun(x, c(0, x)))}

if(is.na(K)==TRUE){prob.vec<-unlist(lapply(stepf,function(s){s(gamma)}))}else{if(is.na(K)==FALSE&K<n){prob.vec<-c(unlist(lapply(stepf,function(s){s(gamma)})),rep(gamma,K))}else{prob.vec=rep(gamma,n)}}

crit.value.disc<-numeric(n)

b=seq(0,n)

if(is.na(method)==TRUE&n<2000){crit.value.disc<- b[min(which((1 -ppoibin(b , prob.vec,method = "DFT-CF")) <= alpha))]}
if(is.na(method)==TRUE&n>=2000){crit.value.disc<- b[min(which((1 -ppoibin(b , prob.vec,method = "RNA")) <= alpha))]}
if(is.na(method)==FALSE&method=="DFT-CF"){crit.value.disc<- b[min(which((1 -ppoibin(b , prob.vec,method = "DFT-CF")) <= alpha))]}
if(is.na(method)==FALSE&method=="RNA"){crit.value.disc<- b[min(which((1 -ppoibin(b , prob.vec,method = "RNA")) <= alpha))]}


num.reject.0<-length(u[u<=gamma])  
rejections.disc<-max(num.reject.0-crit.value.disc+1,0)


Extended.SGoF = min(rejections.disc, sum(as.integer(n *ecdf(u)(u)) <= rejections.disc))
        
su <- sort(u)
jj <- which(u == 1)
if (length(jj) != 0) pi0 <- (-1/n) * sum(log(1 - u[-jj])) else pi0 <- (-1/n) * sum(log(1 - u))

if (Extended.SGoF == 0) {
FDR_DS <- 0
}else {
FDR_DS <-round(sort(robust.fdr(su, sides=Sides, p2 = 1 - su, discrete=Discrete , use8=TRUE)$loc.fdr)[Extended.SGoF],4)
}




return(c(list(Rejections = Extended.SGoF, FDR = FDR_DS)))
}

if(missing(u)){stop("data argument is requiered")}

if(is.na(method)==F&(method!="RNA"&method!="DFT-CF")){stop("The specified method in incorrect")}

if(is.na(pCDFlist)==T&K<(length(u))){stop("The specified K is incorrect")}
u<-as.vector(u)
res<-extended.sgof(u,pCDFlist,K, alpha, gamma,method,Discrete,Sides,...) 

 
res$pvalues<-u
res$alpha<-alpha
res$gamma<-gamma
res$K<-K
res$method<-method
res$Discrete<-Discrete
res$Sides<-Sides
res$call<-match.call()
class(res)<-"Extended.SGoF"
return(res)
}


