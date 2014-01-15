BY <-
function(u,alpha=0.05){

by<-function(u,alpha=0.05){

n=length(u)
r=rank(u,ties.method="max")
q <- sum(1/(1:n))

by=max(c(r[u<=(r/(n*q))*alpha],0),na.rm = T) 

su<-sort(u)
jj<-which(u==1)
if(length(jj)!=0) pi0<-(-1/n)*sum(log(1-u[-jj])) else pi0<-(-1/n)*sum(log(1-u))

if(by==0){FDR_BY<-0}else{FDR_BY<-round((pi0*su[by])/(ecdf(u)(su[by])),4)}



ad.p=numeric(n)
ad.p[n]<-sort(u)[n]
for(i in (n-1):1){
ad.p[i]<-min(sort(u)[i]*q*n/i,ad.p[i+1])
}


return(c(list(Rejections=by,FDR=FDR_BY,Adjusted.pvalues=sort(ad.p))))
}

if(missing(u)){stop("data argument is requiered")}




u<-as.vector(u)
res<-by(u,alpha)
res$data<-sort(u)
res$alpha<-alpha
res$call<-match.call()
class(res)<-"BY"
return(res)
}
