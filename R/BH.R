BH <-
function(u,alpha=0.05){

bh<-function(u,alpha=0.05){

n=length(u)
r=rank(u)

bh=round(max(c(r[u<=(r/n)*alpha],0),na.rm = T) )#effects declared by BH

su<-sort(u)
jj<-which(u==1)
if(length(jj)!=0) pi0<-(-1/n)*sum(log(1-u[-jj])) else pi0<-(-1/n)*sum(log(1-u))

FDR_BH<-(pi0*su[bh])/(ecdf(u)(su[bh]))



ad.p=numeric(n)
ad.p[n]<-sort(u)[n]
for(i in (n-1):1){
ad.p[i]<-min(sort(u)[i]*(n/i),ad.p[i+1])
}


return(c(list(Rejections=bh,FDR=FDR_BH,Adjusted.pvalues=sort(ad.p))))
}

if(missing(u)){stop("data argument is requiered")}




u<-as.vector(u)
res<-bh(u,alpha)
res$data<-sort(u)
res$alpha<-alpha
res$call<-match.call()
class(res)<-"BH"
return(res)
}
