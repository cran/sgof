SGoF <-
function(u,alpha=0.05,gamma=0.05){

sgof<-function(u,alpha=0.05,gamma=0.05){

v=as.numeric(u<=gamma)
n=length(v)

SGoF=floor(max(n*(mean(v)-gamma)-n*sqrt(mean(v)*(1-mean(v))/n)*qnorm(1-alpha)+1,0))

su<-sort(u)
jj<-which(u==1)
if(length(jj)!=0) pi0<-(-1/n)*sum(log(1-u[-jj])) else pi0<-(-1/n)*sum(log(1-u))

FDR_S<-(pi0*su[SGoF])/(ecdf(u)(su[SGoF]))


Nu1=n*(ecdf(su)(su)-su)-sqrt(n*ecdf(su)(su)*(1-ecdf(su)(su)))*qnorm(1-su)+1
umax1=which.max(Nu1/n)

max<-(ecdf(su)(su[umax1])-su[umax1])-sqrt((ecdf(su)(su[umax1])*(1-ecdf(su)(su[umax1])))/n)*qnorm(1-su[umax1])+(1/n)

jj<-sum(ecdf(su)(su)<=max) 


a.p<-numeric(n)
a.p[(jj+1):n]<-1
a.p[1:jj]<-sapply(1:jj,function(i) a.p[i]<-min(su[which(n*ecdf(su)(su[i])<=Nu1)]))



return(c(list(Rejections=SGoF,FDR=FDR_S,Adjusted.pvalues=a.p)))
}




if(missing(u)){stop("data argument is requiered")}
u<-as.vector(u)
res<-sgof(u,alpha,gamma)
res$data<-sort(u)
res$alpha<-alpha
res$gamma<-gamma
res$call<-match.call()
class(res)<-"SGoF"
return(res)
}
