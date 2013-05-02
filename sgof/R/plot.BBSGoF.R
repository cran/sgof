plot.BBSGoF <-
function(x, ...){

Adjusted.pvalues=x$Adjusted.pvalues
data=x$data

ss=seq(0.001,0.999,by=0.001)

plot(x$n.blocks,x$Tarone.pvalues,ylab="pvalue",xlab="blocks",main="Tarone's test");abline(h=0.05,col=2,lty=2)
par(ask=T) 
plot(x$n.blocks,x$cor,ylab="cor",xlab="blocks",main="Within-block correlation")
par(ask=T) 
plot(ss,dbeta(ss,x$beta.parameters[1],x$beta.parameters[2]),type="l",ylab="density",xlab="probability",main="Beta density"); abline(v=x$p,col=2,lty=2)
par(ask=T) 
plot(x$effects,ylab="effects",xlab="blocks",main="Decision plot",ylim=c(min(x$effects)-5,x$SGoF));abline(h=x$SGoF,col=2,lty=2)
if(x$adjusted.pvalues==TRUE){
par(ask=T) 
plot(sort(data),sort(Adjusted.pvalues),main="BB-SGoF Adjusted p-values",xlab="Unadjusted p-values",ylab="Adjusted p-values",xlim=c(0,0.2))
par(ask=F)
}

}
