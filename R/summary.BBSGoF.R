summary.BBSGoF <-
function(object, ...){
cat("Call:\n")
print(object$call)
cat("\n")
cat("Parameters:","\n")
cat("alpha=",object$alpha,"\n")
cat("gamma=",object$gamma,"\n")
cat("kmin=",object$kmin,"\n")
cat("kmaobject=",object$kmaobject,"\n")
cat("\n")

if(object$adjusted.pvalues==T){
tabla<-table(object$Adjusted.pvalues<=object$gamma)
if(sum(object$Adjusted.pvalues>object$gamma)==length(object$data)){attributes(tabla)$dimnames[[1]]=c(">gamma")}else{if(sum(object$Adjusted.pvalues<=object$gamma)==length(object$data)){attributes(tabla)$dimnames[[1]]=c("<=gamma")}else{attributes(tabla)$dimnames[[1]]=c(">gamma","<=gamma")}}

res <- list(Rejections=object$Rejections,FDR=round( object$FDR,3),Adjusted.pvalues=tabla,Tarone.pvalue.auto=object$Tarone.pvalues[object$automatic.blocks-length(object$deleted.blocks[object$deleted.blocks<object$automatic.blocks])-1], beta.parameters=object$beta.parameters,betabinomial.parameters=object$betabinomial.parameters,sd.betabinomial.parameters=object$sd.betabinomial.parameters,automatic.blocks=object$automatic.blocks)
}else{
res <- list(Rejections=object$Rejections,FDR=round( object$FDR,3),Tarone.pvalue.auto=object$Tarone.pvalues[object$automatic.blocks-length(object$deleted.blocks[object$deleted.blocks<object$automatic.blocks])-1], beta.parameters=object$beta.parameters,betabinomial.parameters=object$betabinomial.parameters,sd.betabinomial.parameters=object$sd.betabinomial.parameters,automatic.blocks=object$automatic.blocks)
}
class(res) <- "summary.BBSGoF"
return(res)
}
