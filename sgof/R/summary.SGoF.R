summary.SGoF <-
function(object, ...){
cat("Call:\n")
 print(object$call)
cat("\n")
cat("Parameters:","\n")
cat("alpha=",object$alpha,"\n")
cat("gamma=",object$gamma,"\n")
tabla<-table(object$Adjusted.pvalues<=object$gamma)
if(sum(object$Adjusted.pvalues>object$gamma)==length(object$data)){attributes(tabla)$dimnames[[1]]=c(">gamma")}else{if(sum(object$Adjusted.pvalues<=object$gamma)==length(object$data)){attributes(tabla)$dimnames[[1]]=c("<=gamma")}else{attributes(tabla)$dimnames[[1]]=c(">gamma","<=gamma")}}

cat("\n")
res <- list(Rejections=object$Rejections,FDR=round( object$FDR,3),Adjusted.pvalues=tabla )

return(res)
}
