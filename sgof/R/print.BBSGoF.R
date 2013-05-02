print.BBSGoF <-
function(x, ...){
cat("Call:\n") 
print(x$call) 
cat("\n")
cat("Parameters:","\n")
cat("alpha=",x$alpha,"\n")
cat("gamma=",x$gamma,"\n")
cat("kmin=",x$kmin,"\n")
cat("kmax=",x$kmax,"\n")
cat("\n")
if(length(x$deleted.blocks)!=0){
cat("Warning:","\n")
cat("Blocks", sort(x$deleted.blocks), "have been removed because they provided negative or atypical variances.","\n")
}
cat("\n")

cat("Rejections:\n") 
print(x$Rejections)
}
