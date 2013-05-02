plot.BH <-
function(x, ...){
data <- x$data
Adjusted.pvalues<-x$Adjusted.pvalues
plot(sort(data),sort(Adjusted.pvalues),main="BH Adjusted p-values",xlab="Unadjusted p-values",type="p",ylab="Adjusted p-values",...)

}
