plot.SGoF <-
function(x, ...){
data <- x$data
Adjusted.pvalues<-x$Adjusted.pvalues
plot(sort(data),sort(Adjusted.pvalues),main="SGoF Adjusted p-values",xlab="Unadjusted p-values",type="p",ylab="Adjusted p-values",...)

}
