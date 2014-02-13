\encoding{UTF-8}
\name{SGoF}
\alias{SGoF}
\alias{print.SGoF}

\title{Conservative SGoF multitesting procedure}

\description{Performs Conservative SGoF method (\cite{de Uña Álvarez, 2011}) for multiple hypothesis testing.}
\usage{
SGoF(u, alpha = 0.05, gamma = 0.05)

}




  \arguments{
  \item{u}{A (non-empty) numeric vector of p-values.}
  \item{alpha}{Numeric value. The significance level of the metatest.}
  \item{gamma}{Numeric value. The p-value threshold, so Conservative SGoF looks for significance in the amount of p-values below gamma.}
  
}
  
\details{
Conservative SGoF is an asymptotic version (large number of tests) of the  Binomial SGoF  procedure, where the binomial quantiles are approximated by the normal ones. Besides, the variance of the number of p-values below \code{gamma} is estimated without assuming that all the null hypotheses are true, which typically results in a more conservative decision (from this the method’s name). When the number of tests is large, Conservative SGoF and Binomial SGoF report approximately the same result. This method should no be used when the number of tests is small, because the binomial-normal approximation will perform poorly. Conservative SGoF method has the main properties of  Binomial SGoF like weak control of  the family-wise error rate (FWER) and the false discovery rate (FDR) at level \code{alpha} and  a good balance between FDR and power, particularly when the number of tests is large, and the effect level is weak to moderate. See \code{Binomial.SGoF} for more details.
Typically, the choice \code{alpha}=\code{gamma} will be used; this common value will be set as one of the usual significance levels (0.001, 0.01, 0.05, 0.1). Note however that \code{alpha} and \code{gamma} have different roles.
The FDR is estimated by the simple method proposed by: \cite{Dalmasso, Broet, Moreau  (2005)}, by taking n=1 in their formula.
The adjusted p-value of a given p-value pi is defined as the smallest alpha0 for which the null hypothesis attached to pi is rejected by Conservative SGoF with \code{alpha}=\code{gamma}=alpha0. Actually, Conservative.SGoF function provides  these adjusted p-values by restricting alpha0 to the set of original p-values (\cite{Castro Conde  and de Uña Álvarez , 2013 13/05}).
}

\value{
A list containing the following values:
 \item{Rejections }{The number of effects declared by SGoF.}
 \item{FDR }{The estimated false discovery rate.}
 \item{Adjusted.pvalues }{The adjusted p-values.}
 \item{data }{The original p-values.}
 \item{alpha }{The specified significance level for the metatest.}
 \item{gamma }{The specified p-value threshold.}
 \item{call }{The matched call.}
}
\references{
Castro Conde I and de Uña Álvarez J (2013). Adjusted p-values for SGoF multitesting method. \emph{Discussion Papers in Statistics and Operation Research}. Report 13/05. Statistics and OR Department. University of Vigo. \url{https://webs.uvigo.es/depc05/reports/13_05.pdf}


Dalmasso C, Broet P and Moreau T (2005). A simple procedure for estimating the false discovery rate. 
\emph{Bioinformatics} 21:660--668


de Uña-Álvarez J (2011). On the statistical properties of SGoF multitesting method. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 10, Iss. 1, Article 18.

}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\seealso{\code{\link{summary.SGoF}},\code{\link{plot.SGoF}}}

\examples{

p<-runif(387)^2  #387 independent p-values, non-uniform intersection null violated

res<-SGoF(p)
summary(res)   #number of rejected nulls, estimated FDR
plot(res)   #adjusted p-values
}

\keyword{htest}


\keyword{multitesting}
\keyword{SGoF}
