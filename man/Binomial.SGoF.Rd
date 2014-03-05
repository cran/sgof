\encoding{UTF-8}
\name{Binomial.SGoF}
\alias{Binomial.SGoF}
\alias{print.Binomial.SGoF}

\title{Binomial SGoF multi-testing procedure}

\description{Performs the Binomial SGoF method (\cite{Carvajal Rodríguez et al., 2009}) for multiple hypothesis testing.}

\usage{
Binomial.SGoF(u, alpha = 0.05, gamma = 0.05)

}




  \arguments{
  \item{u}{A (non-empty) numeric vector of p-values.}
  \item{alpha}{Numeric value. The significance level of the metatest.}
  \item{gamma}{Numeric value. The p-value threshold, so Binomial SGoF looks for significance in the amount of p-values below gamma.}
  
}
  
\details{
Binomial SGoF starts by counting the amount of p-values below \code{gamma}. This amount is compared to the expected one under the intersection or complete null hypothesis (all the nulls are true) in a metatest, performed at level \code{alpha}. Note that, under the intersection null, the p-values will be uniformly distributed on the (0,1) interval, so one expects \code{gamma} times the length of \code{u} p-values falling below \code{gamma}. If the intersection null is accepted, Binomial  SGoF reports no effects. If the intersection null is rejected, the excess of observed significant cases are reported as number of existing effects N (by effects it is meant null hypotheses to be rejected). Finally, the effects are identified by considering the smallest N p-values. The only input you need is the set of p-values.
Binomial SGoF procedure weakly controls the family-wise error rate (FWER) and the false discovery rate (FDR) at level \code{alpha}. That is, the probability of committing one or more than one type I errors along the multiple tests is bounded by \code{alpha} when all the null hypotheses are true. SGoF does not control for FWER nor FDR in the presence of effects. It has been quoted that Binomial SGoF provides a good balance between FDR and power, particularly when the number of tests is large, and the effect level is weak to moderate. It is also known that the number of effects declared by Binomial SGoF is a 100(1-\code{alpha})\% lower bound for the true number of existing effects with p-value below the initial threshold \code{gamma} so, interestingly, at probability 1-\code{alpha}, the number of false discoveries of SGoF does not exceed the number of false non-discoveries (\cite{de Uña Álvarez, 2012}).
Typically, the choice \code{alpha}=\code{gamma} will be used; this common value will be set as one of the usual significance levels (0.001, 0.01, 0.05, 0.1). Note however that \code{alpha} and \code{gamma} have different roles.
The FDR is estimated by the simple method proposed by: \cite{Dalmasso, Broet, Moreau (2005)}, by taking n=1 in their formula.
The adjusted p-value of a given p-value pi is defined as the smallest alpha0 for which the null hypothesis attached to pi is rejected by SGoF with \code{alpha}=\code{gamma}=alpha0. Actually, Binomial.SGoF function provides  these adjusted p-values by restricting alpha0 to the set of original p-values. \cite{Castro-Conde and de Uña-Álvarez  (2013 13/05)}   proved that this restriction does not change the adjusted p-values, while reducing the computational time.
}

\value{
A list containing the following values:
 \item{Rejections }{The number of effects declared by Binomial SGoF.}
 \item{FDR }{The estimated false discovery rate.}
 \item{Adjusted.pvalues}{The adjusted p-values.}
 \item{data }{The original p-values.}
 \item{alpha }{The specified significance level for the metatest.}
 \item{gamma }{The specified p-value threshold.}
 \item{call }{The matched call.}
}
\references{
Carvajal Rodríguez A, de Uña Álvarez J and Rolán Álvarez E (2009). A new
multitest correction (SGoF) that increases its statistical power when increasing the number of tests. \emph{BMC Bioinformatics} 10:209.


Castro Conde I and de Uña Álvarez J (2013). Adjusted p-values for SGoF multitesting method.  \emph{Discussion Papers in Statistics and Operation Research}. Report 13/05. Statistics and OR Department. University of Vigo. \url{https://webs.uvigo.es/depc05/reports/13_05.pdf}

Dalmasso C, Broet P and Moreau T (2005) A simple procedure for estimating the false discovery rate. \emph{Bioinformatics} 21:660--668

de Uña Álvarez J (2011). On the statistical properties of SGoF multitesting method. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 10, Iss. 1, Article 18.

}


\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\seealso{\code{\link{plot.Binomial.SGoF}},\code{\link{summary.Binomial.SGoF}}}

\examples{

p<-runif(387)^2  #387 independent p-values, non-uniform intersection null violated

res<-Binomial.SGoF(p)
summary(res)   #number of rejected nulls, estimated FDR
plot(res)   #adjusted p-values
}

\keyword{htest}

\keyword{Binomial.SGoF}
\keyword{multi-testing}













