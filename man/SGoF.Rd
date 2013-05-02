\encoding{UTF-8}
\name{SGoF}
\alias{SGoF}
\title{SGoF multitesting procedure}

\description{Performs SGoF method (\cite{Carvajal-Rodríguez et al.}, 2009) for multiple hypotheses testing.}

\usage{
SGoF(u, alpha = 0.05, gamma = 0.05)
}




  \arguments{
  \item{u}{A (non-empty) numeric vector of p-values.}
  \item{alpha}{Numeric value. The significance level of the metatest.}
  \item{gamma}{Numeric value. The p-value threshold, so SGoF looks for significance in the amount of p-values below gamma.}
  
}
  
\details{
SGoF starts by counting the amount of p-values below gamma. This amount is compared to the expected one under the intersection or complete null hypothesis (all the nulls are true) in a metatest, performed at level alpha. Note that, under the intersection null, the p-values will be uniformly distributed on the (0,1) interval, so one expects gamma times the length of x p-values falling below gamma. If the intersection null is accepted, SGoF reports no effects. If the intersection null is rejected, the excess of observed significant cases are reported as number of existing effects N. (By effects it is meant null hypotheses to be rejected). Finally, the effects are identified by considering the smallest N p-values. The only input you need is the set of p-values.
SGoF procedure weakly controls the family-wise error rate (FWER) and the false discovery rate (FDR) at level alpha. That is, the probability of commiting one or more than one type I errors along the multiple tests is bounded by alpha when all the null hypotheses are true. SGoF does not control for FWER nor FDR in the presence of effects. It has been quoted that SGoF provides a good balance between FDR and power, particularly when the number of tests is large, and the effect level is weak to moderate. It is also known that the number of effects declared by SGoF is a 100(1-alpha)% lower bound for the true number of existing effects with p-value below the initial threshold gamma so, interestingly, at probability 1-alpha, the number of false discoveries of SGoF does not exceed the number of false non-discoveries (de Uña-Álvarez, 2012).
Typically, the choice alpha=gamma will be used; this common value will be set as one of the usual significance levels (0.001, 0.01, 0.05, 0.1). Note however that alpha and gamma have different roles.
The normal asymptotic approximation of the binomial model in the metatest is used to perform the inferences (so caution is needed when the number of p-values is small). The implementation of the method is based on \cite{de Uña-Álvarez} (2011); see however {Carvajal-Rodríguez et al.} (2009) for a sequential approach to SGoF.
The false discovery rate is estimated by the simple method proposed by: \cite{Dalmasso C, Broet P, Moreau T} (2005) A simple procedure for estimating the false discovery rate. Bioinformatics 21:660-668, by taking n=1 in their formula.
The adjusted p-value of a given p-value pi is defined as the smallest alpha0 for which the null hypothesis attached to pi is rejected by SGoF with alpha=gamma=alpha0. Actually, SGoF function provides an approximation of these adjusted p-values by restricting alpha0 to the set of original p-values.
}
\value{
A list containing the following values:
 \item{Rejections }{The number of effects declared by BB-SGoF with automatic k.}
 \item{FDR }{The estimated false discovery rate.}
 \item{Adjusted.pvalues }{The adjusted p-values.}
 \item{data }{The original p-values.}
 \item{alpha }{The specified significance level for the metatest.}
 \item{gamma }{The specified p-value threshold.}
 \item{call }{The matched call.}
}
\references{
Carvajal-Rodríguez A, de Uña-Álvarez J, Rolán-Alvarez E (2009) A new
multitest correction (SGoF) that increases its statistical power when increasing the number of tests. BMC Bioinformatics 10:209.

Dalmasso C, Broet P, Moreau T (2005) A simple procedure for estimating the false discovery rate. 
Bioinformatics 21:660-668


de Uña-Álvarez J (2011) On the statistical properties of SGoF multitesting method. Statistical Applications in Genetics and Molecular Biology, Vol. 10, Iss. 1, Article 18.

de Uña-Álvarez J (2012). The Beta-Binomial SGoF method for multiple dependent tests. Statistical Applications in Genetics and Molecular Biology, Vol. 11, Iss. 3, Article 14.
}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\seealso{\code{\link{BBSGoF}}}

\examples{
data(Hedenfalk)

res<-SGoF(Hedenfalk$x)
summary(res)   #number of rejected nulls, estimated FDR
plot(res)   #adjusted p-values
}

\keyword{htest}

\keyword{BB-SGoF}

\keyword{multitesting}
\keyword{SGOF}