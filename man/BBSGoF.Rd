\encoding{UTF-8}

\name{BBSGoF}
\alias{BBSGoF}
\alias{print.BBSGoF}

\title{BBSGoF multi-testing procedure.}

\description{
BB-SGoF (\cite{de Uña-Álvarez, 2012}; \cite{Castro-Conde and de Uña-Álvarez, 2013 13/03}) is an adaptation of SGoF method for possibly dependent tests. It is initially assumed that the provided vector of p-values are correlated
 in k blocks of equal size (following the given sequence), where k is unknown.
}

\usage{
BBSGoF(u, alpha = 0.05, gamma = 0.05, kmin = 2, kmax = min(length(u)\%/\%10, 100),
 tol = 10, adjusted.pvalues = FALSE, blocks = NA)
}


\arguments{
  \item{u}{A (non-empty) numeric vector of p-values.}
  \item{alpha}{Numeric value. The significance level of the metatest.}
  \item{gamma}{Numeric value. The p-value threshold, so SGoF looks for significance in the amount of p-values below gamma.}
  \item{kmin}{Numeric value. The smallest allowed number of blocks of correlated tests.}
  \item{kmax}{Numeric value. The largest allowed number of blocks of correlated tests.}
  \item{tol}{Numeric value. The tolerance in model fitting (see Details).}
  \item{adjusted.pvalues}{Logical. Default is FALSE. A variable indicating whether to compute the adjusted p-values.}
  \item{blocks}{Numeric value. The number of existing blocks (see Details).}

}

\details{
BB-SGoF (\cite{de Uña-Álvarez, 2012}; \cite{Castro-Conde and de Uña-Álvarez, 2013 13/03}) is an adaptation of SGoF method for possibly dependent tests. It is initially assumed that the provided vector of p-values are correlated in k blocks of equal size (following the given sequence), where k is unknown. Inference on the number of existing effects is performed following SGoF principles, but replacing the binomial distribution for a beta-binomial in the metatest. The beta-binomial distribution is approximated by the normal distribution; therefore, some caution is needed when the number of tests is small. It is implicitly assumed that the probability p for a p-value to fall below \code{gamma} is random, following a beta distribution, Beta(a,b); as a consequence, the number of p-values below gamma in each block generates a random sample from a Betabinomial(p,rho) model, where p=E(p)=a/(a+b) and rho=Var(p)/p(1-p)=1/(a+b+1) are respectively the mean of p and the within-block correlation between two indicators of type I(pi<\code{gamma}), I(pj<\code{gamma}). The parameters are estimated by maximum likelihood, and the asymptotic normal distribution of the estimated parameters is used to perform the inferences (so caution is needed when the number of p-values is small). Since k is unknown, the method is fitted for each integer ranging from k=\code{kmin} to k=\code{kmax}, and results for each k are saved. Automatic (conservative) choice of k is also performed; the automatic k is the value of k leading to the smallest amount of declared effects (by effects it is meant null hypotheses to be rejected).
The excess of observed significant cases in the beta-binomial metatest are reported as number of existing effects N. Finally, the effects are identified by considering the smallest N p-values.
BB-SGoF procedure weakly controls the family-wise error rate (FWER) and the false discovery rate (FDR) at level \code{alpha}. That is, the probability of commiting one or more than one type I errors along the multiple tests is bounded by \code{alpha} when all the null hypotheses are true. SGoF does not control for FWER nor FDR in the presence of effects. It has been quoted that BB-SGoF provides a good balance between FDR and power, particularly when the number of tests is large, and the effect level is weak to moderate. It is also known that the number of effects declared by BB-SGoF is a 100(1-\code{alpha})\% lower bound for the true number of existing effects with p-value below the initial threshold \code{gamma} so, interestingly, at probability 1-\code{alpha}, the number of false discoveries of BB-SGoF does not exceed the number of false non-discoveries (\cite{de Uña-Álvarez, 2012}).
As for SGoF method, typically the choice \code{alpha}=\code{gamma} will be used for BB-SGoF; this common value will be set as one of the usual significance levels (0.001, 0.01, 0.05, 0.1). Note however that \code{alpha} and \code{gamma} have different roles.
When \code{adjusted.pvalues}=TRUE adjusted p-values are calculated. This are defined in the same spirit of SGoF method, but a guessed value for k must be supplied in the argument \code{blocks}. Once k is supplied, the adjusted p-value of a given p-value pi is defined as the smallest alpha0 for which the null hypothesis attached to pi is rejected by BB-SGoF (based on the given k) with \code{alpha}=\code{gamma}=alpha0. Actually, BBSGoF function provides an approximation of these adjusted p-values by restricting alpha0 to the set of original p-values.
The argument \code{tol} allows for a stronger (small \code{tol}) or weaker (large \code{tol}) criterion when removing poor fits of the beta-binomial model. When the variance of the estimated beta-binomial parameters for a given k is larger than \code{tol} times the median variance along k=\code{kmin},...,\code{kmax}, the particular value of k is discarded.  
The false discovery rate is estimated by the simple method proposed by: \cite{Dalmasso, Broet, Moreau (2005)}, by taking n=1 in their formula.
}

\value{
A list containing the following values:
 \item{Rejections }{The number of effects declared by BB-SGoF with automatic k.}
 \item{FDR }{ The estimated false discovery rate.}
 \item{Adjusted.pvalues }{ The adjusted p-values.}
 \item{effects }{ A vector with the number of effects declared by BB-SGoF for each value of k.}
 \item{SGoF }{The number of effects declared by Conservative SGoF.}
 \item{automatic.blocks }{ The automatic number of blocks.}
 \item{deleted.blocks }{ A vector with the values of k for which the model gave a poor fit.}
 \item{n.blocks }{A vector with the values of k for which the model fitted well.}
 \item{p }{The average ratio of p-values below gamma.}
 \item{cor }{A vector with the estimated within-block correlation.}
 \item{Tarone.pvalues }{A vector with the p-values of Tarone’s test for no correlation.}
 \item{Tarone.pvalue.auto }{The p-values of Tarone’s test for the automatic k.}
 \item{beta.parameters }{The estimated parameters of the Beta(a,b) model for the automatic k.}
 \item{betabinomial.parameters }{The estimated parameters of the Betabinomial(p,rho) model for the automatic k.}
 \item{sd.betabinomial.parameters }{The standard deviation of the estimated parameters of the Betabinomial(p,rho) model for the automatic k.}
 \item{data }{The original p-values.}
 \item{adjusted.pvalues }{A logical value indicating whether the adjusted p-values have been ordered.}
 \item{blocks }{Guessed value of k.}
 \item{n }{The length of x.}
 \item{alpha }{The specified significance level for the metatest.}
 \item{gamma }{The specified p-value threshold.}
 \item{kmin }{The smallest allowed number of blocks of correlated tests.}
 \item{kmax }{The largest allowed number of blocks of correlated tests.}
 \item{tol }{Tolerance in model fitting (see Details).}
 \item{call }{The matched call.}
}

\references{

Castro Conde I and de Uña Álvarez J (2013). Power, FDR and conservativeness of BB-SGoF method for multiple dependent tests: a simulation study.  \emph{Discussion Papers in Statistics and Operation Research}. Report 13/03. Statistics and OR Department. University of Vigo. 
\url{http://webs.uvigo.es/depc05/reports/13_03.pdf}

Dalmasso C, Broet P and Moreau T (2005). A simple procedure for estimating the false discovery rate. \emph{Bioinformatics} 21:660--668

de Uña Álvarez J (2012). The Beta-Binomial SGoF method for multiple dependent tests. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 11, Iss. 3, Article 14.

}
\author{Irene Castro Conde and Jacobo de Uña Álvarez}

\seealso{\code{\link{summary.BBSGoF}},\code{\link{plot.BBSGoF}},\code{\link{BY}} }

\examples{

p<-runif(387)^2  #387 independent p-values, non-uniform intersection null violated

res<-BBSGoF(p)
summary(res)    #automatic number of blocks, number of rejected nulls, 
		#estimated FDR, beta and beta-binomial parameters,
		#Tarone test of no correlation 

par(mfrow=c(2,2))
plot(res)   #Tarone test, within-block correlation, beta density (for automatic k),
	    #and decision plot (number of rejected nulls)


}

\keyword{htest}
\keyword{correlation}
\keyword{BBSGoF}
\keyword{multi-testing}

