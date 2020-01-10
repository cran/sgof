\encoding{UTF-8}
\name{Discrete.SGoF}
\alias{Discrete.SGoF}
\alias{print.Discrete.SGoF}

\title{Discrete SGoF multiple testing procedure}

\description{Performs the Discrete SGoF method (\cite{Castro-Conde, Döhler et al., 2015}) for multiple hypothesis testing.}

\usage{
Discrete.SGoF(u,pCDFlist=NA, K=NA, alpha = 0.05, gamma = 0.05, method=NA, 
              Discrete=TRUE, Sides=1,\dots)

}




  \arguments{
  \item{u}{A (non-empty) numeric vector of p-values.}
  \item{pCDFlist}{A (non-empty) list with the empirical cumulative function of each discrete p-value.}
  \item{K}{Numeric value. The number of continuous tests.}
  \item{alpha}{Numeric value. The significance level of the metatest.}
  \item{gamma}{Numeric value. The p-value threshold, so Discrete SGoF looks for significance in the amount of p-values below gamma.}
  \item{method}{Method used in the computation of the Poisson binomial quantile.  "DFT-CF" for the exact method and "RNA" for the refined normal approximation.}
  \item{Discrete}{Logical. Default is TRUE. A variable indicating if the tests are discrete or continuous in order to estimate the FDR.}
  \item{Sides}{Numeric value indicating if the tests are one-sided (default), \code{Sides=1}, or two-sided, \code{Sides=2} in order to estimate the FDR.}
  \item{\dots}{Other parameters to be passed through to \code{robust.fdr} function.}
 }
 
\details{
Discrete SGoF is an extension of Binomial SGoF, based on the generalized or Poisson binomial distribution (Hong, 2013),  which takes into account the discreteness of the p-values. If all the tests are continuous Discrete SGoF reduces to Binomial SGoF method. 
In particular, if the p-values are continuous, the number of rejections given by Discrete.SGoF will be the number of effects declared by Binomial SGoF.  For computing the Poisson Binomial quantile, the \code{poibin} package (Hong, 2019) is used. The exact method ("DFT-CF") and the "RNA" approximation are used by default to compute the quantile depending on whether the number of tests is smaller than 2000  or not, respectively (see Hong 2013a for more information). However, the user can specified which of the two methods to use.
Discrete SGoF works the same like Binomial SGoF but it uses the quantiles of the generalized binomial distribution, as mentioned, instead of the binomial quantiles.
Discrete SGoF maintains the theoretical properties of Binomial SGoF, e.g. weak control of FDR(FWER) and increasing power when the number of tests increases (de Uña Álvarez, 2011).
The FDR is estimated by using the method proposed by: \cite{Pounds and Cheng (2006)} using the \code{robust.fdr} function provided by the authors.
}

\value{
A list containing the following values:
 \item{Rejections }{The number of effects declared by Discrete SGoF.}
 \item{FDR }{The estimated false discovery rate.}
  \item{pvalues }{The original p-values.}
 \item{alpha }{The specified significance level for the metatest.}
 \item{gamma }{The specified p-value threshold.}
 \item{K }{The specified number of continuous tests.}
 \item{Method}{The specified method used in the computation of the Poisson binomial quantile. }
  \item{Discrete}{The specified type of tests.}
  \item{Sides}{Numeric value indicating if the tests are one-sided (default), \code{Sides=1}, or two-sided, \code{Sides=2}.}
 \item{call }{The matched call.}
}
\references{




Carvajal Rodríguez A, de Uña Álvarez J and Rolán Álvarez E (2009). A new
multitest correction (SGoF) that increases its statistical power when increasing the number of tests. \emph{BMC Bioinformatics} 10:209.


Castro Conde I, Döhler S and de Uña Álvarez J (2015). An extended sequential goodness-of-fit multiple testing method for discrete data. \emph{Statistical Methods in Medical Research}, doi: 10.1177/0962280215597580.


de Uña Álvarez J (2011). On the statistical properties of SGoF multitesting method. 
\emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 10, Iss. 1, Article 18.


Heller R,   Gur  H and  Yaacoby S. (2012). discreteMTP: Multiple testing procedures for discrete test statistics.
 R package version 0.1-2

	
Hong Y. (2013). On computing the distribution functions for the Poisson binomial
distribution. \emph{Computational Statistics and Data Analysis} 59, 41-51.

Hong Y. (2019). poibin: The Poisson Binomial Distribution. R package version 1.4


Pounds, S. and C. Cheng (2006). Robust estimation of the false discovery rate.
\emph{Bioinformatics} 22 (16), 1979-1987.
}


\author{Irene Castro Conde and Sebastian Döhler}



\seealso{\code{\link{Binomial.SGoF}}}

\examples{


require(discreteMTP)

data(amnesia) #discrete data

A11 <- amnesia$AmnesiaCases 
A21 <- sum(amnesia$AllAdverseCases) - A11
A12 <- amnesia$AllAdverseCases - A11
A22 <- sum(amnesia$AllAdverseCases) - sum(amnesia$AmnesiaCases) - A12

A1. <- sum(amnesia$AmnesiaCases)
A2. <- sum(amnesia$AllAdverseCases) - A1.   
n <- A11 + A12
k <- pmin(n,A1.)

pCDFlist <- list()
pvec <- numeric(nrow(amnesia))

## Calculation of the p-values and the p-values CDFs: 

for (i in 1:nrow(amnesia))
{
  x <- 0:k[i]
  pCDFlist[[i]] <- dhyper(x ,A1., A2. ,n[i]) + phyper(x ,A1. ,A2. ,n[i] ,lower.tail = FALSE)
  pCDFlist[[i]] <- rev(pCDFlist[[i]])
  pvec[i] <- dhyper(A11[i] ,A1. ,A2. ,n[i]) + phyper(A11[i] ,A1. ,A2. ,n[i] ,lower.tail = FALSE)
}

res<-Discrete.SGoF(u=pvec,pCDFlist=pCDFlist,alpha=0.05,gamma=0.05,Discrete=TRUE,Sides=1)
res


#continuous p-values

res2<-Discrete.SGoF(u=Hedenfalk$x,K=3170,Discrete=FALSE, method="DFT-CF",Sides=2)
res2

}

\keyword{htest}

\keyword{Binomial.SGoF}
\keyword{multi-testing}













