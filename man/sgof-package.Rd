\encoding{UTF-8}
\name{sgof-package}
\alias{sgof-package}
\alias{sgof}
\docType{package}
\title{Multiple hypothesis testing}
\description{
This package implements seven different methods for multiple testing problems. 
The \cite{Benjamini and Hochberg (1995)} false discovery rate controlling procedure and its modification for  dependent tests \cite{Benjamini and Yekutieli (2001)}, 
the method called Binomial SGoF  proposed in \cite{Carvajal Rodríguez et al. (2009)} and its conservative and bayesian versions called Conservative SGoF (\cite{de Uña Álvarez, 2011}) and Bayesian SGoF (\cite{Castro Conde  and de Uña Álvarez, 2013 13/06}), respectively,
and the BB-SGoF  (Beta-Binomial SGoF, \cite{de Uña Álvarez, 2012}) and Discrete SGoF (\cite{Castro Conde et al., 2015}) procedures which are  adaptations of SGoF method for possibly correlated tests and for discrete tests, respectively. Number of rejections, FDR and adjusted p-values are computed among other things.
}

\details{
\tabular{ll}{
Package: \tab sgof\cr
Type: \tab Package\cr
Version: \tab 2.3.2\cr
Date: \tab 2020-01-10\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}

This package incorporates the functions \code{BH},\code{BY}, \code{SGoF}, \code{Binomial.SGoF}, \code{Bayesian.SGoF}, \code{Discrete.SGoF}  and \code{BBSGoF}, which call the methods aforementioned. For a complete list of functions, use library(help="sgof").
}

\author{
Irene Castro Conde and Jacobo de Uña Álvarez

Maintainer:Irene Castro Conde \email{irene.castro@uvigo.es}

}
\references{
Benjamini Y and Hochberg Y (1995). Controlling the False Discovery Rate: A
Practical and Powerful Approach to Multiple Testing. \emph{Journal of the Royal Statistical Society Series B (Methodological)} 57, 289--300.

Benjamini Y and Yekutieli D (2001). The control of the false discovery rate in multiple testing under dependency. \emph{Annals of Statistics} 29, 1165–-1188.

Carvajal Rodríguez A, de Uña Álvarez J and Rolán Álvarez E (2009). A new
multitest correction (SGoF) that increases its statistical power when increasing the number of tests. \emph{BMC Bioinformatics} 10:209.



Castro Conde I and de Uña Álvarez J. Power, FDR and conservativeness of BB-SGoF
method. \emph{Computational Statistics}; Volume 30, Issue 4, pp 1143-1161 DOI: 10.1007/s00180-015-0553-2.


Castro Conde I and de Uña Álvarez J (2015). Adjusted p-values for SGoF multiple test procedure.  \emph{Biometrical Journal}; 57(1): 108-122. DOI: 10.1002/bimj.201300238


Castro Conde I, Döhler S and de Uña Álvarez J (2015). An extended sequential goodness-of-fit multiple testing method for discrete data. \emph{Statistical Methods in Medical Research}. doi: 10.1177/0962280215597580.

Castro Conde I and de Uña Álvarez J (2014). sgof: An R package for multiple testing problems. \emph{The
R Journal}; Vol. 6/2 December: 96-113.

Castro Conde I and de Uña Álvarez J (2013). SGoF multitesting method under the Bayesian paradigm. \emph{Discussion Papers in Statistics and Operation Research}. Report 13/06. Statistics and OR Department. University of Vigo. 


Dalmasso C, Broet P and Moreau T (2005). A simple procedure for estimating the false discovery rate. \emph{Bioinformatics} 21:660--668


de Uña Álvarez J (2011). On the statistical properties of SGoF multitesting method. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 10, Iss. 1, Article 18.

de Uña Álvarez J (2012). The Beta-Binomial SGoF method for multiple dependent tests. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 11, Iss. 3, Article 14.


Hong Y. (2013). On computing the distribution functions for the Poisson binomial
distribution. \emph{Computational Statistics and Data Analysis} 59, 41-51.

Hong Y. (2019). poibin: The Poisson Binomial Distribution. R package version 1.4


Pounds, S. and C. Cheng (2006). Robust estimation of the false discovery rate.
\emph{Bioinformatics} 22 (16), 1979-1987.

}
\keyword{package}
\keyword{htest}
\keyword{correlation}
\keyword{BBSGoF}
\keyword{Benjamini-Hochberg}
\keyword{Benjamini-Yekutieli}
\keyword{FDR}
\keyword{BH}
\keyword{BY}
\keyword{multi-testing}
\keyword{Binomial.SGoF}
\keyword{Bayesian.SGoF}
\keyword{Hedenfalk}
