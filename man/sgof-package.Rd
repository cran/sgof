\encoding{UTF-8}
\name{sgof-package}
\alias{sgof-package}
\alias{sgof}
\docType{package}
\title{Multiple hypotheses testing.}
\description{
This package implements three different methods for multitesting problems. 
The \cite{Benjamini and Hochberg (1995)} false discovery rate controlling procedure, 
the method called SGoF (Sequential-Goodness-of-Fit) proposed in \cite{Carvajal-Rodríguez et al. (2009)} 
and the BB-SGoF prodedure (Beta-Binomial SGoF, \cite{de Uña Álvarez, 2012}) which is an adaptation of SGoF method for possibly correlated tests. Number of rejections, FDR and adjusted p-values are computed among other things.
}

\details{
\tabular{ll}{
Package: \tab sgof\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-05-02\cr
License: \tab GPL\cr

LazyLoad: \tab yes\cr
}

This package incorporates the functions \code{BH}, \code{SGoF} and \code{BBSGoF}, which call the methods aforementioned. For a complete list of functions, use library(help="sgof").
}

\author{
Irene Castro Conde and Jacobo de Uña Álvarez

Maintainer:Irene Castro Conde \email{irene.castro@uvigo.es}

}
\references{
Benjamini Y, Hochberg Y (1995): Controlling the False Discovery Rate: A
Practical and Powerful Approach to Multiple Testing. \emph{Journal of the Royal Statistical Society Series B (Methodological)} 57, 289-300.


Carvajal-Rodríguez A, de Uña-Álvarez J, Rolán-Alvarez E (2009) A new
multitest correction (SGoF) that increases its statistical power when increasing the number of tests. \emph{BMC Bioinformatics} 10:209.

Castro Conde, I., de Uña-Álvarez J. (2013) Performance of Beta-Binomial SGoF multitesting method for dependent gene expression levels: a simulation study. \emph{Proceedings of BIOINFORMATICS 2013 – International Conference on Bioinformatics Models, Methods and Algorithms} (Pedro Fernandes, Jordi Solé-Casals, Ana Fred and Hugo Gamboa Eds.), SciTePress. 

Dalmasso C, Broet P, Moreau T (2005) A simple procedure for estimating the false discovery rate. 
Bioinformatics 21:660-668


de Uña-Álvarez J (2011) On the statistical properties of SGoF multitesting method. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 10, Iss. 1, Article 18.

de Uña-Álvarez J (2012). The Beta-Binomial SGoF method for multiple dependent tests. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 11, Iss. 3, Article 14.
}

\keyword{htest}
\keyword{correlation}
\keyword{BB-SGoF}
\keyword{Benjamini-Hochberg}
\keyword{FDR}

\keyword{multitesting}
\keyword{SGOF}
