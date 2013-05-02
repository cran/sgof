\encoding{UTF-8}
\name{print.SGoF}
\alias{print.SGoF}

\title{Print of a SGoF object}

\description{Print of the principal results of the SGoF method. }

\usage{
\method{print}{SGoF}(x, \dots)
}



\arguments{
  \item{x}{A SGoF object.}
  \item{\dots}{Further arguments passed to or from other methods.}
}


\value{

\item{Rejections }{The number of effects declared by BB-SGoF with automatic k.}
 \item{FDR }{The estimated false discovery rate.}
 \item{Adjusted.pvalues }{Table of adjusted p-values falling under gamma.}

}



\references{
Carvajal-Rodríguez A, de Uña-Álvarez J, Rolán-Alvarez E (2009) A new
multitest correction (SGoF) that increases its statistical power when increasing the number of tests. BMC Bioinformatics 10:209.

de Uña-Álvarez J (2011) On the statistical properties of SGoF multitesting method. Statistical Applications in Genetics and Molecular Biology, Vol. 10, Iss. 1, Article 18.
de Uña-Álvarez J (2012). The Beta-Binomial SGoF method for multiple dependent tests. Statistical Applications in Genetics and Molecular Biology, Vol. 11, Iss. 3, Article 14.
}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\seealso{\code{\link{SGoF}}}

\examples{

p<-runif(387)^2  #387 p-values, intersection null violated
res<-SGoF(p,alpha=0.05,gamma=0.05)
summary(res) #number of rejected nulls, estimated FDR

}

\keyword{htest}


\keyword{multitesting}
\keyword{SGOF}