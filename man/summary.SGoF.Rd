\encoding{UTF-8}
\name{summary.SGoF}
\alias{summary.SGoF}

\title{Summary of a SGoF object}

\description{Summary of the most important results given by the SGoF procedure.}

\usage{
\method{summary}{SGoF}(object, \dots)
}


\arguments{
  \item{object}{A SGoF object.}
  \item{\dots}{additional arguments affecting the summary produced.}
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

data(Hedenfalk)

res<-SGoF(Hedenfalk$x)
summary(res)   #number of rejected nulls, estimated FDR


}

\keyword{htest}

\keyword{multitesting}
\keyword{SGOF}
