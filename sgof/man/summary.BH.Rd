\encoding{UTF-8}
\name{summary.BH}
\alias{summary.BH}

\title{Summary of a BH object}

\description{Summary of the most important results given by the BH procedure.}

\usage{
\method{summary}{BH}(object, \dots)
}


\arguments{
  \item{object}{A BH object.}
  \item{\dots}{additional arguments affecting the summary produced.}
}


\value{

\item{Rejections }{The number of effects declared by BB-SGoF with automatic k.}
 \item{FDR }{The estimated false discovery rate.}
 \item{Adjusted.pvalues }{Table of adjusted p-values falling under alpha.}

}



\references{
Benjamini Y, Hochberg Y (1995): Controlling the False Discovery Rate: A
Practical and Powerful Approach to Multiple Testing. Journal of the Royal Statistical Society Series B (Methodological) 57, 289-300.

}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}




\seealso{\code{\link{BH}}}

\examples{
data(Hedenfalk)

res<-BH(Hedenfalk$x)
summary(res) 
}

\keyword{htest}

\keyword{Benjamini-Hochberg}
\keyword{FDR}

\keyword{multitesting}
