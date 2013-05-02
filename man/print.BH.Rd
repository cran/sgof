\encoding{UTF-8}
\name{print.BH}
\alias{print.BH}

\title{Print of a BH obejct}

\description{Print of the principal results of the BH method. }


\usage{
\method{print}{BH}(x, \dots)
}
\arguments{
  \item{x}{A BH object.}
  \item{\dots}{Further arguments passed to or from other methods.}
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
plot(res)  
}


\keyword{htest}

\keyword{Benjamini-Hochberg}
\keyword{FDR}

\keyword{multitesting}
