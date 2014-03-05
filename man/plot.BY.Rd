\encoding{UTF-8}
\name{plot.BY}
\alias{plot.BY}

\title{Plot of a BY object}

\description{Plot of the Adjusted p-values given by the BY method. }

\usage{
\method{plot}{BY}(x, \dots)
}

\arguments{
  \item{x}{A BY object.}
  \item{\dots}{Other parameters to be passed through to plotting functions.}
}


\references{
Benjamini Y and Yekutieli D (2001). The control of the false discovery rate in multiple testing under dependency. \emph{Annals of Statistics} 29, 1165--1188.
}


\author{Irene Castro Conde and Jacobo de Uña Álvarez}





\seealso{\code{\link{BY}},\code{\link{summary.BY}}}




\examples{



res<-BY(Hedenfalk$x)
plot(res)  
}

\keyword{htest}
\keyword{Benjamini-Yekutieli}

\keyword{FDR}
\keyword{BY}

\keyword{multi-testing}
\keyword{correlation}
