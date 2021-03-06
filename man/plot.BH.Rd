\encoding{UTF-8}
\name{plot.BH}
\alias{plot.BH}

\title{Plot of a BH object}

\description{Plot of the Adjusted p-values given by the BH method. }

\usage{
\method{plot}{BH}(x, \dots)
}

\arguments{
  \item{x}{A BH object.}
  \item{\dots}{Other parameters to be passed through to plotting functions.}
}


\references{
Benjamini Y and Hochberg Y (1995). Controlling the False Discovery Rate: A
Practical and Powerful Approach to Multiple Testing. \emph{Journal of the Royal Statistical Society Series B (Methodological)} 57, 289--300.
}


\author{Irene Castro Conde and Jacobo de Uña Álvarez}





\seealso{\code{\link{BH}},\code{\link{summary.BH}},\code{\link{SGoF}}}




\examples{



res<-BH(Hedenfalk$x)
plot(res)  
}

\keyword{htest}

\keyword{Benjamini-Hochberg}
\keyword{FDR}
\keyword{BH}
\keyword{multi-testing}
