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
  \item{\dots}{Additional arguments affecting the summary produced.}
}


\value{

\item{Rejections }{The number of effects declared by the BH procedure.}
 \item{FDR }{The estimated false discovery rate.}
 \item{Adjusted.pvalues }{Table of adjusted p-values falling under alpha.}

}



\references{
Benjamini Y and Hochberg Y (1995): Controlling the False Discovery Rate: A
Practical and Powerful Approach to Multiple Testing. \emph{Journal of the Royal Statistical Society Series B (Methodological)} 57, 289--300.

Dalmasso C, Broet P and Moreau T (2005) A simple procedure for estimating the false discovery rate. \emph{Bioinformatics} 21:660--668

}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}




\seealso{\code{\link{BH}},\code{\link{plot.BH}}}

\examples{


res<-BH(Hedenfalk$x)
summary(res) 
}

\keyword{htest}

\keyword{Benjamini-Hochberg}
\keyword{FDR}
\keyword{BH}
\keyword{multi-testing}
