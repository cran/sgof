\encoding{UTF-8}
\name{summary.BY}
\alias{summary.BY}

\title{Summary of a BY object}

\description{Summary of the most important results given by the BY procedure.}

\usage{
\method{summary}{BY}(object, \dots)
}


\arguments{
  \item{object}{A BY object.}
  \item{\dots}{Additional arguments affecting the summary produced.}
}


\value{

\item{Rejections }{The number of effects declared by the BY method.}
 \item{FDR }{The estimated false discovery rate.}
 \item{Adjusted.pvalues }{Table of adjusted p-values falling under alpha.}

}



\references{
Benjamini Y and Yekutieli D (2001). The control of the false discovery rate in multiple testing under dependency. \emph{Annals of Statistics} 29, 1165--1188.

Dalmasso C, Broet P and Moreau T (2005) A simple procedure for estimating the false discovery rate. \emph{Bioinformatics} 21:660--668

}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}




\seealso{\code{\link{BY}},\code{\link{plot.BY}}}

\examples{


res<-BY(Hedenfalk$x)
summary(res) 
}

\keyword{htest}
\keyword{correlation}
\keyword{Benjamini-Yekutieli}
\keyword{FDR}
\keyword{BY}
\keyword{multitesting}
