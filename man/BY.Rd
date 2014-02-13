\encoding{UTF-8}
\name{BY}
\alias{BY}
\alias{print.BY}
\alias{Benjamini-Yekutieli}

\title{Benjamini-Yekutieli (BY) multitesting procedure}

\description{Performs the Benjamini-Yekutieli FDR-controlling method for multiple hypothesis testing. }

\usage{
BY(u, alpha = 0.05)
}

\arguments{
  \item{u }{ A (non-empty) numeric vector of p-values.}

  \item{alpha }{ Numeric value. The significance level of the test.}
}
\details{
The function BY allows for the application of the \cite{Benjamini and Yekutieli (2001)} false discovery rate controlling procedure under positive dependence assumptions.
The false discovery rate is estimated by the simple method proposed by: \cite{Dalmasso, Broet, Moreau (2005)}, by taking n=1 in their formula.
}


\value{
A list containing the following values:
 \item{Rejections }{ The number of effects declared by the BY procedure.}
 \item{FDR }{ The estimated false discovery rate.}
 \item{Adjusted.pvalues }{The adjusted p-values.}
 \item{data }{ The original p-values.}
 \item{alpha }{ The specified significance level for the test.}
 \item{call }{ The matched call.}
}


\references{

Benjamini Y and Yekutieli D (2001). The control of the false discovery rate in multiple testing under dependency. \emph{Annals of Statistics} 29, 1165--1188.

Dalmasso C, Broet P and Moreau T (2005). A simple procedure for estimating the false discovery rate. \emph{Bioinformatics} 21:660--668
}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\examples{



res<-BY(Hedenfalk$x)
summary(res)   #number of rejected nulls, estimated FDR
plot(res)   #adjusted p-values
}

\seealso{\code{\link{plot.BY}},\code{\link{summary.BY}},\code{\link{BBSGoF}}  }

\keyword{htest}
\keyword{Benjamini-Yekutieli}

\keyword{FDR}

\keyword{BY}
\keyword{correlation}
\keyword{multitesting}
