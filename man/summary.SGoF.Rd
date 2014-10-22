\encoding{UTF-8}
\name{summary.SGoF}
\alias{summary.SGoF}

\title{Summary of a SGoF object}

\description{Summary of the most important results given by the Conservative SGoF procedure.}

\usage{
\method{summary}{SGoF}(object, \dots)
}


\arguments{
  \item{object}{A SGoF object.}
  \item{\dots}{Additional arguments affecting the summary produced.}
}


\value{

\item{Rejections }{The number of effects declared by SGoF.}
 \item{FDR }{The estimated false discovery rate.}
 \item{Adjusted.pvalues }{Table of adjusted p-values falling under gamma.}

}



\references{

Castro Conde I and de Uña Álvarez J (2013). Adjusted p-values for SGoF multiple test procedure.  \emph{Biometrical Journal: in press}. DOI: 10.1002/bimj.201300238

Dalmasso C, Broet P and Moreau T (2005) A simple procedure for estimating the false discovery rate. \emph{Bioinformatics} 21:660--668


de Uña Álvarez J (2011). On the statistical properties of SGoF multitesting method. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 10, Iss. 1, Article 18.
}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\seealso{\code{\link{SGoF}},\code{\link{plot.SGoF}}}

\examples{

p<-runif(387)^2  #387 independent p-values, non-uniform intersection null violated

res<-SGoF(p)
summary(res)   #number of rejected nulls, estimated FDR


}

\keyword{htest}

\keyword{multi-testing}
\keyword{SGoF}
