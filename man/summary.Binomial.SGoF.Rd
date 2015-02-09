\encoding{UTF-8}
\name{summary.Binomial.SGoF}
\alias{summary.Binomial.SGoF}

\title{Summary of a Binomial.SGoF object}

\description{Summary of the most important results given by the Binomial SGoF procedure.}

\usage{
\method{summary}{Binomial.SGoF}(object, \dots)
}


\arguments{
  \item{object}{A Binomial.SGoF object.}
  \item{\dots}{Additional arguments affecting the summary produced.}
}


\value{

\item{Rejections }{The number of effects declared by the Binomial SGoF procedure .}
 \item{FDR }{The estimated false discovery rate.}
 \item{Adjusted.pvalues }{Table of adjusted p-values falling under gamma.}

}



\references{
Carvajal Rodríguez A, de Uña Álvarez J and Rolán Álvarez E (2009). A new
multitest correction (Binomial.SGoF) that increases its statistical power when increasing the number of tests. \emph{BMC Bioinformatics} 10:209.


Castro Conde I and de Uña Álvarez J (2015). Adjusted p-values for SGoF multiple test procedure.  \emph{Biometrical Journal}; 57(1): 108-122. DOI: 10.1002/bimj.201300238

Dalmasso C, Broet P and Moreau T (2005) A simple procedure for estimating the false discovery rate. \emph{Bioinformatics} 21:660--668


}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\seealso{\code{\link{Binomial.SGoF}},\code{\link{plot.Binomial.SGoF}}}

\examples{



p<-runif(387)^2  #387 independent p-values, non-uniform intersection null violated

res<-Binomial.SGoF(p)
summary(res)   #number of rejected nulls, estimated FDR


}

\keyword{htest}

\keyword{multi-testing}
\keyword{Binomial.SGoF}
