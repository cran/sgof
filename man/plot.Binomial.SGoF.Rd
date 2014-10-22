\encoding{UTF-8}
\name{plot.Binomial.SGoF}
\alias{plot.Binomial.SGoF}

\title{Plot of a Binomial.SGoF object}

\description{Plot the Adjusted p-values given by the Binomial SGoF method. }

\usage{
\method{plot}{Binomial.SGoF}(x, \dots)
}

\arguments{
  \item{x}{A Binomial.SGoF object.}
  \item{\dots}{Other parameters to be passed through to plotting functions.}
}


\references{
Carvajal Rodríguez A, de Uña Álvarez J and Rolán Álvarez E (2009) A new
multitest correction (Binomial.SGoF) that increases its statistical power when increasing the number of tests. \emph{BMC Bioinformatics} 10:209.

Castro Conde I and de Uña Álvarez J (2013). Adjusted p-values for SGoF multiple test procedure.  \emph{Biometrical Journal: in press}. DOI: 10.1002/bimj.201300238

}


\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\seealso{\code{\link{Binomial.SGoF}},\code{\link{summary.Binomial.SGoF}}}


\examples{

p<-runif(387)^2  #387 independent p-values, non-uniform intersection null violated

res<-Binomial.SGoF(p)
plot(res)  
}


\keyword{htest}

\keyword{multi-testing}
\keyword{Binomial.SGoF}