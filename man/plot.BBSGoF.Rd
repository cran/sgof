\encoding{UTF-8}
\name{plot.BBSGoF}
\alias{plot.BBSGoF}

\title{Plot of a BBSGoF object.}

 \description{Plot of a BBSGoF object}

\usage{
\method{plot}{BBSGoF}(x, \dots)
}

\arguments{
  \item{x}{A BBSGoF object.}
  \item{\dots}{Other parameters to be passed through to plotting functions.}
}



\references{

de Uña Álvarez J (2012). The Beta-Binomial SGoF method for multiple dependent tests. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 11, Iss. 3, Article 14.
}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}




\seealso{\code{\link{BBSGoF}},\code{\link{summary.BBSGoF}}}





\examples{

p<-runif(387)^2  #387 independent p-values, non-uniform intersection null violated

res<-BBSGoF(p)
	
par(mfrow=c(2,2))
plot(res)   #Tarone test, within-block correlation, beta density (for automatic k),
	        #and decision plot (number of rejected nulls)

}

\keyword{htest}
\keyword{correlation}
\keyword{BBSGoF}

\keyword{multi-testing}
