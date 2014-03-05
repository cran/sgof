\encoding{UTF-8}
\name{plot.SGoF}
\alias{plot.SGoF}

\title{Plot of a SGoF object}

\description{Plot the Adjusted p-values given by the Conservative SGoF method. }

\usage{
\method{plot}{SGoF}(x, \dots)
}

\arguments{
  \item{x}{A SGoF object.}
  \item{\dots}{Other parameters to be passed through to plotting functions.}
}


\references{

Castro Conde I and de Uña Álvarez J (2013). Adjusted p-values for SGoF multitesting method.  \emph{Discussion Papers in Statistics and Operation Research}. Report 13/05. Statistics and OR Department. University of Vigo. \url{https://webs.uvigo.es/depc05/reports/13_05.pdf}


de Uña Álvarez J (2011). On the statistical properties of SGoF multitesting method. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 10, Iss. 1, Article 18.
}


\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\seealso{\code{\link{SGoF}},\code{\link{summary.SGoF}}}






\examples{

p<-runif(387)^2  #387 independent p-values, non-uniform intersection null violated

res<-SGoF(p)
plot(res)  
}


\keyword{htest}


\keyword{multi-testing}
\keyword{SGoF}