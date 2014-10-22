\encoding{UTF-8}
\name{summary.Bayesian.SGoF}
\alias{summary.Bayesian.SGoF}

\title{Summary of a Bayesian.SGoF object}

\description{Summary of the most important results given by the Bayesian SGoF procedure.}

\usage{
\method{summary}{Bayesian.SGoF}(object, \dots)
}


\arguments{
  \item{object}{A Bayesian.SGoF object.}
  \item{\dots}{Additional arguments affecting the summary produced.}
}


\value{

\item{Rejections }{The number of effects declared by the Bayesian SGoF procedure.}
 \item{FDR }{The estimated false discovery rate.}
\item{Posterior}{The posterior probability that the complete null hypothesis is true considering the non informative election.}
\item{s}{The proportion of p-values falling below gamma.}
\item{s.alpha}{The first integer from 0 to n such that the Bayesian pre-test rejects the complete null hypothesis.}

}



\references{

Castro Conde I and de Uña Álvarez J (2013). SGoF multitesting method under the Bayesian paradigm. \emph{Discussion Papers in Statistics and Operation Research}. Report 13/06. Statistics and OR Department. University of Vigo. Under
revision in Journal of Statistical Computation and Simulation.

Dalmasso C, Broet P and Moreau T (2005) A simple procedure for estimating the false discovery rate. \emph{Bioinformatics} 21:660--668

}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\seealso{\code{\link{Bayesian.SGoF}}}

\examples{



res<-Bayesian.SGoF(Hedenfalk$x)
summary(res)   


}

\keyword{htest}
\keyword{correlation}
\keyword{multi-testing}
\keyword{Bayesian.SGoF}

