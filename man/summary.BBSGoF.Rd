\encoding{UTF-8}
\name{summary.BBSGoF}
\alias{summary.BBSGoF}

\title{Summary of a BBSGoF object}

\description{Summary of the most important results given by the BBSGoF procedure.}

\usage{
\method{summary}{BBSGoF}(object,\dots)
}


\arguments{
  \item{object}{A BBSGoF object.}
  \item{\dots}{Additional arguments affecting the summary produced.}
}


\value{

\item{Rejections }{The number of effects declared by BB-SGoF with automatic k.}
 \item{FDR }{The estimated false discovery rate.}
 \item{Adjusted.pvalues }{Table of adjusted p-values falling under gamma.}
 \item{Tarone.pvalue.auto }{The p-values of Tarone’s test for the automatic k.}
 \item{beta.parameters }{The estimated parameters of the Beta(a,b) model for the automatic k.}
 \item{betabinomial.parameters }{The estimated parameters of the Betabinomial(p,ro) model for the automatic k.}
 \item{sd.betabinomial.parameters }{The standard deviation of the estimated parameters of the Betabinomial(p,ro) model for the automatic k.}
 \item{automatic.blocks  }{The automatic number of blocks.}

}


\references{
Dalmasso C, Broet P and Moreau T (2005) A simple procedure for estimating the false discovery rate. \emph{Bioinformatics} 21:660--668

de Uña Álvarez J (2012). The Beta-Binomial SGoF method for multiple dependent tests. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 11, Iss. 3, Article 14.
}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}




\seealso{\code{\link{BBSGoF}},\code{\link{plot.BBSGoF}}}

\examples{

p<-runif(387)^2  #387 p-values, intersection null violated

res<-BBSGoF(p)
summary(res)    #automatic number of blocks, number of rejected nulls, 
		#estimated FDR, beta and beta-binomial parameters,
		#Tarone test of no correlation 


}

\keyword{htest}
\keyword{correlation}
\keyword{BBSGoF}

\keyword{multitesting}
