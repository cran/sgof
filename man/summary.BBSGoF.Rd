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
  \item{\dots}{additional arguments affecting the summary produced.}
}


\value{

\item{Rejections }{The number of effects declared by BB-SGoF with automatic k.}
 \item{FDR }{The estimated false discovery rate.}
 \item{Adjusted.pvalues }{Table of adjusted p-values falling under gamma.}
 \item{Tarone.pvalue.auto }{The p-values of Tarone’s test for the automatic k.}
 \item{beta.parameters }{The estimated parameters of the Beta(a,b) model for the automatic k.}
 \item{betabinomial.parameters }{The estimated parameters of the Betabinomial(p,?) model for the automatic k.}
 \item{sd.betabinomial.parameters }{The standard deviation of the estimated parameters of the Betabinomial(p,?) model for the automatic k.}
 \item{automatic.blocks  }{The automatic number of blocks.}

}


\references{
Carvajal-Rodríguez A, de Uña-Álvarez J, Rolán-Alvarez E (2009) A new
multitest correction (SGoF) that increases its statistical power when increasing the number of tests. BMC Bioinformatics 10:209.


Castro Conde, I., de Uña-Álvarez J. (2013) Performance of Beta-Binomial SGoF multitesting method for dependent gene expression levels: a simulation study. Proceedings of BIOINFORMATICS 2013 – International Conference on Bioinformatics Models, Methods and Algorithms (Pedro Fernandes, Jordi Solé-Casals, Ana Fred and Hugo Gamboa Eds.), SciTePress. 

de Uña-Álvarez J (2011) On the statistical properties of SGoF multitesting method. Statistical Applications in Genetics and Molecular Biology, Vol. 10, Iss. 1, Article 18.

de Uña-Álvarez J (2012). The Beta-Binomial SGoF method for multiple dependent tests. Statistical Applications in Genetics and Molecular Biology, Vol. 11, Iss. 3, Article 14.
}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}




\seealso{\code{\link{BBSGoF}}}

\examples{

p<-runif(387)^2  #387 p-values, intersection null violated

res<-BBSGoF(p)
summary(res)    #automatic number of blocks, number of rejected nulls, 
		#estimated FDR, beta and beta-binomial parameters,
		#Tarone test of no correlation 


}

\keyword{htest}
\keyword{correlation}
\keyword{ BB-SGoF}

\keyword{multitesting}
