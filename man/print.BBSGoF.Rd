\encoding{UTF-8}
\name{print.BBSGoF}
\alias{print.BBSGoF}

\title{ Print of a BBSGoF object}

\usage{
\method{print}{BBSGoF}(x, \dots)
}


\description{Print of the principal results of the BBSGoF method. }

\arguments{
  \item{x}{A BBSGoF object.}
  \item{\dots}{Further arguments passed to or from other methods.}
}


\references{
Carvajal-Rodríguez A, de Uña-Álvarez J, Rolán-Alvarez E (2009) A new
multitest correction (SGoF) that increases its statistical power when increasing the number of tests. \emph{BMC Bioinformatics} 10:209.


Castro Conde, I, de Uña-Álvarez J (2013) Performance of Beta-Binomial SGoF multitesting method for dependent gene expression levels: a simulation study. \emph{Proceedings of BIOINFORMATICS 2013 – International Conference on Bioinformatics Models, Methods and Algorithms} (Pedro Fernandes, Jordi Solé-Casals, Ana Fred and Hugo Gamboa Eds.), SciTePress. 

Dalmasso C, Broet P, Moreau T (2005) A simple procedure for estimating the false discovery rate. 
Bioinformatics 21:660-668

de Uña-Álvarez J (2011) On the statistical properties of SGoF multitesting method. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 10, Iss. 1, Article 18.

de Uña-Álvarez J (2012). The Beta-Binomial SGoF method for multiple dependent tests. \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 11, Iss. 3, Article 14.
}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}






\seealso{\code{\link{BBSGoF}}}



\examples{

p<-runif(387)^2  #387 p-values, intersection null violated

res<-BBSGoF(p)
	
par(mfrow=c(2,2))
plot(res)   #Tarone test, within-block correlation, beta density (for automatic k),
	    #and decision plot (number of rejected nulls)

}



\keyword{htest}
\keyword{correlation}
\keyword{ BB-SGoF}


\keyword{multitesting}
