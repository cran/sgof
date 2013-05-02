\encoding{UTF-8}
\name{plot.SGoF}
\alias{plot.SGoF}

\title{Plot of a SGoF object}

\description{Plot the Adjusted p-values given by the  SGoF method. }

\usage{
\method{plot}{SGoF}(x, \dots)
}

\arguments{
  \item{x}{A SGoF object.}
  \item{\dots}{Other parameters to be passed through to plotting functions.}
}


\references{
Carvajal-Rodríguez A, de Uña-Álvarez J, Rolán-Alvarez E (2009) A new
multitest correction (SGoF) that increases its statistical power when increasing the number of tests. BMC Bioinformatics 10:209.

de Uña-Álvarez J (2011) On the statistical properties of SGoF multitesting method. Statistical Applications in Genetics and Molecular Biology, Vol. 10, Iss. 1, Article 18.

de Uña-Álvarez J (2012). The Beta-Binomial SGoF method for multiple dependent tests. Statistical Applications in Genetics and Molecular Biology, Vol. 11, Iss. 3, Article 14.
}


\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\seealso{\code{\link{SGoF}}}






\examples{

data(Hedenfalk)

res<-SGoF(Hedenfalk$x)
plot(res)  
}


\keyword{htest}





\keyword{multitesting}
\keyword{SGOF}