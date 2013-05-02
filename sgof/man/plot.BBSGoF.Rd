\encoding{UTF-8}
\name{plot.BBSGoF}
\alias{plot.BBSGoF}

\title{Plot BBSGoG}

 \description{Plot of a BBSGoF object.}

\usage{
\method{plot}{BBSGoF}(x, \dots)
}

\arguments{
  \item{x}{A BBSGoF object.}
  \item{\dots}{Other parameters to be passed through to plotting functions.}
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

data(Hedenfalk)

res<-BBSGoF(Hedenfalk$x)
plot(res)  
}

\keyword{htest}
\keyword{correlation}
\keyword{BB-SGoF}

\keyword{multitesting}
