\encoding{UTF-8}
\name{BH}
\alias{BH}
\alias{Benjamini-Hochberg}

\title{Benjamini-Hochberg (BH) multitesting procedure}

\description{Performs the Benjamini-Hochberg FDR-controlling method for multiple hypotheses testing. }

\usage{
BH(u, alpha = 0.05)
}

\arguments{
  \item{u }{        A (non-empty) numeric vector of p-values.}

  \item{alpha }{        Numeric value. The significance level of the test.}
}
\details{
The function BH allows for the application of the \cite{Benjamini and Hochberg} (1995) false discovery rate controlling procedure.
The false discovery rate is estimated by the simple method proposed by: \cite{Dalmasso C, Broet P, Moreau T} (2005) A simple procedure for estimating the false discovery rate. 
Bioinformatics 21:660-668, by taking n=1 in their formula.
}


\value{
A list containing the following values:
 \item{Rejections }{ The number of effects declared by BB-SGoF with automatic k.}
 \item{FDR }{ The estimated false discovery rate.}
 \item{ Adjusted.pvalues }{The adjusted p-values.}
 \item{data }{ The original p-values.}
 \item{alpha }{ The specified significance level for the test.}
 \item{call }{ The matched call.}
}


\references{
Benjamini Y, Hochberg Y (1995): Controlling the False Discovery Rate: A
Practical and Powerful Approach to Multiple Testing. Journal of the Royal Statistical Society Series B (Methodological) 57, 289-300.

Dalmasso C, Broet P, Moreau T (2005) A simple procedure for estimating the false discovery rate. 
Bioinformatics 21:660-668
}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\examples{

data(Hedenfalk)

res<-BH(Hedenfalk$x)
summary(res)   #number of rejected nulls, estimated FDR
plot(res)   #adjusted p-values
}

\keyword{htest}


\keyword{Benjamini-Hochberg}
\keyword{FDR}

\keyword{multitesting}
