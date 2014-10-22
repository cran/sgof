\encoding{UTF-8}
\name{Bayesian.SGoF}
\alias{Bayesian.SGoF}
\alias{print.Bayesian.SGoF}

\title{Bayesian SGoF multi-testing procedure}
\description{Performs the Bayesian SGoF method (\cite{Castro Conde and de Uña Álvarez , 2013 13/06}) for multiple hypothesis testing.}
\usage{
Bayesian.SGoF(u, alpha = 0.05, gamma = 0.05, P0 = 0.5, a0 = 1, b0 = 1)
}


  \arguments{
  \item{u}{A (non-empty) numeric vector of p-values.}
  \item{alpha}{Numeric value. The significance level of the metatest.}
  \item{gamma}{Numeric value. The p-value threshold, so Bayesian SGoF looks for significance in the amount of p-values below gamma.}
  \item{P0}{The a priori probability of the null hypothesis.}
  \item{a0}{The first parameter of the a priori beta distribution.}
  \item{b0}{The second parameter of the a priori beta distribution.}
}

\details{
Bayesian SGoF (\cite{Castro Conde  and de Uña Álvarez, 2013 13/06}) is an adaptation of SGoF method to the Bayesian paradigm, in which the proportion of p-values falling below \code{gamma} is random. This method has two main steps. First, Bayesian SGoF performs a pre-test at level \code{alpha} which decides if the complete null hypothesis should be rejected or not. 
This Bayesian pre-test is based on lower bounds of the a posteriori probability of H0 (computed using the  default a priori probability \code{P0}=.5, unless otherwise is indicated,  and a family of a priori beta distributions located at the null and indexed by a correlation factor). Second, the number of rejections is computed constructing an interval for the 'excess of significant cases', analogously to the SGoF procedure. For this, the posterior distribution of the proportion of p-values falling below \code{gamma} is used; this posterior distribution is calculated on the basis of the default priors  \code{a0}=\code{b0}=1, unless otherwise is indicated. Besides, the posterior probability that the complete null hypothesis is true is computed using P0, a0 and b0.
One important difference between the frequentist and the Bayesian SGoF is that the Bayesian setting induces (and hence allows for) a dependence structure among the p-values; this is very interesting for real problems where correlation is present. From a less philosophical point of view, in practice Bayesian SGoF may be more conservative than frequentist SGoF, particularly when the number of tests is small; this is due to the fact that Bayesian testing of point nulls is much more conservative than its frequentist counterpart and, therefore, the pre-test part of Bayesian SGoF may play a very important role.
Typically the choice \code{alpha}=\code{gamma} will be used for Bayesian.SGoF; this common value will be set as one of the usual significance levels (0.001, 0.01, 0.05, 0.1). Note however that \code{alpha} and \code{gamma} have different roles.
The false discovery rate is estimated by the simple method proposed by: \cite{Dalmasso , Broet , Moreau  (2005)}, by taking n=1 in their formula.
}





\value{
A list containing the following values:
 \item{Rejections }{The number of effects declared by Bayesian SGoF.}
 \item{FDR }{The estimated false discovery rate.}
 \item{Posterior}{The posterior probability that the complete null hypothesis is true depending on a0, b0 and P0.}
 \item{s}{The amount of p-values falling below gamma.}
 \item{s.alpha}{Critical point at level alpha of the Bayesian pre-test for the complete null depending on P0.}
 \item{data }{The original p-values.}
 \item{alpha }{The specified significance level for the metatest.}
 \item{gamma }{The specified p-value threshold.}
 \item{P0}{The specified a priori probability of the null hypothesis.}
 \item{a0}{The first  specified parameter of the a priori beta distribution.}
 \item{b0}{The second specified parameter of the a priori beta distribution.}
 \item{call }{The matched call.}
}
\references{

Castro Conde I and de Uña Álvarez J (2013). SGoF multitesting method under the Bayesian paradigm. \emph{Discussion Papers in Statistics and Operation Research}. Report 13/06. Statistics and OR Department. University of Vigo. Under
revision in Journal of Statistical Computation and Simulation.

Dalmasso C, Broet P and Moreau T (2005) A simple procedure for estimating the false discovery rate. \emph{Bioinformatics} 21:660--668

}

\author{Irene Castro Conde and Jacobo de Uña Álvarez}



\seealso{\code{\link{summary.Bayesian.SGoF}}}

\examples{


res<-Bayesian.SGoF(Hedenfalk$x)
summary(res)   

}

\keyword{htest}

\keyword{Bayesian.SGoF}
\keyword{multi-testing}
\keyword{correlation}