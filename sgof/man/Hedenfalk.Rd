\encoding{UTF-8}
\name{Hedenfalk}
\alias{Hedenfalk}
\docType{data}
\title{Hedenfalk data}
\description{The data include information of the micro array study of hereditary breast cancer
of Hedenfalk et al. (2001). Many cases of hereditary breast cancer are due to mutations in either the BRCA1 or the BRCA2 gene. The histopathological changes in these cancers are often characteristic of the mutant gene. They hypothesized that the genes expressed by these two types of tumors are also distinctive, perhaps allowing to identify cases of hereditary breast cancer on the basis of gene-expression profiles.

The patients consisted of 23 with BRCA1 mutations, 17 with BRCA2 mutations, 20 with familial breast cancer, 19 with possibly familial breast cancer and 34 with sporadic breast cancer to determine whether there are distinctive patterns of global gene expression in these three kinds of tumors.

One of the goals of this study was to find genes
differentially expressed between BRCA1- and BRCA2-mutation positive tumors.
Thus, the data included here are p-values obtained from a two- sample t-test analysis on a subset of 3170 genes, as described in Storey and Tibshirani (2003).}

\usage{data(Hedenfalk)}
\format{

  \describe{
    \item{\code{x}}{A numeric vector of 3170 p-values of tests comparing BRCA1 to BRCA2.}
    }
}


\references{
Hedenfalk I, Duggan D, Chen Y, Radmacher M, Bittner M et al. (2001). Gene-Expression Profiles in Hereditary Breast Cancer. New England Journal of Medicine 344, 539-548.

Storey JD and Tibshirani R. (2003). Statistical significance for genome-wide studies. Proceedings of the National Academy of Sciences, 100: 9440-9445.
}
\examples{
data(Hedenfalk)
hist(Hedenfalk$x)
}

\keyword{datasets}
