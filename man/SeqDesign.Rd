\name{SeqDesign}
\alias{SeqDesign}
\alias{SeqDesignBinomial}
\title{
Create a sequential sampling plan
}
\description{
Selects the appropriate sequential sampling plan from the given inputs. The only distribution that has been used in functions thus far is the binomial, but further development is expected.
}
\usage{
SeqDesignBinomial(N, AQL, alpha, LQL, beta, Plots = TRUE)
}
\arguments{
\item{N}{the lot size, ignored for the design of the plan unless the underlying distribution is hypergeometric}
  \item{AQL}{Acceptable quality level}
  \item{alpha}{producer's risk}
  \item{LQL}{Limiting quality level}
  \item{beta}{consumers' risk}
  \item{Plots}{
logical stating if the sequential chart should be plotted}
}

\author{Raj Govindaraju and Jonathan Godfrey}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
