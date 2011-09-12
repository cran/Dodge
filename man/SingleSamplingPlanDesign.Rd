\name{SingleSamplingPlanDesign}
\alias{SSPDesign}
\alias{SSPDesignBinomial}
\alias{SSPDesignPoisson}
\title{
Single Sampling Plan Designs
}
\description{
Design a single sampling plan for given AQL, alpha, LQL, and beta. Currently there are functions for the binomial and Poisson distributions.}
\usage{
SSPDesignBinomial(AQL, alpha, LQL, beta)

SSPDesignPoisson(AQL, alpha, LQL, beta)
}
\arguments{
  \item{AQL}{Acceptable quality level}
  \item{alpha}{producer's risk}
  \item{LQL}{Limiting quality level}
  \item{beta}{consumers' risk}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\references{
Dodge, H.F. and Romig, H.G. (1959) \dQuote{Sampling Inspection Tables, Single and Double Sampling}, 2nd edition, John Wiley and Sons, New York.
}
\author{Raj Govindaraju with minor editing by Jonathan Godfrey}
\examples{
SSPDesignBinomial(0.01, 0.05, 0.04, 0.05)
SSPDesignPoisson(0.01, 0.05, 0.04, 0.05)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
