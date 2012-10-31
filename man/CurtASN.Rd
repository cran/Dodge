\name{CurtASN}
\alias{CurtASN}
\alias{CurtBinomial}
\title{
Curtailed Average Sample Number
}
\description{
Computes the average sample number for a curtailed inspection plan for single sampling plans. Functionality is currrently available for only the binomial distribution.}
\usage{
CurtBinomial(n, Ac, p = seq(0, 0.5, 0.01), Plots = TRUE)
}
\arguments{
  \item{n}{the sample size (potential)}
  \item{Ac}{the acceptance number}
  \item{p}{a vector of values for the possible fraction of product that is nonconforming}
  \item{Plots}{logical to request generation of the four plots}
}
\author{Raj Govindaraju with minor editing by Jonathan Godfrey}
\examples{
CurtBinomial(20,1)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
