\name{ChainSamplingPlans}
\alias{ChainSamplingPlans}
\alias{ChainBinomial}
\alias{ChainPoisson}
\title{
Chain Sampling Plans
}
\description{
Chain Sampling Plans for the binomial and Poisson distributions. 
}
\usage{
ChainBinomial(N, n, i, p = seq(0, 0.2, 0.001), Plots = TRUE)

ChainPoisson(N, n, i, p = seq(0, 0.3, 0.001), Plots = TRUE)
}
\arguments{
  \item{N}{the lot size}
  \item{n}{the sample size}
  \item{i}{the number of preceding lots that are  free from nonconforming units for the lot to be accepted}
  \item{p}{a vector of values for the possible fraction of product that is nonconforming}
  \item{Plots}{logical to request generation of the four plots}
}
\value{
A matrix containing the argument \code{p} as supplied and the calculated OC, ATI and ???}
\references{
Dodge, H.F. (1955) \dQuote{Chain Sampling Inspection Plan},
\emph{Industrial Quality Control} \bold{11}(4), pp10-13.}
\author{Raj Govindaraju with minor editing by Jonathan Godfrey}
\examples{
require(Dodge)
ChainBinomial(1000, 20,3)
ChainPoisson(1000, 20,3)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
