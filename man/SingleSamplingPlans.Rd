\name{SingleSamplingPlans}
\alias{SSPlanBinomial}
\alias{SingleSamplingPlans}
\alias{SSPlanHyper}
\alias{SSPlanPoisson}
\title{
Single Sampling Plans
}
\description{
Single sampling plans for the binomial, hypergeometric and Poisson distributions.
}
\usage{
SSPlanBinomial(N, n, Ac, p = seq(0, 0.3, 0.001), Plots = TRUE)

SSPlanHyper(N, n, Ac, p = seq(0, 0.3, 0.001), Plots = TRUE)

SSPlanPoisson(N, n, Ac, p = seq(0, 0.3, 0.001), Plots = TRUE)
}
\arguments{
  \item{N}{the lot size}
  \item{n}{the sample size}
  \item{Ac}{the acceptance number, being the maximum allowable number of nonconforming units or nonconformities}
  \item{p}{a vector of values for the possible fraction of product that is nonconforming}
  \item{Plots}{logical to request generation of the four plots}
}
\references{
Dodge, H.F. and Romig, H.G. (1959) \dQuote{Sampling Inspection Tables, Single and Double Sampling}, 2nd edition, John Wiley and Sons, New York.
}
\author{Raj Govindaraju with minor editing by Jonathan Godfrey}
\examples{
SSPlanBinomial(1000, 20,1)
SSPlanHyper(5000, 200,3)
SSPlanPoisson(1000, 20,1)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
