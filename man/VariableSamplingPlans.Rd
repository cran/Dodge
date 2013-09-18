\name{VariableSamplingPlans}
\alias{VSPKnown}
\alias{VSPUnknown}
\title{
Variable Sampling Plans
}
\description{
Variable sampling plans for known and unknown sigma, evaluated for given parameters.}
\usage{
VSPKnown(N, n, k, Pa = seq(0, 1, 0.001), Plots = TRUE)

VSPUnknown(N, n, k, Pa = seq(0, 1, 0.001), Plots = TRUE)
}
\arguments{
  \item{N}{the lot size}
  \item{n}{the sample size}
  \item{k}{the acceptability constant}
  \item{Pa}{fraction nonconforming}
  \item{Plots}{logical indicating whether the four plots are required}
}
\author{Raj Govindaraju with minor editing by Jonathan Godfrey}

\examples{
VSPKnown(1000, 20,1)
VSPUnknown(1000, 20,1)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
