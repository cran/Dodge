\name{DoubleSamplingPlans}
\alias{DSPlanBinomial}
\alias{DoubleSamplingPlans}
\alias{DSPlanPoisson}
\title{Double Sampling Plans}
\description{
Double Sampling Plans for the binomial and Poisson distributions.}
\usage{
DSPlanBinomial(N, n1, n2, Ac1, Re1, Ac2, p = seq(0, 0.25, 0.005), Plots = TRUE)

DSPlanPoisson(N, n1, n2, Ac1, Re1, Ac2, p = seq(0, 0.25, 0.005), Plots = TRUE)
}
\arguments{
  \item{N}{the lot size}
  \item{n1}{the sample size in the first stage of the plan}
  \item{n2}{the sample size in the second stage of the plan}
  \item{Ac1}{the first stage acceptance number}
  \item{Re1}{the first stage rejection number} 
  \item{Ac2}{the second stage acceptance number}
  \item{p}{a vector of values for the possible fraction of product that is nonconforming}
  \item{Plots}{logical to request generation of the four plots}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
}
\references{
Dodge, H.F. and Romig, H.G. (1959) \dQuote{Sampling Inspection Tables, Single and Double Sampling}, 2nd edition, John Wiley and Sons, New York.
}
\author{Raj Govindaraju with minor editing by Jonathan Godfrey}

\examples{
DSPlanBinomial(1000, 10, 10, 0, 2, 1)
DSPlanPoisson(1000, 10, 10, 0,2, 1)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
