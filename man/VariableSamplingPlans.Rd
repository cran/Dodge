\name{VariableSamplingPlans}
\alias{VSPKnown}
\alias{VSPUnknown}
\title{
Variable Sampling Plans
}
\description{
Variable sampling plans for known and unknown sigma evaluated for given parameters.}
\usage{
VSPKnown(N, n, k, p = seq(0, 1, 0.001), Plots = TRUE)

VSPUnknown(N, n, k, p = seq(0, 1, 0.001), Plots = TRUE)
}
\arguments{
  \item{N}{the lot size}
  \item{n}{the sample size}
  \item{k}{the acceptability constant}
  \item{p}{fraction nonconforming}
  \item{Plots}{logical indicating whether the four plots are required}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Raj Govindaraju with minor editing by Jonathan Godfrey}
\note{
%%  ~~further notes~~
}

\examples{
VSPKnown(1000, 20,1)
VSPUnknown(1000, 20,1)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
