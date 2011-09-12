\name{Sequential}
\alias{Sequential}
\title{
Attribute Sequential Sampling Plans}
\description{
Designs an attribute sequential sampling plan for given AQL, alpha, LQL, and beta. The user can request plots describing the performance of the plan.
}
\usage{
Sequential(AQL, alpha, LQL, beta, Plots = TRUE)
}
\arguments{
  \item{AQL}{Acceptable quality level}
  \item{alpha}{producer's risk}
  \item{LQL}{Limiting quality level}
  \item{beta}{consumers' risk}
  \item{Plots}{logical indicating if the four plots should be returned}
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

\examples{
Sequential(AQL=0.01, alpha=0.05, LQL=0.04, beta=0.05)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
