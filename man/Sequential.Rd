\name{Sequential}
\alias{Sequential}
\alias{SequentialBinomial}
\title{
Attribute Sequential Sampling Plans}
\description{
Designs an attribute sequential sampling plan for given AQL, alpha, LQL, and beta. The user can request plots describing the performance of the plan.
}
\usage{
SequentialBinomial(x, Plots = TRUE)
}
\arguments{
\item{x}{an object of class SeqSampPlan, or at least having the same elements as one.}
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
PlanDesign=SeqDesignBinomial(AQL=0.01, alpha=0.05, LQL=0.04, beta=0.05, Plots=FALSE)
SequentialBinomial(PlanDesign)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
