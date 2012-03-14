\name{print.AccSampPlan}
\alias{print.AccSampPlan}
\alias{print.CurtSampPlan}
\alias{print.SeqSampPlan}
\title{
print methods for thee Dodge package
}
\description{
Adds to the base \R functionality for the print() command. The accompanying \code{plot} methods are more sophisticated.
}
\usage{
\method{print}{AccSampPlan}(x,...)

\method{print}{CurtSampPlan}(x,...)

\method{print}{SeqSampPlan}(x,...)
}
\arguments{
  \item{x}{an object of class AccSampPlan, CurtSampPlan, or SeqSampPlan}
\item{...}{further arguments passed to or from other methods.}
}
\details{
These methods print the most necessary elements of the corresponding objects.
}
\author{
Jonathan Godfrey
}

\seealso{
The corresponding plot method is far more interesting. See \code{\link{plot.AccSampPlan}} for example.
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
