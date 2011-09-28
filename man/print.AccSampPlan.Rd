\name{print.AccSampPlan}
\alias{print.AccSampPlan}
\title{
print method for Dodge package
}
\description{
Adds to the base \R functionality for the print() command. An accompanying \code{plot} method is more sophisticated.
}
\usage{
\method{print}{AccSampPlan}(x,...)
}
\arguments{
  \item{x}{an object of class AccSampPlan}
\item{...}{further arguments passed to or from other methods.}
}
\details{
A trivial addition that just prints the object as if it were a data.frame, which it was during its construction. Improvements may be made in future if warranted by demand.
}
\author{
Jonathan Godfrey
}

\seealso{
The corresponding plot method is far more interesting. See \code{\link{plot.AccSampPlan}}
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
