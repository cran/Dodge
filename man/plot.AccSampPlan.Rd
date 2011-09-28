\name{plot.AccSampPlan}
\alias{plot.AccSampPlan}
\title{
plot method for the Dodge package
}
\description{
Creates plots of OC, AOQ, and ATI against proportion nonconforming. If the specific plan also includes other performance measures, these are also plotted against the proportion of nonconforming product.
}
\usage{
\method{plot}{AccSampPlan}(x,y=NULL,...)
}
\arguments{
  \item{x}{an object of class AccSampPlan}
\item{y}{ignored}
\item{...}{further arguments passed to or from other methods.}
}
\details{
At this stage the method only plots the Operating Characteristic (OC) curve, the Average (AOQ) and (ATI) against the proportion (p) of product that is nonconforming. It also plots the curtailed sample size or the average sample number (ASN) against p. Further development is still required.
}
\author{
Jonathan Godfrey with some assistance from Raj Govindaraju
}

\examples{
Plan1 = SSPlanBinomial(1000, 20,1, Plots=FALSE)
plot(Plan1)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
