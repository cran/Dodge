\name{LSP}
\alias{LSP}
\title{
Lot Sensitive Compliance Sampling Plans
}
\description{
The lot sensitive compliance sampling plans for given parameters.
}
\usage{
LSP(N, LTPD, beta, p = seq(0, 0.3, 0.001), Plots = TRUE)
}
\arguments{
  \item{N}{the lot size}
  \item{LTPD}{the lot tolerance percent defective, also known as the limiting quality}
  \item{beta}{consumer risk}
  \item{p}{fraction nonconforming}
  \item{Plots}{logical indicating if the four plots are required}
}
\references{
Schilling, E.G. (1978) \dQuote{A Lot Sensitive Sampling Plan for Compliance Testing and Acceptance Inspection}, \emph{Journal of Quality Technology} \bold{10}(2), pp47-51.
}
\author{Raj Govindaraju with minor editing by Jonathan Godfrey}
\examples{
LSP(1000, 0.04,0.05)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
