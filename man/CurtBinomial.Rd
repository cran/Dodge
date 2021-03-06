% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Curtailed.R
\name{CurtBinomial}
\alias{CurtBinomial}
\alias{CurtASN}
\title{Curtailed Average Sample Number}
\usage{
CurtBinomial(n, Ac, p = seq(0, 0.5, 0.01), Plots = TRUE)
}
\arguments{
\item{n}{the sample size (potential)}

\item{Ac}{the acceptance number}

\item{p}{a vector of values for the possible fraction of product that is
nonconforming}

\item{Plots}{logical to request generation of the four plots}
}
\description{
Computes the average sample number for a curtailed inspection plan for
single sampling plans. Functionality is currently available for only the
binomial distribution.
}
\examples{

CurtBinomial(20,1)

}
\author{
Raj Govindaraju with minor editing by Jonathan Godfrey
}
