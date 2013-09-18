\name{VSPDesign}
\alias{VSPDesign}
\title{
Variable Sampling Plan Design
}
\description{
Design the variable sampling plan for given AQL, alpha, LQL, and beta.
}
\usage{
VSPDesign(AQL, alpha, LQL, beta)
}
\arguments{
  \item{AQL}{Acceptable quality level}
  \item{alpha}{producer's risk}
  \item{LQL}{Limiting quality level}
  \item{beta}{consumers' risk}
}
\author{Raj Govindaraju with minor editing by Jonathan Godfrey}
\examples{
VSPDesign(AQL=0.01, alpha=0.05, LQL=0.04, beta=0.05)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
