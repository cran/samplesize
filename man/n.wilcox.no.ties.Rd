\name{n.wilcox.no.ties}
\alias{n.wilcox.no.ties}
\title{Sample size for Wilcoxon-Mann-Whitney test without ties}
\description{
  Function computes sample size for Wilcoxon test without ties
}
\usage{
n.wilcox.no.ties(beta = 0.2, alpha = 0.05, t = 0.53, p = c(0.66, 0.15, 0.19), q = c(0.61, 0.23, 0.16))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta}{required Level II-error}
  \item{alpha}{required Level I-error}
  \item{t}{treatment fraction n/N, n is sample size of group B}
  \item{p}{vector of rates of group A}
  \item{q}{vector of rates of group B}
}
\details{
  p and q values are rates of group A and B in categories C_1,...C_D
}
\value{
  \item{N}{Total sample size}
}
\references{Zhao YD, Rahardja D, Qu Yongming. Sample size calculation for the Wilcoxon-Mann-Whitney test adjsuting for ties. Statistics in Medicine 2008; 27:462-468}
\author{Ralph Scherer}
\seealso{n.wilcox.w.ties}
\examples{
n.wilcox.no.ties(beta = 0.2, alpha = 0.05, t = 0.53, p = c(0.66, 0.15, 0.19), q = c(0.61, 0.23, 0.16))
}
\keyword{htest}
