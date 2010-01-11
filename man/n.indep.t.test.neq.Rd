\name{n.indep.t.test.neq}
\alias{n.indep.t.test.neq}
\title{Sample size for independent Student's t-test with unequal group size}
\description{
  Function computes sample size for independent Student's t-test with unequal group size
}
\usage{
n.indep.t.test.neq(power = 0.8, alpha = 0.95, mean.diff = 0.8, sd.est = 0.83, k = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{power}{required power 1-beta}
  \item{alpha}{required Level I-error 1-alpha}
  \item{mean.diff}{required minimum difference between group means}
  \item{sd.est}{standard deviation in groups}
  \item{k}{n_2=n_1*k}
}
\details{
  n_1= size group 1, n_2= size group 2, N=n_1+n_2
}
\value{
  \item{N}{total sample size}
  \item{n.1}{sample size group 1}
  \item{n.2}{sample size group 2}
}
\references{Bock J., Bestimmung des Stichprobenumfangs fuer biologische Experimente und kontrollierte klinische Studien. Oldenbourg 1998}
\author{Ralph Scherer}
\seealso{n.indep.t.test.eq(), n.paired.t.test(), n.welch.test()}
\examples{n.indep.t.test.neq(power = 0.8, alpha = 0.95, mean.diff = 0.8, sd.est = 0.83, k = 0.5)}
\keyword{htest}
