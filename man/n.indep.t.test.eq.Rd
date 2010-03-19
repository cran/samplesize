\name{n.indep.t.test.eq}
\alias{n.indep.t.test.eq}
\title{Sample size for independent Student's t-test with equal group size}
\description{
  Function computes sample size for Student's independent t-test with equal group size
}
\usage{
n.indep.t.test.eq(power = 0.8, alpha = 0.95, mean.diff = 0.8, sd.est = 0.83)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{power}{required power 1-beta}
  \item{alpha}{required Level I-error 1-alpha}
  \item{mean.diff}{required minimum difference between group means}
  \item{sd.est}{standard deviation in groups}
}
\value{
  \item{sample size}{sample size in each group}
}
\references{Bock J., Bestimmung des Stichprobenumfangs fuer biologische Experimente und kontrollierte klinische Studien. Oldenbourg 1998}
\author{Ralph Scherer}
\seealso{n.indep.t.test.neq(), n.paired.t.test(), n.welch.test()}
\examples{
 n.indep.t.test.eq(power = 0.8, alpha = 0.95, mean.diff = 0.8, sd.est = 0.83)
}
\keyword{htest}
