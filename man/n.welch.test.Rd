\name{n.welch.test}
\alias{n.welch.test}
\title{Sample size for Student's t-test with Welch approximation}
\description{
  Function computes sample size for Student's t-test with Welch approximation for unequal variances
}
\usage{
n.welch.test(power = 0.8, alpha = 0.95, mean.diff = 2, sd.est1 = 1, sd.est2 = 2.65)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{power}{required power 1-beta}
  \item{alpha}{required Level I-error 1-alpha}
  \item{mean.diff}{required minimum difference between group means}
  \item{sd.est1}{standard deviation in group 1}
  \item{sd.est2}{standard deviation in group 2}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{sample size}{Total sample size N}
  \item{sample size n1}{sample size in group 1}
  \item{sample size n2}{sample size in group 2}
}
\references{Bock J., Bestimmung des Stichprobenumfangs fuer biologische Experimente und kontrollierte klinische Studien. Oldenbourg 1998}
\author{Ralph Scherer}
\seealso{n.inep.t.test.neq(), n.paired.t.test(), n.inep.t.test.eq()}
\examples{
 n.welch.test(power = 0.8, alpha = 0.95, mean.diff = 2, sd.est1 = 1, sd.est2 = 2.65)
}
\keyword{htest}
