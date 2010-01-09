\name{n.paired.t.test}
\alias{n.paired.t.test}
\title{Sample size for paired Student's t-test}
\description{
  Computes sample size for paired Student's t-test with equal group size
}
\usage{
 n.paired.t.test(power = 0.8, alpha = 0.95, mean.diff = 0.8, sd.est = 0.83)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{power}{required power 1-beta}
  \item{alpha}{required Level I-error 1-alpha}
  \item{mean.diff}{required minimum difference between group means}
  \item{sd.est}{standard deviation}
%\details{
%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{sample size}{sample size in each group}
}
\references{Bock J., Bestimmung des Stichprobenumfangs fuer biologische Experimente und kontrollierte klinische Studien. Oldenbourg 1998}
\author{Ralph Scherer}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{n.inep.t.test.eq(), n.inep.t.test.neq(), n.welch.test()}
\examples{
 n.paired.t.test(power = 0.8, alpha = 0.95, mean.diff = 0.8, sd.est = 0.83)
}
\keyword{htest}
