\name{VerifTable}
\alias{VerifTable}
\docType{data}
\title{
a matrix
}
\description{
A table in which numeric sequences generated for arguments \code{x1=10},
\code{x2=20}, \code{constantVal=14}, \code{seqlen=6} are stored.
This table is used by \code{AddRule} functions to check the uniqueness of
rules specyfied by user. By default this table contains a single vector \code{NA}
of length 6.

}
\usage{data(VerifTable)}
\format{
  
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{
\code{\link{AddRule}},
\code{\link{automaticTest}},
\code{\link{MyRules}}

}

\examples{

VerifTable
#      [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]   NA   NA   NA   NA   NA   NA


MyRules<-list(NA)
# [[1]]
# [1] NA


AddRule(new("AddConstSingleRule", constantVal=7)) #"Your rule succesfully added tp 'MyRule' list"


VerifTable
#      [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]   10   17   24   31   38   45
#[2,]   NA   NA   NA   NA   NA   NA


MyRules
#[[1]]
# [1] NA
#
#[[2]]
#
#name: AddConstSingleRule, constant value:  7


AddRule(new("AddConstSingleRule", constantVal=7))
#Error in AddRule(new("AddConstSingleRule", constantVal = 7)) :There already is a rule that gives the same result.

}
\keyword{datasets}
