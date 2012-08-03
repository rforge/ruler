\name{MyRules}
\alias{MyRules}
\docType{data}
\title{
list of rules specyfied by user
}
\description{
A list on which combined rules defined by user are stored. By default this list contains one \code{NA} value.
Rules can be added to this list by function \code{AddRule}.
}
\usage{data(MyRules)}
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
\code{\link{createSR}},
\code{\link{createDR}}
}


\examples{
MyRules

m<-createSR()
print(m)
AddRule(m)#"Your rule succesfully added tp 'MyRule' list"

MyRules
AddRule(m) #Error in AddRule(m) : There already is a rule that gives the same result.

k<-createDR()
print(k)
AddRule(k)#"Your rule succesfully added tp 'MyRule' list"

}
\keyword{datasets}
