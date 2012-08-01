\name{createDR}
\alias{createDR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

}
\description{
A function to generate objects of class DoubleRule.
}
\usage{
createDR(a = NULL, fr = NULL, sr = NULL, ns = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
an index of \code{DoubleRule} stored on  \code{doubleRules} list to be used while creating new object. If not specyfied it will be generated automatically. 
}
  \item{fr}{
(first rule) an object of class \code{SingleRule}. This rule will be applied to the first element of numeric sequence. If not specyfied it will be generated automatically. \code{fr=1} means that no additional rule is to be executed on the first argument of numeric sequence.
}
  \item{sr}{
(second rule) an object of class \code{SingleRule}. This rule will be applied to the second argument of numeric sequence. If not specyfied it will be generated automatically. \code{sr=1} means that no additional rule is to be executed on the first element of numeric sequence.
}
  \item{ns}{
(next single rule) an object of class \code{SingleRule}. This rule will be applied to the second argument of numeric sequence. If not specyfied it will be generated automatically. \code{ns=0} means that no additional rule is to be executed on the result of \code{DoubleRule}.
}
}
\details{
Parameters generated automatically have different probabilities assigned. 
Because too complicated rules are very difficult to solve, objects of class \code{IdenSingleRule} 
are generated with greater probability than other.
}
\value{
A function returns an object that inherits from class \code{DoubleRule}. 
Returned object will be of class corresponding to applied \code{DoubleRule}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{SingleRule}},
\code{\linkS4class{DoubleRule}},
\code{\link{createSR()}}
}
\examples{

##[1]random combination

m<-createDR()
print(m)



##[2]specifying only one argument

m<-createDR(sr=new("NegativeSingleRule"))
#only secondRule is specyfied
print(m)


##[3]simple 'DoubleRule' object created manually 

b<-createSR(a1=1,n=0)#creating SingleRule that is identical function
# setting firstRule, secondRule and nextSingle are objects of class 'IdenSingleRule'
m<-createDR(a=1,fr=b,sr=b,ns=b) 
print(m)


##[4] Nesting many rules

b<-createSR(a1=5,n=2,2,14,3,6)
print(b) #two nested rules inside a 'SingleRule' b object 

c<-createSR(a1=2,cv=45, n=3,)
m<-createDR(a=1,fr=b,ns=c)

##note that UNLIMITED number of rules can be nested  !!! 





}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
