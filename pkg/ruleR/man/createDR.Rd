\name{createDR}
\alias{createDR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to create objects of class \code{Doublerule}
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
(first rule) an object of class \code{SingleRule}. This rule will be applied to the first element of numeric sequence. If not specyfied it will be generated automatically. To specify no \code{firstRule} argument in object of class \code{DoubleRule}, \code{fr} must be defined as identical function \code{IdenSingleRule} (\code{fr=createSR("IdenSingleRule",n=0)}). For details see example [3].
}
  \item{sr}{
(second rule) an object of class \code{SingleRule}. This rule will be applied to the second argument of numeric sequence. If not specyfied it will be generated automatically. To specify no \code{secondRule} argument in object of class \code{DoubleRule}, \code{sr} must be defined as identical function \code{IdenSingleRule} (\code{sr=createSR("IdenSingleRule",n=0)}). For details see example [3].
}
  \item{ns}{
(next single rule) an object of class \code{SingleRule}. This rule will be applied to the second argument of numeric sequence. If not specyfied it will be generated automatically. To specify no \code{nextSingle} argument in object of class \code{DoubleRule}, \code{ns} must be defined as identical function \code{IdenSingleRule} (\code{sr=createSR("IdenSingleRule",n=0)}). For details see example [3].
}
}
\details{
Parameters generated automatically have different probabilities assigned. 
Because too complicated rules are very difficult to solve, objects of class \code{IdenSingleRule} 
are generated with greater probability than other.
}
\value{
A function returns an object that inherits from class \code{DoubleRule}. 
Returned object will be of class corresponding to applied \code{DoubleRule}.

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
\code{\link{createSR}},
\code{\link{calculate}}
}
\examples{

##[1] DoubleRule object with random combination of rules

m<-createDR()
print(m)


##[2]DoubleRule object with only one argument specyfied

m<-createDR(sr=new("AddConstSingleRule",constantVal=3,previousRule=new("IdenSingleRule")))
#only secondRule argument is specyfied, all other arguments are generated automatically
print(m)


##[3]simple 'DoubleRule' object created manually (no nested rules)

b<-createSR(a1=1,n=0)#creating SingleRule that is identical function
# setting firstRule, secondRule and nextSingle are objects of class 'IdenSingleRule'
m<-createDR(a=1,fr=b,sr=b,ns=b) 
#altrnatively 'm<-createDR(a=1,fr=new("IdenSingleRule"),sr=new("IdenSingleRule"),ns=new("IdenSingleRule"))'
print(m)


##[4] Nesting many rules

b<-createSR(a1=2,cv1=1,n=2,2,14,3,6)
print(b) #two nested rules inside a 'SingleRule' b object 

#An object of class "AddConstSingleRule"
#Slot "constantVal":
#[1] 1

#Slot "previousRule":

#name: AddConstSingleRule, constant value:  14
#name: MultConstSingleRule, constant value:  6
#name: IdenSingleRule
#Slot ".S3Class":
#[1] "AddConstSingleRule" "SingleRule"


c<-createSR(a1=2,cv=45, n=3) # I want to nest three SingleRules 
#(no parameters for nested rules are specyfied - program generates them )
print(c)

#name: AddConstSingleRule, constant value:  45
#name: SubsConstSingleRule, constant value:  10 
# first nested rule (generated automatically - it will be different for every generation)
#name: AddConstSingleRule, constant value:  10 
# second nested rule (generated automatically- it will be different for every generation)
#name: MultConstSingleRule, constant value:  58 
# third nested rule (generated automatically- it will be different for every generation)


m<-createDR(a=1,fr=b,ns=c) #combining DoubleRule with SingleRules
print(m)

#AddDoubleRule
# FIRST RULE:
#name: DigSumSingleRule
#name: AddConstSingleRule, constant value:  14
#name: MultConstSingleRule, constant value:  6
# SECOND RULE:
#name: IdenSingleRule
# NEXT SINGLE:
#name: AddConstSingleRule, constant value:  45
#name: SubsConstSingleRule, constant value:  10
#name: AddConstSingleRule, constant value:  10
#name: MultConstSingleRule, constant value:  58


##note that UNLIMITED number of rules can be nested  !!! 





}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
