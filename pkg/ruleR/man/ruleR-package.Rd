\name{ruleR-package}
\alias{ruleR-package}
\alias{ruleR}
\docType{package}
\title{
ruleR: An R package for rule based item generation
}
\description{
The package generates numeric sequences items for intelligence tests.
Items can be generated  using either the rules specified by the user or random combination of 
implemented rules.\cr\cr
Parameters of the items such as:\cr
- starting value(s),\cr 
- maximum and minimum allowed elements,\cr
- rules allowed to create items,\cr
- number of nesting allowed (the length of chain of rules),\cr
- length of numeric sequences\cr
can be specified by user. 

\code{ruleR} 




}
\details{
\tabular{ll}{
Package: \tab ruleR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-07-31\cr
License: \tab GPL-2\cr
Depends: \tab methods\cr
}
%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Maria Rafalak, Philipp Doebler\cr
Maintainer: Maria Rafalak <m.rafalak@practest.com.pl>
}
\references{
%~~ Literature or other references for background information ~~
}

\keyword{ package }
\seealso{
\code{\link{createTest}}
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
##[1] CREATING SIMPLE SINGLE OR DOUBLE RULES

# single rules (operating on the previous element of the number sequence)
ruleS1<-createSR(a1=2,cv1=14,n=0) # or: ruleS1<-new("AddConstSingleRule",constantVal=14)
print(ruleS1)
ruleS2<-createSR(a1=3,cv=0,n=0) #or: ruleS2<-new("DigSumSingleRule")
print(ruleS2)
s<-createSR(a1=1,cv=0,n=0) #or: s<-new("IdenSingleRule")
print(s)

# double rules (operating on the two previous elements of the number sequence)
ruleD1<-createDR(a=2, fr=s,sr=s,ns=s) #or: ruleD1<-new("MultDoubleRule")
print(ruleD1)
ruleD2<-createDR(a=1,fr=s,sr=s,ns=s) #or: ruleD2<-new("AddDoubleRule")
print(ruleD2)

##[2] NESTING SEVERAL RULES

combine1<-createSR(a1=2,cv1=14,n=2,3,2,2,0)
print(combine1)

combine2<-createDR(a=1,fr=combine1,sr=s,ns=ruleS1)
print(combine2)

##[3] CALCULATING WITH THE RULES

calculate(ruleS1,2) # 2+14=16
calculate(ruleS1,2,7)#2+14=16 // second argument is ignored because ruleS1 is a SingleRule

calculate(ruleS2,67)#6+7=13
calculate(ruleS2,67,45)#6+7=13 // second argument is ignored because ruleS2 is a SingleRule

calculate(ruleD1,4,17)#4*17=68
calculate(ruleD1,4) #4 //just returns the first argument

calculate(ruleD2,5,17)#5+17=22
calculate(ruleD1,5) #5 //just returns the first argument

calculate(combine1,12)# (1+2)*2+14=20

calculate(combine2,15,1) #41 
# operations on first element: (1+5)*2+14=26
# operations on second element: 1
# DoubleRule calculated on the results: 26+1=27
# next SingleRule calculated on the reult: 27+14=41


##[4] CREATING A TEST CONSISTING OF 10 ITEMS
m<-createTest(10)#all parameters default

#printing rules used to create an item
for(i in 1:length(m[[2]])){
    cat(paste("\n\n--------------","rule for item number",i,"-------------\n"))
    print.Rule(m[[2]][[i]])
                          }
## SEE \code{createTest} function for more detail

}