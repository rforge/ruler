\name{ruleR-package}
\alias{ruleR-package}
\alias{ruleR}
\docType{package}
\title{
an R package for rule based item generation
}
\description{
The package generates two types of items for intelligence tests: numeric sequences and Raven type matrix items.
Items can be generated  using either only the rules specyfied by user or random combination of rules.
During the process of creating numeric test uniqueness of presented sequences is being checked. Program prevents also from
generating constant sequences or sequences containing too small (>-1000) or to big elements (>1000). 
Program generates also some noise answers in case multiple answer format is required in a test.


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
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Maria Rafalak, Philipp Doebler
Maintainer: Maria Rafalak <m.rafalak@practest.com.pl>
}
\references{
~~ Literature or other references for background information ~~
}

\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
##[1] CREATING SIMPLE SINGLE OR DOUBLE RULE

#single rules (operating on one argument of numeric sequence)
ruleS1<-createSR(a1=2,cv1=14,n=0) # or: ruleS1<-new("AddConstSingleRule",constantVal=14)
print(ruleS1)
ruleS2<-createSR(a1=5,cv=0,n=0) #or: ruleS2<-new("DigSumSingleRule")
print(ruleS2)
s<-create(a1=1,cv=0,n=0) #or: s<-new("IdenSingleRule")
print(s)

# double rules (operating on two arguments of numeric sequence)
ruleD1<-createDR(a=2, fr=s,sr=s,ns=s) #or: ruleD1<-new("MultDoubleRule")
print(ruleD1)
ruleD2<-createDR(a=1,fr=s,sr=s,ns=s) #or: ruleD2<-new("AddDoubleRule")
print(ruleD2)

##[2]NESTING SEVERAL RULES

combine1<-createSR(a1=2,cv1=14,n=2,3,2,5,0)
print(combine1)


combine2<-createDR(a=1,fr=combine1,sr=s,ns=ruleS1)
print(combine2)


##[3] CALCULATING THE RULES

calculate(ruleS1,2) # 2+14=16
calculate(ruleS1,2,7)#2+14=16 // second argument is ignored because ruleS1 is a SingleRule

calculate(ruleS2,67)#6+7=13
calculate(ruleS2,67,45)#6+7=13 // second argument is ignored because ruleS2 is a SingleRule

calculate(ruleD1,4,17)#4*17=68
calculate(ruleD1,4) #4 //just returning the first argument

calculate(ruleD2,5,17)#5+17=22
calculate(ruleD1,5) #5 //just returning the first argument

calculate(combine1,12)# (1+2)*2+14=20

calculate(combine2,15,1) #41 
# operations on first element: (1+5)*2+14=26
# operations on second element: 1
# DoubleRule calculated on the results: 26+1=27
# next SingleRule calculated on the reult: 27+14=41


##[4] GENERATING TEST 

##a test generated only using rules defined by user consisting of 10 items of length 8

AddRule(rulesS1)#"Your rule succesfully added tp 'MyRule' list"
AddRule(ruleS2)#"Your rule succesfully added tp 'MyRule' list"
AddRule(ruleD1)#"Your rule succesfully added tp 'MyRule' list"
AddRule(ruleD2)#"Your rule succesfully added tp 'MyRule' list"
AddRule(combine1)#"Your rule succesfully added tp 'MyRule' list"
AddRule(combine2)#"Your rule succesfully added tp 'MyRule' list"
AddRule(combine2) #Error in AddRule(combine2):There already is a rule that gives the same result.

m<-automaticTest(testlen=10,type=2,seqlen=8) 
# this is a sample generation, so every call of automaticTest will give different results
m$items # matrix of items 
m$noise # sample noise answers
m$rules #only rules specyfied by user are used to generate items


## a test consisting of 10 items of length 8 generated  with random combination of rules
r<-automaticTest(testlen=10,type=1,seqlen=8)
r$items # matrix of items 
r$noise # sample noise answers
r$rules

}
