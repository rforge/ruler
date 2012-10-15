\name{createTest}
\alias{createTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to create a test with specified parameters.
}
\description{
A function to a create test with specified parameters.
}
\usage{
createTest(n, dictionary = basicDictionary, start = 1:100, chain = 2, del = c(1, 2, 3), 
 seqlen = 6,exact = NULL, element_range = c(-1000, 1000))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
how many numeric sequences should be generated? (how long the test should be)
}
  \item{dictionary}{
a list of all rules that can be used in generating a test. Every element of this list should be an object of class \code{DictionaryRule}. \cr
By default dictionary is a \code{basicDictionary} list but such list can be easily created by user. 
}
  \item{start}{
a vector specifying the allowed range for starting values. Default range is 1:100
}
  \item{chain}{
How many rules can be combined together (nested) at one time. By default \code{chain=2}
}
  \item{del}{
refers only to \code{DoubleRules}. Specifies which elements (1-firstRule,2-secondRule,\cr 3-nextSingle) should be generated. 
By default all three arguments are generated.
}
  \item{seqlen}{
how long should each numeric sequence be? \cr If it is a \code{numeric} every numeric sequence in a test will have the same length.
}
  \item{exact}{
which rules from \code{dictionary} should be used to create certain items? You can define it for every item in a form of list.
By default it is all rules from defined \code{dictionary}.
}
  \item{element_range}{
what maximum and minimum size of elements are allowed in the numeric sequence? It should be a vector of two numeric elements - first one specifying minimal values, second maximum values. If you want to define it separately for every numeric sequence it must be a list of two-element vectors.
By default elements larger than 1000 and smaller than -100 are not allowed in numeric sequences.
}
}
\details{
If \code{createTest} is unable to generate numeric sequences which are not constant or unique with the parameters given, it will return a list of NA's
instead of numeric sequence and \code{IdenSingleRule} as a rule used to produce it.
}
\value{
Function returns a list of numeric sequences \code{[[1]]} and a list of rules to create them \code{[[2]]}. 
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
 \code{\link{basicDictionary}},\code{\link{createDictRule}}, \code{\linkS4class{DoubleRule}},\code{\linkS4class{SingleRule}}
}
\examples{
#-----------------------------------------------------------
# example 1
#-----------------------------------------------------------

m<-createTest(4) #generate a Test consisting of 4 items (all settings default)
m[[1]] # a list of items (numeric sequences)
m[[2]] # a list of rules used to create those items 

#If your rule looks like this:
#name: MultConstSingleRule, constant value:  2
#name: AddConstSingleRule, constant value:  14
#name: DigSumSingleRule
#name: IdenSingleRule
#it means that you should take the sum of digits of a number, 
#add 14 and multiply the result by 2

#If your rule looks like this:

#MultDoubleRule
# FIRST RULE:
#name: DigSumSingleRule
#name: AddConstSingleRule, constant value:  -14
#name: IdenSingleRule

# SECOND RULE:
#name: MultConstSingleRule, constant value:  -12
#name: DigSumSingleRule
#name: IdenSingleRule

# NEXT SINGLE:
#name: DigSumSingleRule
#name: AddConstSingleRule, constant value:  -8
#name: IdenSingleRule

#(a) it means that you should add a constant -14 to 
#the first element of numeric sequence and take digitsum of the result
#(b) take a digitsum from the second element of numeric sequence and multiply it by -12
#that multiply (a) and (b)
# in the end add -8 to the result and take the sum of digits from your calculations


#-----------------------------------------------------------
# example 2
#-----------------------------------------------------------

m<-createTest(4,seqlen=list(4,5,6,7),start=1) 
# generating 4 sequences of length respectively  4,5,6,7, all starting with 1
 
m<-createTest(4,seqlen=list(4,5,6,7),start=7:10) 
# as above, but with starting values from range 7:10

m<-createTest(4,seqlen=list(4,5,6,7),start=list(7:10,1,15:17,4))
# first sequence should have starting elements from range 1:10, 
#second sequence should start with 1, thirs sequence should have 
#starting element from range 15:17, fourth sequence should have 
#starting element 4

m<-createTest(5,chain=0) # only one rule can be used to create every numeric sequence

m<-createTest(2,chain=list(0,1)) 
#the first sequence will be generated using only one rule, 
#the second using two rules combined (one rule nested)


#-----------------------------------------------------------
# example 3
#-----------------------------------------------------------

sss2<-createDictRule(2,range=-10:10) 
#"AddConstSingleRule" with constant value between -10 to 10

sss3<-createDictRule(3,range=2) 
#"MultConstSingleRule" with constant value 2

ddd5<-createDictRule(5) #"AddDoubleRule"
ddd6<-createDictRule(6) #"MultDoubleRule"

Mydict<-list(sss2,sss3,ddd5,ddd6) 
# I want to use only those rules in my test

m<-createTest(3,dictionary=Mydict,chain=0)
# a test using only rules from Mydict list

m<-createTest(4,chain=0,exact=list(c(1,2),c(2,3),c(3,4),c(1,4))) 
#rules 1 and 2 can be used to create first numeric sequence, rules 2 and 3 to create second numeric sequence ...
#the 'exact' list must be of length n

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
