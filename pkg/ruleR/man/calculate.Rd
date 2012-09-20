\name{calculate}
\alias{calculate}

\title{A function to calculate a rule on given object
}
\description{
A function to calculate a rule on given element
}
\usage{
calculate(x, y, z = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
- an object of class \code{SingleRule} or \code{DoubleRule}
}
  \item{y}{
- first element of numeric sequence
}
  \item{z}{
- second element of numeric sequence 
}
}
\details{
argument \code{z} is ignored if \code{x} inherits from class \code{SingleRule}
}
\value{
function returs single value of class \code{numeric}
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
\code{\link{calculateSpecific}}
}
\examples{

#[1] for objects of class SingleRule

# a rule adding a constant value '6' to  the element of numeric sequence
p<-new("AddConstSingleRule", constantVal=6, previousRule=new("IdenSingleRule")) 

calculate(p,4)# 4+6=10



#[2] for combined objects

m<-new("DigSumSingleRule",previousRule=new("IdenSingleRule"))
r<-new("AddConstSingleRule", constantVal=6,previousRule=new("IdenSingleRule"))
g<-new("MultDoubleRule", firstRule=m, secondRule=m, nextSingle=r)

calculate(g,12,34) #27

# rule 'm' (first rule) is executed on the first element of numeric sequence (1+2=3)
# rule 'm' (secodn rule) is executed on the second element of numeric sequence (3+4=7)
# rule 'g' is executed on the result of firstRule and secondRule (3*7=21)
# rule 'r' (nextSingle) is executed on the result of DoubleRule (21+6=27)
#[(1+2) * (3+4) ]+6 = 27



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
