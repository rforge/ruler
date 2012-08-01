\name{calculateSpecific}
\alias{calculateSpecific}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculateSpecific
}
\description{
Every rule is an object of separate class, which inherits either from
\code{SingleRule} or \code{DoubleRule} virtual class. This is why for every rule there is defined
 \code{calculateSpecific} method specyfying how to transform arguments supplied to 
function \code{calculate}. }

\usage{
calculateSpecific(x, y, z = NULL)
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
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#setClass("ExpDoubleRule", contains="DoubleRule",S3methods=TRUE)
#
#setMethod("calculateSpecific", 
#          signature(x="ExpDoubleRule", y="numeric", z="numeric"),
#          function(x,y,z){
#            return(y^z)
#          })



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
